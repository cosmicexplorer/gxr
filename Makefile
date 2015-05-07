.PHONY: all clean check-c check-cpp check

CLANG_CXX := clang++
GCC_CXX := g++
CXX := $(shell if hash $(CLANG_CXX) 2>/dev/null; then echo $(CLANG_CXX); else \
	if hash $(GCC_CXX) 2>/dev/null; then echo $(GCC_CXX); else echo oops; \
	fi; fi)
ifeq ($(CXX), oops)
$(error no suitable compiler found! install $(CLANG_CXX) or $(GCC_CXX))
endif

LISP := sbcl

# takes a path to $(LISP) and loads sbcl-compile.lisp which has the top-level
# form (local-compile). the allowable succeeding arguments are detailed in
# sbcl-compile.sh
COMPILE_LISP := $(LISP) --noinform --non-interactive \
	--load ./sbcl-compile.lisp --eval "(local-compile)" \
	--quit

LLVM_FLAGS := $(shell llvm-config --cxxflags)
CXXFLAGS := -std=c++11 -Wall -Wextra -Werror -g -O0

# first the llvm-config, which should just work, but doesn't
# then extraneous other random libraries, which shouldn't be required, but are
LLVMLDFLAGS := $(shell llvm-config --ldflags --libs) \
	-pthread -ldl -lz -lncurses \
	-lclangFrontend -lclangParse -lclangSema -lclangAnalysis -lclangAST \
	-lclangBasic -lclangDriver -lclangSerialization -lclangEdit -lclangLex \
	-lLLVMSupport -lLLVMOption -lLLVMBitReader -lLLVMMC -lLLVMMCParser \
	-lLLVMSupport

LDFLAGS := -lclang

DEPS :=
AST_OBJ := walk-ast.o
PREPROC_OBJ := pull-preprocessor-directives.o
LISP_OBJ := parse-sexp.fasl

AST_DRIVER := walk-ast
PREPROC_DRIVER := pull-preprocessor-directives
LISP_DRIVER := parse-sexp

all: $(AST_DRIVER) $(LISP_DRIVER) $(PREPROC_DRIVER)

walk-ast.o: walk-ast.cpp $(DEPS)
	$(CXX) -c $< $(CXXFLAGS)

pull-preprocessor-directives.o: pull-preprocessor-directives.cpp $(DEPS)
	$(CXX) -c $< $(CXXFLAGS) $(LLVM_FLAGS)

%.fasl: %.lisp
	$(COMPILE_LISP) -f $<

$(AST_DRIVER): $(AST_OBJ)
	$(CXX) $(AST_OBJ) -o $@ $(LDFLAGS)

$(PREPROC_DRIVER): $(PREPROC_OBJ)
	$(CXX) $(PREPROC_OBJ) -o $@ $(LDFLAGS) $(LLVMLDFLAGS)

${LISP_DRIVER}: $(LISP_OBJ)
	$(COMPILE_LISP) -c $<

clean:
	@rm -f $(AST_DRIVER)
	@rm -f ${PREPROC_DRIVER}
	@rm -f $(LISP_DRIVER)
	@rm -f $(AST_OBJ)
	@rm -f $(PREPROC_OBJ)
	@rm -f $(LISP_OBJ)
	@rm -f $(wildcard *.fasl) # pick up random compilations

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
TEST_C_OBJ := $(TEST_DIR)/outfile-c
TEST_CXX := $(TEST_DIR)/hello.cpp
TEST_CXX_OBJ := $(TEST_DIR)/outfile-cpp

check: check-c check-cpp

$(TEST_C_OBJ): $(TEST_C) all
	./$(AST_DRIVER) $< 0 -x c -I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) /dev/null - $(TEST_C_OBJ)
check-c: $(TEST_C_OBJ)

# this test is still broken due to random segfaults
# i think it's just because c++ is a bigger language and libclang is an iffy
# library
$(TEST_CXX_OBJ): $(TEST_CXX) all
	./$(AST_DRIVER) $< 1 -x c++ -std=c++11 \
	-I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) /dev/null - $(TEST_CXX_OBJ)
check-cpp: $(TEST_CXX_OBJ)
