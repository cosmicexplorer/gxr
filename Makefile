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
COMPILE_LISP := $(LISP) --control-stack-size 1000 --noinform --non-interactive \
	--load ./sbcl-compile.lisp --eval "(local-compile)"

CXXFLAGS := -std=c++11 -Wall -Wextra -Werror -g -O0
LDFLAGS := -lclang

DEPS := Stack.h
AST_OBJ := walk-ast.o
LISP_OBJ := parse-sexp.fasl

AST_DRIVER := walk-ast
LISP_DRIVER := parse-sexp

all: $(AST_DRIVER) $(LISP_DRIVER)

walk-ast.o: walk-ast.cpp $(DEPS)
	$(CXX) -c $< $(CXXFLAGS)

%.fasl: %.lisp
	$(COMPILE_LISP) -f $<

$(AST_DRIVER): $(AST_OBJ)
	$(CXX) $(AST_OBJ) -o $@ $(LDFLAGS)

$(LISP_DRIVER): $(LISP_OBJ)
	$(COMPILE_LISP) -c $<

clean:
	@rm -f $(AST_DRIVER)
	@rm -f $(LISP_DRIVER)
	@rm -f $(AST_OBJ)
	@rm -f $(LISP_OBJ)
	@rm -f $(wildcard *.fasl) # pick up random compilations

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
TEST_C_OBJ := $(TEST_DIR)/outfile-c
TEST_CXX := $(TEST_DIR)/hello.cpp
TEST_CXX_OBJ := $(TEST_DIR)/outfile-cpp

check: check-c check-cpp

$(TEST_C_OBJ): $(TEST_C) all
	./$(AST_DRIVER) $< 1 -I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) - $(TEST_C_OBJ)
check-c: $(TEST_C_OBJ)

# this test is still broken due to random segfaults
# i think it's just because c++ is a bigger language and libclang is an iffy
# library
$(TEST_CXX_OBJ): $(TEST_CXX) all
	./$(AST_DRIVER) $< 1 -std=c++14 -I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) - $(TEST_CXX_OBJ)
check-cpp: $(TEST_CXX_OBJ)
