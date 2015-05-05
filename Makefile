.PHONY: all clean check-c check-cpp check

CXX := clang++
LISP := sbcl

# takes a path to sbcl $(LISP) and loads sbcl-compile.lisp which has the
# top-level form (local-compile). the allowable succeeding arguments are
# detailed in sbcl-compile.sh
COMPILE_LISP := $(LISP) --noinform --non-interactive \
	--load ./sbcl-compile.lisp --eval "(local-compile)" \
	--quit

COMPP_DIR := compp
COMPP_MAIN := $(COMPP_DIR)/lib/compp

CXXFLAGS := -std=c++11 -Wall -Wextra -Werror -g -O0
LFLAGS := -lclang

DEPS := Stack.h
OBJ := walk-ast.o
LISP_OBJ := parse-sexp.fasl

DRIVER := walk-ast
LISP_DRIVER := parse-sexp

all: $(DRIVER) $(LISP_DRIVER) $(COFFEE_DRIVER)

%.o: %.cpp $(DEPS)
	$(CXX) -c $< $(CXXFLAGS)

%.fasl: %.lisp
	$(COMPILE_LISP) -f $<

$(DRIVER): $(OBJ) $(LISP_OBJ)
	$(CXX) $(OBJ) -o $@ $(LFLAGS)

${LISP_DRIVER}: $(LISP_OBJ)
	$(COMPILE_LISP) -c $<

clean:
	@rm -f $(DRIVER)
	@rm -f $(LISP_DRIVER)
	@rm -f $(OBJ)
	@rm -f $(LISP_OBJ)
	@rm -f $(wildcard *.fasl) # pick up random compilations

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
TEST_C_OBJ := $(TEST_DIR)/outfile-c
TEST_CXX := $(TEST_DIR)/hello.cpp
TEST_CXX_OBJ := $(TEST_DIR)/outfile-cpp

check: check-c check-cpp

$(TEST_C_OBJ): $(TEST_C) all
	./$(DRIVER) $< -x c -I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) /dev/null - 2>&1 | tee $(TEST_C_OBJ) | head -n20
check-c: $(TEST_C_OBJ)

# this test is still broken due to random segfaults
# i thinks it's just because c++ is a bigger language and libclang is an iffy
# library
$(TEST_CXX_OBJ): $(TEST_CXX) all
	./$(DRIVER) $< -x c++ -std=c++11 -I/usr/lib/clang/3.6.0/include | \
	./$(LISP_DRIVER) /dev/null - 2>&1 | tee $(TEST_CXX_OBJ) | head -n20
check-cpp: $(TEST_CXX_OBJ)
