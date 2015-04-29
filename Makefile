.PHONY: all clean check check-debug

DEPS := Stack.h
OBJ := walk-ast.o
LISP_OBJ := parse-sexp.fasl

CXX := clang++
LISP := sbcl
COMPILE_LISP := ./compile-lisp.sh $(LISP) ./sbcl-compile.lisp

CXXFLAGS := -std=c++11 -Wall -Wextra -Werror -g -O0
LFLAGS := -lclang

DRIVER := walk-ast
LISP_DRIVER := parse-sexp

all: $(DRIVER) $(LISP_DRIVER)

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
	@rm -f $(wildcard *.fasl)
	@rm -f $(wildcard *.o)
	@rm -f $(TEST_OBJ)

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
# this test is still broken due to random segfaults
# i thinks it's just because c++ is a bigger language and libclang is an iffy
# library
TEST_CXX := $(TEST_DIR)/hello.cpp

TEST_OBJ := file outfile

check: all
	./$(DRIVER) $(TEST_C) -I/usr/lib/clang/3.6.0/include > file && \
	./$(LISP_DRIVER) file outfile && head outfile -n20 1>&2
