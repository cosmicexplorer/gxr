.PHONY: all clean check-c check-cpp check

CXX := $(shell if hash clang++ 2>/dev/null; then echo clang++; else \
	if hash g++ 2>/dev/null; then echo g++; else echo oops; fi; fi)
ifeq ($(CXX), oops)
$(error no suitable c++ compiler found! install g++ or clang++)
endif

LISP_CC := sbcl
LISP := $(shell if hash $(LISP_CC) 2>/dev/null; then echo $(LISP_CC); else \
	echo oops; fi)
ifeq ($(LISP), oops)
$(error no suitable lisp compiler found! install $(LISP_CC))
endif

# takes a path to $(LISP) and loads sbcl-compile.lisp which has the top-level
# form (local-compile). the allowable succeeding arguments are detailed in
# sbcl-compile.sh
COMPILE_LISP := $(LISP) --noinform --non-interactive \
	--load ./sbcl-compile.lisp --eval "(local-compile)"

CXXFLAGS := -std=c++14 -Wall -Wextra -Werror -g -O0
LDFLAGS := -lclang

DRIVERS := walk-ast.so parse-sexp ast-visit

all: $(DRIVERS)

%.fasl: %.lisp
	$(COMPILE_LISP) -f $<

%: %.fasl
	$(COMPILE_LISP) -c $<

%.so: %.cpp
	$(CXX) $< -o $@ $(CXXFLAGS) $(LDFLAGS) -shared -fPIC

clean:
	@rm -f $(DRIVERS)
	@rm -f $(wildcard *.fasl) # pick up random compilations

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
TEST_C_OBJ := $(TEST_DIR)/outfile-c
TEST_CXX := $(TEST_DIR)/hello.cpp
TEST_CXX_OBJ := $(TEST_DIR)/outfile-cpp

check: check-c check-cpp

$(TEST_C_OBJ): $(TEST_C) all
	./$(LISP_DRIVER) $(TEST_C) $(TEST_C_OBJ) 1 \
	-I/usr/lib/clang/3.6.0/include
check-c: $(TEST_C_OBJ)

$(TEST_CXX_OBJ): $(TEST_CXX) all
	./$(LISP_DRIVER) $(TEST_CXX) $(TEST_CXX_OBJ) 1 \
	-I/usr/lib/clang/3.6.0/include
check-cpp: $(TEST_CXX_OBJ)
