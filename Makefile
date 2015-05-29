.PHONY: all clean check-c check-cpp check

CLANG_CXX := clang++
GCC_CXX := g++
CXX := $(shell if hash $(CLANG_CXX) 2>/dev/null; then echo $(CLANG_CXX); else \
	if hash $(GCC_CXX) 2>/dev/null; then echo $(GCC_CXX); else echo oops; \
	fi; fi)
ifeq ($(CXX), oops)
$(error no suitable c++ compiler found! install $(CLANG_CXX) or $(GCC_CXX))
endif

CXXFLAGS := -std=c++14 -Wall -Wextra -Werror -g -O0
LDFLAGS := -lclang

SRC_DIR := src
OBJ_DIR := obj

DEPS := $(wildcard $(SRC_DIR)/*.hpp)
AST_SRC := $(wildcard $(SRC_DIR)/*.cpp)
AST_OBJ := $(patsubst $(SRC_DIR)/%.cpp, $(OBJ_DIR)/%.o, $(AST_SRC))

AST_DRIVER := walk-ast

all: $(AST_DRIVER)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp $(DEPS)
	$(CXX) -o $@ -c $< $(CXXFLAGS)

$(AST_DRIVER): $(AST_OBJ)
	$(CXX) $^ -o $@ $(LDFLAGS)

TEST_DIR := test
TEST_C := $(TEST_DIR)/hello.c
TEST_C_OBJ := $(TEST_DIR)/outfile-c
# TEST_CXX := $(TEST_DIR)/hello.cpp
# TEST_CXX_OBJ := $(TEST_DIR)/outfile-cpp

# just c for now; we'll add c++ support later
check: check-c
# check: check-c check-cpp

# TODO: this shouldn't need to be here
INCLUDE_ARG := -I/usr/lib/clang/3.6.0/include

$(TEST_C_OBJ): $(TEST_C) all
	./$(AST_DRIVER) $< $(INCLUDE_ARG) > $@
check-c: $(TEST_C_OBJ)

# $(TEST_CXX_OBJ): $(TEST_CXX) all
# 	./$(AST_DRIVER) $< -std=c++14 $(INCLUDE_ARG) > $@
# check-cpp: $(TEST_CXX_OBJ)

clean:
	@rm -f $(AST_DRIVER)
	@rm -f $(AST_OBJ)
	@rm -f $(TEST_C_OBJ)
#	@rm -f $(TEST_CXX_OBJ)
