# DEFAULTS TO DEBUG CONFIGURATION
.PHONY: all debug release clean check-c check-cpp check clean-check check-helper

# note: if you run make with multiple targets at once, the compilation flags
# will not be set correctly. this is because it is confusing and not wholely
# necessary to specify separate makefiles to take care of the uncommon case of
# multiple targets. if you do like using multiple targets, sorry. make an issue
# and i'll fix it.

CLANG_CXX := clang++
GCC_CXX := g++
CXX := $(shell if hash $(CLANG_CXX) 2>/dev/null; then echo $(CLANG_CXX); else \
	if hash $(GCC_CXX) 2>/dev/null; then echo $(GCC_CXX); else echo oops; \
	fi; fi)
ifeq ($(CXX), oops)
$(error no suitable c++ compiler found! install $(CLANG_CXX) or $(GCC_CXX))
endif

CXXFLAGS := -std=c++14 -Wall -Wextra -Werror -g -O0 -DDEBUG
LDFLAGS := -lclang

SRC_DIR := src
OBJ_DIR := obj

DEPS := $(wildcard $(SRC_DIR)/*.hpp)
AST_SRC := $(wildcard $(SRC_DIR)/*.cpp)
AST_OBJ := $(patsubst $(SRC_DIR)/%.cpp, $(OBJ_DIR)/%.o, $(AST_SRC))

AST_DRIVER := walk-ast

# release builds gotta go fast
ifeq ($(MAKECMDGOALS),release)
CXXFLAGS := -std=c++14 -Ofast -DRELEASE
endif


all: $(AST_DRIVER)
debug: all
release: all

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
check: check-helper

check-helper: clean-check
	@make check-c
# check: check-c check-cpp

DIAG_FILE := $(TEST_DIR)/diagnostics
$(TEST_C_OBJ): $(TEST_C) all
	time ./$(AST_DRIVER) $< > $@ 2>>$(DIAG_FILE)
check-c: $(TEST_C_OBJ)
	@echo "$(TEST_C_OBJ) has" $(shell wc -l "$(TEST_C_OBJ)" | \
		grep -Po "^[0-9]+") "lines!"

# $(TEST_CXX_OBJ): $(TEST_CXX) all
# 	time ./$(AST_DRIVER) $< > $@ 2>>$(DIAG_FILE)
# check-cpp: $(TEST_CXX_OBJ)

clean-check:
	@rm -f $(DIAG_FILE)
	@rm -f $(TEST_C_OBJ)
	@rm -f $(TEST_CXX_OBJ)

clean: clean-check
	@rm -f $(AST_DRIVER)
	@rm -f $(AST_OBJ)
