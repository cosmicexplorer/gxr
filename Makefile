.PHONY: all clean check check-debug

DEPS := Stack.h
OBJ := test.o

CXX := clang++

CXXFLAGS := -std=c++11 -Wall -Wextra -Werror -g -O0
LFLAGS := -lclang

DRIVER := test

all: $(DRIVER)

%.o: %.cpp $(DEPS)
	$(CXX) -c $< $(CXXFLAGS)

$(DRIVER): $(OBJ)
	$(CXX) $(OBJ) -o $@ $(LFLAGS)

clean:
	@rm -f $(DRIVER)
	@rm -f $(OBJ)

TEST_C := hello.c
# why is this breaking everything? not sure yet
TEST_CXX := hello.cpp

# it yells about being unable to find stddef.h and how that's a "fatal" error,
# yet it also just keeps going? that's annoying. remove this soon.
check: all
	./$(DRIVER) hello.c -I/usr/lib/clang/3.6.0/include > file
