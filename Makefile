.PHONY: all clean

CXX = clang++

CXXFLAGS := -std=c++11 -Wall -Wextra -Werror
LFLAGS := -lclang

DRIVER := test

all: $(DRIVER)

$(DRIVER): test.cpp
	$(CXX) $(CXXFLAGS) $< -o $@ $(LFLAGS)

clean:
	@rm -f $(DRIVER)
