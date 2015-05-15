#include <iostream>

#include "hello.hpp"

#define B_VAL 3

extern int b;

int b = B_VAL;

int a = 3;

using namespace fff;

int main(int argc, char ** argv) {
  int * aptr = nullptr;
  aptr = &fff::a;
  *aptr = 2;
  std::cout << "hello world!\n\\" << fff::a << std::endl;
}
