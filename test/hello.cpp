#include <iostream>

#include "hello.hpp"

#define B_VAL 3

extern int b;

struct bbb;

struct bbb {
  int a;
};

typedef bbb ccc;

int b = B_VAL;

int a = 3;

using namespace fff;

int main(int argc, char ** argv) {
  ccc c;
  c.a = 3;
  int * aptr = nullptr;
  aptr = &fff::a;
  *aptr = 2;
  std::cout << "hello world!\n\\" << fff::a << std::endl;
  std::cout << c.a << std::endl;
}
