#include <iostream>

int b = 3;

namespace fff {
int a = 3;
}

using namespace fff;

int main(int argc, char ** argv) {
  int * aptr = nullptr;
  aptr = &a;
  *aptr = 2;
  std::cout << "hello world!\n\\" << a << std::endl;
}
