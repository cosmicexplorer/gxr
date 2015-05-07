#include <iostream>

int b = 3;

int a = 3;

int main(int argc, char ** argv) {
  int * aptr = nullptr;
  aptr = &a;
  *aptr = 2;
  std::cout << "hello world!\n\\" << a << std::endl;
}
