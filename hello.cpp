#include <iostream>

int a = 3;

int main() {
  int * aptr = nullptr;
  aptr = &a;
  *aptr = 2;
  std::cout << "hello world!" << a << std::endl;
}
