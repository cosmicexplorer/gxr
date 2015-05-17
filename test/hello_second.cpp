#include <iostream>

#define B_VAL 3

int b = B_VAL;

int a = 3;

struct bbb {
  int a;
};

typedef bbb ccc;

int main() {
  ccc c;
  c.a = 3;
  std::cout << c.a << std::endl;
}
