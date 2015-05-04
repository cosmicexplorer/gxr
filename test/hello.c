#include <stdio.h>

#define A 3

int b;
int a;

int _a() {
  int _b;
  _b = A;
  return _b;
}

int main();

int main() {
  int * aptr = NULL;
  aptr = &a;
  *aptr = 2;
  printf("%d\n", a);
}

int c = 3;
