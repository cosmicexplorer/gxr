#include <stdio.h>

int b;
int a;

int _a() {
  int _b;
  _b = 3;
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
