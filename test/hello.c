int __a() {
  return 3;
}

struct {
  int a;
} bb = {.a = 3};

#define AAAA (__a())
#define BBBB (AAAA + 3)

int ___a() {
  return BBBB;
}

static int A = 4;

#include <stdio.h>

#define A 3

#define F(x, y) x##y

int b = 3;

#include "hello.h"

void f() {
  int b = a;
}

#ifdef BBB
int _a() {
  int _b;
  _b = A;
  return _b;
}
#else
int _a();
int c = 3;
int _a() {
  int _b;
  _b = A;
  return _b;
}
#endif

extern int AAAAAA;
int AAAAAA = 3;
struct q {
  int b;
};
void _b() {
  struct q BBBBBB = {.b = AAAAAA};
}

int main();

int main() {
  int * F(a, ptr) = NULL;
  aptr = &a;
  *aptr = A;
  b = _a();
#undef A
  int c = A;
  printf("%d\n", a);
}
