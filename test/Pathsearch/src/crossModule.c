extern int foo(int x, int y, int z);
  // can be provided by, e.g., simple1.c, or multipleCallers.c

int simpleCrossModuleCaller(int a, int b) {
  int x = foo(a, b, a+b);
  return x + 3;
}

int multipleCrossModuleCaller(int a, int b) {
  if (foo(a, b, a-b) > 11) {
    return foo(a + b, a - b, a * b);
  } else if (foo(b, a, 2) > 13) {
    return foo(a - b, a + b, a % b);
  } else {
    return foo(b + 2, a * 2, b / a);
  }
}

int crossModuleCallerCaller(int c, int d) {
  return simpleCrossModuleCaller(c, d) - multipleCrossModuleCaller(d, c);
}
