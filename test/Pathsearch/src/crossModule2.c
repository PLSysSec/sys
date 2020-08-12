extern int simpleCrossModuleCaller(int a, int b);
extern int foo(int x, int y, int z);

int simple(int x, int y) {
  return simpleCrossModuleCaller(x + 3, y * 2) - 150;
}

int anotherFooCaller(int x, int y, int z) {
  return foo(x/2, y*2, z%2) * 30;
}
