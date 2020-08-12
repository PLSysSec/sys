int simple(int x) {
  int y = x * 2;
  if (y > 25) return y;
  return simple(y);
}

int moreBlocks(int x) {
  int y = x + 2;
  int z;
  if (y % 3 == 0) z = 1;
  else if (y % 3 == 1) z = 5;
  else z = 11;
  for(int f = 0; f < 2; f++) {
    y++;
  }
  if (y * z > 242) return y/z;
  else return moreBlocks(y);
}

int notTailRec(int x) {
  if (x > 7) return x + 10;
  int b = notTailRec(x - 2);
  if (b > 0) return b * 2;
  else return b + 5;
}

int mutRecursiveB(int);
int mutRecursiveA(int x) {
  if(x > 100) return x;
  return mutRecursiveB(x + 11);
}

int mutRecursiveB(int x) {
  if(x < 66) return x;
  return mutRecursiveA(x / 2);
}

int threeWayMutRecursiveB(int);
int threeWayMutRecursiveC(int);
int threeWayMutRecursiveA(int x) {
  if(x % 5 == 0) return x;
  return threeWayMutRecursiveB(x + 1);
}

int threeWayMutRecursiveB(int x) {
  if(x > 124) return x - 2;
  return threeWayMutRecursiveC(x + 4);
}

int threeWayMutRecursiveC(int x) {
  return threeWayMutRecursiveA(x + 2);
}

int __attribute__((noinline)) notRecursive(int x, int y) {
  return x + y * 3;
}

int nestedRecursive(int x) {
  int y = notRecursive(3, x);
  if(y > 100) return y;
  return nestedRecursive(y);
}

int doubleRecursive(int x, int y) {
  if(x - y > 100) return x + y;
  int a = 2 * doubleRecursive(x, y - 1);
  int b = 3 + doubleRecursive(a, x);
  return a - b;
}
