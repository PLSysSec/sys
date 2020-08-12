int callee1(int x);
int callee2(int y);
int callee3(int z);

int diffCalls(int a, int b) {
  if (a < b) {
    return callee1(a) + callee2(b) - callee3(a-b);
  } else {
    return callee1(b);
  }
}

int sameCalls(int a, int b) {
  if (a > b) {
    return callee1(a) +
           callee1(b) +
           callee2(a) +
           callee1(a+b) +
           callee1(a*b);
  } else {
    return callee1(b-a);
  }
}

int callee1(int x) {
  return x / 2;
}

int callee2(int y) {
  return y % 2;
}

int callee3(int z) {
  return z * 3;
}
