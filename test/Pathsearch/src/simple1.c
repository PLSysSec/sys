int foo(int x, int y, int z) {
  if (x > y) {
    return 3;
  } else {
    if (x > 55) {
      return y*3;
    } else {
      int w = z + 53;
      return w*x;
    }
  }
  return x;
}
