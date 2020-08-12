int foo(int x, int y, int z) {
  switch (x) {
    case 3:
      return 3;
    case 4:
      return z;
    default:
      if (x > 55) {
        return y*3;
      }
  }
}
