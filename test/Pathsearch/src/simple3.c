int foo() {
  int y = 0;
  int z = 44;
  for (int i = 0; i < 3; i++) {
    y += z * 3;
  }
  return y;
}
