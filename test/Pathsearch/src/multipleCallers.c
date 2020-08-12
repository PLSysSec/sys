int foo(int x, int y, int z) {
  if (x < y) {
    return x + z;
  } else {
    return x - z;
  }
}

int caller1(int c) {
  if (c > 10) {
    return foo(c, c % 17, 2);
  } else {
    return foo(c, 10 - c, 3);
  }
}

int caller2(int c) {
  if (c > 12) {
    return foo(c, c & 0xface, c/2);
  } else {
    return foo(c, 12 - c, c*3);
  }
}

int callercaller(int c, int d) {
  if (caller1(c) > 5) {
    return caller2(d) + 3;
  } else {
    return caller2(d) * 5;
  }
}
