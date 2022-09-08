int main() {
  int x[3];
  int *p;
  x[0] = 10;
  x[1] = 20;

  p = x+1;
  p--;
  return *p;
}
