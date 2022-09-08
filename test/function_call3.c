int ten();
int add(int a,int b);

int main() {
  int a;
  int b;
  int c;
  a = ten();
  b = a-5;

  c = add(a+3,b+ten());

  return c;
}

