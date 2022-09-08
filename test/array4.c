int fibonacci(int n){
  int fib[10];
  fib[0] = 1;
  fib[1] = 1;

  int i;
  for(i=2;i<10;i = i+1){
    fib[i] = fib[i-1]+fib[i-2];
  }

  return fib[n];
}

int main() {
  return fibonacci(8);
}

