int alloc4();

int main() {
  int *p;
  alloc4(&p,1,2,4,8);

  int i;
  int sum;
  sum = 0;
  for(i=0;i<4;i+=1){
    sum += *p;
    p+=1;
  }
  return sum;
}
