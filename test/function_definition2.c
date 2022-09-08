int big_func(int a,int aa,int aaa,int aaaa,int aaaaa,int aaaaaa){
  return a+aa+aaa+aaaa+aaaaa+aaaaaa;
}

int small_func(){
  return 10;
}

int main() {
  return big_func(small_func(),1,2,3,4,5);
}
