#include <stdlib.h>

int alloc4(int **p,int a,int b,int c,int d){
  *p = malloc(4*sizeof(int));
  (*p)[0] = a;
  (*p)[1] = b;
  (*p)[2] = c;
  (*p)[3] = d;
  return 0;
}
