                   int putchar();void*
               memset();int m(int a,int b)
            {return (a*b+5000)/10000;}void a(
          int*c,int*s,int d,int t){int k;k=m(*c,d
        )-m(*s,t);int l;l=m(*s,d)+m(*c,t);*c=k;*s=l
      ;}int usleep();int printf();int main(){int z[
     1760];char b[1760];printf("\e[2J");int s;s=10000;
    int q;q=s;int r;r=0;int u;u=s;int v;v=0;for(;;a(&q,&r,s-8
   ,400),a(&u,&v,s-2,200)){memset(b,32,1760);memset(z,
  0,7040);int l;l=0;int p;p           =s;int i;for(i=0;i<88;i
  ++,a(&p,&l,9974+i%                 2,714)){int w;w=0;int
 e;e=s;int j;for(j=0;j<                   314;j++,a(&e,&w,s-
 2,200)){int f;f=p+2                     *s;int g;g=s*s/(m(m
(w,f),r)+m(l,q)+5*                     s);int t;t=m(m(w,q),
f)-m(l,r);int x;x=40                     +30*m(g,m(m(e,u),f
 )-m(t,v))/s;int y;y                     =12+15*m(g,m(m(e, 
 v),f)+m(t,u))/s;int                   o;o=x+80*y;int N;N=8* 
  (m(m(l,r)-m(m(w,q)                ,p),u)-m(m(w,r),p)-  
  m(l,q)-m(m(e,v),p))/s           ;if(y>0&&g>z[o]&&y<22  
   &&x>0&&80>x){z[o]=g;b[o]=".,-~:;=!*#$@"[N>=1?N:0]+0
    ;}}}printf("\e[H");int k;for(k=0;k<1761;k++)putchar
     (k%80?b[k]:10);printf("Author: @a1k0n. Rewritt"
      "en by @hsjoihs so that it works without flo"
        );https://github.com/hsjoihs/c-compiler/
          printf("ating types.\nNote that roun"
            "ding errors gradually reduce th"
               "e donut's size.\n");usleep
                   (50000);}return 0;}
