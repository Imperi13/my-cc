#!/bin/bash

# hsjoihsさんのテストをmy-cc向けに修正
# https://github.com/hsjoihs/c-compiler/blob/master/test_cases.sh
#

cc=/usr/local/musl/bin/musl-gcc
cc_option="-std=gnu11 -g -static"

run_test_with_supplement0() {
  rm -f ./tmp ./tmp.s
  echo -e "$2" > ./external/tmp.c
  $MYCC -S ./external/tmp.c -o tmp.s
	${cc} ${cc_option} ./external/misc/supplement0.c -S -o supplement0.s
	${cc} ${cc_option} tmp.s supplement0.s -o tmp
	./tmp
	res=$?
	if [ $res -ne $3 ]; then { echo "got:" $res; echo "expected:" $3; echo -e "\033[31mFAIL\033[m, at test case" $1: "$2"; exit 1; }; else echo -e "\033[32mPASS\033[m"; fi
}

run_test() {
  rm -f ./tmp ./tmp.s
  echo -e "$2" > ./external/tmp.c
	$MYCC -S ./external/tmp.c -o tmp.s
  ${cc} ${cc_option} -o tmp tmp.s
	./tmp
	res=$?
	if [ $res -ne $3 ]; then { echo "got:" $res; echo "expected:" $3; echo -e "\033[31mFAIL\033[m, at test case" $1: "$2"; exit 1; }; else echo -e "\033[32mPASS\033[m"; fi
}

run_test_with_supplement1() {
  rm -f ./tmp ./tmp.s
  echo -e "$2" > ./external/tmp.c
  $MYCC -S ./external/tmp.c -o tmp.s
	${cc} ${cc_option} ./external/misc/supplement1.c -S -o supplement1.s
	${cc} ${cc_option} tmp.s supplement1.s -o tmp
	./tmp
	res=$?
	if [ $res -ne $3 ]; then { echo "got:" $res; echo "expected:" $3; echo -e "\033[31mFAIL\033[m, at test case (mixed)" $1: "$2"; exit 1; }; else echo -e "\033[32mPASS (mixed)\033[m"; fi

  rm -f ./tmp ./tmp.s ./supplement1.s
  echo -e "$2" > ./external/tmp.c
  $MYCC -S ./external/tmp.c -o tmp.s
  $MYCC -S ./external/misc/supplement1.c -o supplement1.s
	${cc} ${cc_option} tmp.s supplement1.s -o tmp
	./tmp
	res=$?
	if [ $res -ne $3 ]; then { echo "got:" $res; echo "expected:" $3; echo -e "\033[31mFAIL\033[m, at test case (pure)" $1: "$2"; }; else echo -e "\033[32mPASS (pure)\033[m"; fi
}

run_test 001 'int main(){return 123;}' 123
run_test 002 'int main(){return (123);}' 123
run_test 003 'int main(){return ((((123))));}' 123
run_test 004 'int main(){return 123+51;}' 174
run_test 005 'int main(){return 123+56-5;}' 174
run_test 006 'int main(){return 175-(4-3);}' 174
run_test 007 'int main(){return 181-4-3;}' 174
run_test 008 'int main(){return 0x29*3+7*8-5*1;}' 174
run_test 009 'int main(){return 6*(3+7)-5*1;}' 55
run_test 010 'int main(){return 43,6*(3+7)-5*1;}' 55
run_test 011 'int main(){return 43,6*(3+(4|3))-(5|1)*1;}' 55
run_test 012 'int main(){return 043,41*3+07*010-0Xa/(010%(1+2));}' 174
run_test 013 'int main(){return 7*5,(12,41*3)+7*16/(9,2)-10/(8%3);}' 174
run_test 013_1 'int main(){return 173+ (1<2);}' 174
run_test 014 'int main(){return 7*5 	,	(0xC,(41   )*(4-(011>8)))+7*(((1+2)>=3)<<4)/(9,(4>>(10<=10))+(3<3))-10/(	  ( 	1  <<3)	%3);}'  174
run_test 015 'int main(){return 35,	((	41|	(8   !=     15))*  ((3==3)+2))+((5|2)*(9&10))   -   (10/(8%3));}'  174
run_test 016 'int main(){return 043,41*3+07*010-0Xa/(010%(!!1+2));}' 174
run_test 017 'int main(){return 7*5 	,	(0xC,(41   )*(4-(011>8)))+7*(((1-~1)>=3)<<4)/(9,(4>>(10<=10))+(3<3))-10/(	  ( 	!0  <<3)	%3);}' 174
run_test 018 'int main(){return +174;}' 174
run_test 019 'int main(){return -(1-175);}' 174
run_test 020 'int main(){23; 45+37; ((12-1)*75); return -(1-175);}' 174
run_test 021 'int main(){23; 45+37; return -(1-175); ((12-1)*75);}' 174
run_test 022 'int main(){int a; int b; return (a = b = 9, a = 41*3, 55 - (b = 4) + a);}' 174
run_test 023 'int main(){int a; int b; int c; int d; int _q432; a = b = c = 9; d = 5; a = 41*3; return (c, _q432 = 8, d = 11*5) - (b = 4) + a;}'  174
run_test 024 'int main(){return 175^1;}' 174
run_test 025 'int main(){return 2 + (1? 100 + 72 : 17);}' 174
run_test 026 'int main(){return (0? 234 : 2) + (1? 100 + 72 : 17);}' 174
run_test_with_supplement1 027 'int always87(); int main(){return (3, always87() + always87());}' 174
run_test_with_supplement1 028 'int always87();int always8();int main(){return always87() + ((always8()* 11) -1);}' 174
run_test_with_supplement1 029 'int add();int main(){return add(170,4);}' 174
run_test_with_supplement1 030 'int always87();int always8();int add();int main(){return always87() + ((always8()* add(4,7)) -1);}' 174
run_test_with_supplement1 031 'int always87();int always8();int subtract();int main(){return always87() + ((always8()* subtract(12,1)) -1);}' 174
run_test 032 'int main(){3; {5; 7; 11; } return 175^1;}' 174
run_test_with_supplement1 033 'int always87();int always87_(){return 87;} int main(){return (3, always87() + always87_());}' 174
run_test 034 'int add_(int x, int y){4; return x+y;} int main(){3; return add_(87,87);}' 174
run_test 035 'int fib(int n){ return n < 2? n : fib(n - 1) + fib(n - 2); } int main(){3; return fib(10);}' 55
run_test 036 'int tarai(int x,int y,int z){ return x <= y? y : tarai(tarai(x-1, y, z), tarai(y-1, z, x), tarai(z-1, x, y)); } int main(){return tarai(12,6,0);}' 12
run_test 037 'int main() { return (3 && 2 && 5) + 173; }' 174
run_test 038 'int main() { return (3 && 2) + !(3 && 0) + !(0 && 3)+ !(0 && 0) + 170; }' 174
run_test 039 'int main() { return (3 || 2 || 5) + 173; }' 174
run_test 040 'int main() { return (3 || 2) + (3 || 0) + (0 || 3)+ !(0 || 0) + 170; }' 174
run_test 041 'int main() {int a; a = 3; a += 5;  return a + 166; }' 174
run_test 042 'int main() {int a; int b; a = 3; b = (a += 5);  return a + b + 158; }' 174
run_test 043 'int main() {int a; int b; a = 3; b = 1; b *= (a += 5);  return a + b + 158; }' 174
run_test 044 'int main() {int a; int b; a = 11; a -=5; a /= 2; b = 1; b *= (a += 5);  return a + b + 158; }' 174
run_test 045 'int main() {int a; int b; int c; a = 7; a &= ~2; a <<= 2; a |=2; a >>= 1; a -=5; a /= 2; b = 3; c = 8; b ^= (c%=3); b *= (a += 5);  return a + b + 158; }' 174
run_test 046 'int foo(){ return 2;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();} else { }    return 172+b;}' 174
run_test 047 'int foo(){ return 2;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();}   return 172+b;}' 174
run_test 048 'int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();} else { c = bar();}    return 172+b;}' 174
run_test 049 'int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 0;b = 5;c = 2;if(a) {b = foo();} else { c = bar();}    return 162+b+c;}' 174
run_test 050 'int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) if(0) { b = foo(); } else {  c = bar(); }    return 162+b+c;}' 174
run_test 051 'int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) if(0)b=foo();else c = bar();return 162+b+c;}' 174
run_test 052 'int main() {int a; a = 4; if(1){return 170+a; a = 7; }else{return 170-a; a = 9;} a = 5; return a;}' 174
run_test 053 'int foo(){return 1;} int main(){int a; a=0;do{a=3;}while(a==foo());return 174;}' 174
run_test 054 'int main(){int a; a=0;do{a+=1;}while(a && a < 174);return a;}' 174
run_test 055 'int main(){int a; a=-8;do{a+=1;}while(a);return a+174;}' 174
run_test 056 'int foo(){return 3;} int main() {int a; a = 0;while(a == foo()) {a = 3;}return 174;}' 174
run_test 057 'int main(){int a; int b; a = 0; b = 0; while(a <= 10) {b += a; a += 1;}return b;}' 55
run_test 058 'int main(){int a; a = 3;while (a) {a = 2;if (a - 3) {break;}a += 3;}return 174;}' 174
run_test 059 'int main(){int a; int b; int c; a = 3; b = 5; c = 0;while(a){while(b) {c += b;b-=1;if(b == 3) break;}b = 7;a-=1;if(a == 1) break;} return a*7+b*15+c*2;}' 174
run_test 060 'int main(){int a; a = 3;while (a) {a = 2;if (a - 3) {break;}a += 3;}return 174;}' 174
run_test 061 'int main(){int a; int b; a=11; b=0; while(a){a-=1;b+=a;if(a)continue;break; a+=100;} return b;}' 55
run_test 062 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return -a;}' 3
run_test 063 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return -b;}' 6
run_test 064 'int main(){int a; int b; a =-3; b=-6; return a*b*10+a+b+3;}' 174
run_test 065 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b*10;}' 180
run_test 066 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b*10+a+b+3;}' 174
run_test 067 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b;}' 18
run_test 068 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return b*a;}' 18
run_test 069 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return b*a*10;}' 180
run_test 070 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break;}while(a+3); return a*b;}' 18
run_test 071 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(!a)break;}while(a+3); return a*b;}' 18
run_test 072 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;}while(a+3); return a*b;}' 18
run_test 073 'int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;}while(a+3); return b*a;}' 18
run_test 074 'int main(){int a; int b; a=3; b=0; b+= ++a; return a*b*11-2;}' 174
run_test 075 'int main(){int a; int b; a=3; b=0; b+= a++; return !(b-3)+!(a-4)+172;}' 174
run_test 076 'int main(){int a; int b; a =0; b=0; do{b+=--a;}while(a+3); return b*a;}' 18
run_test 077 'int main(){int a; for (a = 3;a;) {a = 2;if (a - 3) {break;}a += 3;}return 174;}' 174
run_test 078 'int main(){int a; for (a = 3;;) {a = 2;if (a - 3) {break;}a += 3;}return 174;}' 174
run_test 079 'int main(){int a; int b; for(a=0,b=0;a <= 10;) {b += a; a += 1;}return b;}' 55
run_test 080 'int main(){int a; int b; for(a=11, b=0;a;){a-=1;b+=a;if(a)continue;break; a+=100;} return b;}' 55
run_test 081 'int main(){int a; int b; for(a=0,b=0;a <= 10;++a) {b += a;}return b;}' 55
run_test 082 'int main(){int a; int b; for(a=0,b=0;a <= 10;a++) {b += a;}return b;}' 55
run_test 083 'int main(){int a; int b; int c; int d; d=0; b = 5; c = 0;for(a = 3;a;d++){for(;b;++d) {c += b;b-=1;if(b == 3) break;}b = 7;a-=1;if(a == 1) break;} return a*7+b*15+c*2;}' 174
run_test 084 'int main(){int a; int b; for(a=0,b=0;a<10;a++){ if(a ==5)continue;b+=a;} return b;}' 40
run_test 085 'int main(){int a; a = 174; {int a; a = 3;} return a;}' 174
run_test 086 'int main(){int a; a = 3; { a = 174;} return a;}' 174
run_test 087 'int main() {int *b; int a; a = 3; a += 5;  return a + 166; }' 174
run_test 088 'int main() {int *******b; int a; a = 3; a += 5;  return a + 166; }' 174
run_test 089 'int main() {int a; a = 174; int *b; b = &a; return a;}' 174
run_test 090 'int main(){int x;x = 86;int *y;y = &x; return (*y) + x + 2;}' 174
run_test 091 'int main(){int x;x = 86;int *y;y = &x; return (*y) + (*y) + 2;}' 174
run_test 092 'int main(){int x;x = 86;int *y;y = &x;int **z;z = &y;return (*y) + (**z) + 2;}' 174
run_test 093 'int main(){int x;x = 86;int *y;y = &x;int **z;z = &y;return*y+**z+2;}' 174
run_test 094 'int main() {int x;int *y;x = 3;y = &x;*y = 174;return x;}' 174
run_test 095 'int main() {int x;int *y;x = 3;y = &x;*y = 171;*y += 3;return x;}' 174
run_test 096 'int main(){int x; int y; int *z; int*a; z=&x; a=&y; *z=*a=87; return(x+y);}' 174
run_test 097 'int main(){int x; int *y; int **z; z = &y; *z = &x; *y = 174; return x;}' 174
run_test 098 'int foo(int* p){return 3;} int main(){int x; return 174;}' 174
run_test 099 'int foo(int* p){return *p;} int main(){int x; x = 174; return foo(&x);}' 174
run_test 100 'int foo(int* p){*p = 172; return *p+2;} int main(){int x; return foo(&x);}' 174
run_test 101 'int *foo(int *p){*p = 4;return p;} int main(){int x;int *y;y = foo(&x); *y+= 170;return x;}' 174
run_test 102 'int *foo(int *p){*p = 4;return p;} int main(){int x;int y;*foo(&x) += 170;return x;}' 174
run_test_with_supplement0 103 'int *alloc4();int main(){int *p;p = alloc4(62, 8, 31, 85);return *p;}' 62
run_test_with_supplement0 104 'int *alloc4();int main(){int *p;int *r;p = alloc4(62, 8, 31, 85);int *q;q = p;return *q;}' 62
run_test_with_supplement0 105 'int *alloc4();int main(){int *p;int *r;p = alloc4(62, 8, 31, 85);int *q;q = p + 2;return *q;}' 31
run_test_with_supplement0 106 'int *alloc4();int main(){int *p;int c;int e;c = 2;int d;d = 1;p = alloc4(62, 8, 31, 85);int *q;q = p + c;return *(q+d) - *q + (*p-2)*2;}' 174
run_test_with_supplement0 107 'int *alloc4();int main(){int *p;p = alloc4(62, 8, 31, 85);int *q;q = p;return *q;}' 62
run_test_with_supplement0 108 'int *alloc4();int main(){int *p;p = alloc4(62, 8, 31, 85);int *q;q = p + 2;return *q;}' 31
run_test_with_supplement0 109 'int *alloc4();int main(){int *p;int c;c = 2;int d;d = 1;p = alloc4(62, 8, 31, 85);int *q;q = p + c;return *(q+d) - *q + (*p-2)*2;}' 174
run_test_with_supplement0 110 'int *alloc4();int main(){int *p;int c;c = 2;int d;d = 1;p = alloc4(62, 8, 31, 85);int *q;q = c + p;return *(d+q) - *q + (*p-2)*2;}' 174
run_test_with_supplement0 111 'int *alloc4();int main(){int *p;int c;c = -2;int d;d = 1;p = alloc4(62, 8, 31, 85);int *q;q = p - c;return *(d+q) - *q + (*p-2)*2;}' 174
run_test_with_supplement0 112 'int *alloc4();int main(){int *p; int *q; q = p+174; return q-p;}' 174
run_test 113 'int *foo(int *p){*p = 4;return p;} int main(){int x;int y; int **z; *foo(&x) += 170;return x;}' 174
run_test 114 'int main(){int a[2][3]; return 174;}' 174
run_test 115 'int x; int *y; int main(){return 174;}' 174
run_test 116 'int x; int *y; int main(){return x+174;}' 174
run_test 117 'int x; int *y; int main(){x=3; int a; a=2; y=&a; return x+*y+169;}' 174

run_test 118 'int main(){int a[1]; int *p; p = a; *p=2; return 174;}' 174
run_test 119 'int main(){int a[1]; *(a+0)=2;return 174;}' 174
run_test 120 'int x; int *y; int main(){x=3; int a[1]; *a=2; y=a; return x+*y+169;}' 174
run_test 121 'int x; int main(){x=3; int *y; y=&x; return *y+171;}' 174
run_test 122 'int a[1]; int main(){ *a=2;return 174;}' 174
run_test 123 'int main(){int a[1][2];int *q;q = *a;return 174;}' 174
run_test 124 'int main(){int a[1][2];int *q;q = *a; *q=174; return **a;}' 174
run_test 125 'int main(){int a[86][2];int *q;q = *(a+1); *q=174; return **(a+1);}' 174
run_test 126 'int main(){int a[5][6];int *q;q = *(a+1); *(2+q)=174; return *(*(1+a)+2);}' 174
run_test 127 'int changeBoard(int board[30][30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {*(*(board + i) + k) += d;*(*(board + k) + j) += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {*(*(board + k + (i - j)) + k) += d;}} else {for (k = 0; k < N - (j - i); k++) {*(*(board + k) + k + (j - i)) += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {*(*(board + i + j - k) + k) += d;}} else {for (k = i + j - N + 1; k < N; k++) {*(*(board + i + j - k) + k) += d;}}return 0;}int setQueen(int board[30][30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (*(*(board+num_placed)+j) == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}' 92
run_test 128 'int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && *(hist + j) != i && (*(hist + j) - i) != col - j && (*(hist + j) - i) != j - col; j++){}if (j < col)continue;*(hist+col) = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}' 92
run_test 129 'int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && *(hist + j) != i && (hist [j] - i) != col - j && (*(hist + j) - i) != j - col; j++){}if (j < col)continue;*(hist+col) = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}' 92
run_test 130 'int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}' 92
run_test 131 'int changeBoard(int board[30][30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {board[i][k] += d;board[k][j] += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {board [k + (i - j)][k] += d;}} else {for (k = 0; k < N - (j - i); k++) {board[k][k + (j - i)] += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {board[i + j - k][k] += d;}} else {for (k = i + j - N + 1; k < N; k++) {board[i + j - k][k] += d;}}return 0;}int setQueen(int board[30][30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (board[num_placed][j] == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}' 92
run_test 132 'int main(){int a[5][6];int *q;q = a[1]; 2[q]=174; return 1[a][2];}' 174

run_test 133 'char foo(){char a; return a;} int main(){foo(); return 174;}' 174
run_test 134 'char foo(char *p){char a; return a;} int main(){char q; foo(&q); return 174;}' 174
run_test 135 'char foo(char *p){char a; a = 5; return a;} int main(){char q; foo(&q); return 174;}' 174
run_test 136 'int main(){char x[3]; x[0] = -1; x[1] = 2; int y; y = 4; return x[0] + y + 171;}' 174
run_test 137 'char foo(char *p){*p = 5; char a;a = 3; return a;} int main(){char q; char r; r = foo(&q); return 172-r+q;}' 174
run_test 138 'char a;char foo(char *p){*p = 5; a = 3; return a;} int main(){char q; char r; r = foo(&q); return 172-r+q;}' 174
run_test 139 'int foo(char a){int d;d = 3;char c;c = a+d;return c;} int main(){char f;f=3;return foo(f)*4+150;}' 174
run_test 140 'int foo(char a){int d;d = 3;char c;c = a+d;return c*4;} int main(){char f;f=3;return foo(f)+150;}' 174
run_test_with_supplement1 141 'int qwerty(); int main(){char f;f=3;return qwerty(f,4)+150;}' 174
run_test_with_supplement1 142 'int qwer(); int main(){char f;f=3;return qwer(f,4)+150;}' 174
run_test 143 'int foo(char a, char b){return 23;} int main(){char f;f=3;return foo(f,4)+151;}' 174
run_test 144 'int foo(char a, char b){return a*4+11;} int main(){char f;f=3;return foo(f,4)+151;}' 174
run_test 145 'int foo(char a, char b){return a*4+12;} int main(){char f;f=3;return foo(f,4)+150;}' 174
run_test 146 'int foo(char a, char b){return (a+3)*4;} int main(){char f;f=3;return foo(f,4)+150;}' 174
run_test 147 'int foo(char a, char b){char c;c = a+3;return c*4;} int main(){char f;f=3;return foo(f,4)+150;}' 174
run_test 148 'int foo(char a, char b){int d;d = 3;char c;c = a+d;return c*4;} int main(){char f;f=3;return foo(f,4)+150;}' 174
run_test 149 'int foo(char a, char b){int d;d = 3;char c;c = a+d;return c*b;} int main(){char f;f=3;return foo(f,4)+150;}' 174
run_test 150 'char foo() { char *x;x = "1ab"; return x[0]; }int main(){ char *y;y = "a2b"; int z;z = 12; char a;a = y[1]; return (a-foo())*z+162;}' 174
run_test 151 'int printf();int main(){printf("%d %s", 1, "a");return 174;}' 174
run_test 152 'int printf();int puts();int A[200][200];int main() {int i; for (i = 1; i <= 12; i++) { printf("%d %d", i, i); puts(""); } return 0;}' 0
run_test 153 'int printf();int puts();int a(int b, int c) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { int j;j = a(0, i); printf("%d %d", i, j); puts("");} return 0;}' 0
run_test 154 'int printf();int puts();int A[200][200];int dfs(int row, int N) { if (row == N) return 1; int ret;ret = 0; int col;for (col = 0; col < N; col++) { int ok; ok = 1; int i; for (i = 1; i < N; i++) { if (row - i >= 0 && col - i >= 0) { ok = ok && A[row - i][col - i] == 0; } if (row - i >= 0) { ok = ok && A[row - i][col] == 0; } if (row - i >= 0 && col + i < N) { ok = ok && A[row - i][col + i] == 0; } } if (ok) { A[row][col] = 1; ret += dfs(row + 1, N); A[row][col] = 0; } } return ret;}int main() {int i; for (i = 1; i < 11; i++) { int j; j = dfs(0, i); printf("%d queen: %d", i, j); puts("");} return 0;}' 0
run_test 155 'int printf();int puts();int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int i; int hist[20]; for (i = 2; i < 11; i++) { count=0; solve(i, 0, hist); printf("%d queens: %d", i, count); puts("");} return 0;}' 0
run_test 156 'int main(){/**/return 123;}' 123
run_test 157 'int main(){/*u89g3wihu-@w3erolk*/ return (123);}' 123
run_test 158 'int/*/* 0^[o;:._/-*/main(){return ((((123))));}' 123
run_test 159 'int a; int main(){int *p; p = &a; int i; for(i=0;i<174;i++){++*p;} return a;}' 174
run_test 160 'int a; int main(){int *p; p = &a; int i; for(i=0;i<174;((i))++){++*p;} return a;}' 174
run_test 161 'int main(){int a[10]; a[5] = 173; int b; b = a[5]++; return a[5]*!(a[5]-b-1);}' 174

run_test 162 'int printf();int a() {return 3;}int main() {int i; printf("%d %d", i, a()); return 0;}' 0
run_test 163 'int foo(char *a, int b, int c){return 0;} int a(int N) {return 3;}int main() {int i; foo("%d %d", i, a(i)); return 0;}' 0
run_test_with_supplement0 164 'int foobar();int a(int N) {return 3;}int main() {int i; foobar("%d %d", i, a(i)); return 0;}' 0
run_test 165 'int printf();int a(int N) {return 3;}int main() {int i; printf("%d %d", i, a(i)); return 0;}' 0
run_test 166 'int printf();int puts();int a(int N) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { printf("%d %d", i, a(i)); puts("");} return 0;}' 0
run_test 167 'int printf();int puts();int A[200][200];int a(int row, int N) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { printf("%d %d", i, a(0, i)); puts("");} return 0;}' 0
run_test 168 'int printf();int puts();int A[200][200];int dfs(int row, int N) { if (row == N) return 1; int ret;ret = 0; int col;for (col = 0; col < N; col++) { int ok; ok = 1; int i; for (i = 1; i < N; i++) { if (row - i >= 0 && col - i >= 0) { ok = ok && A[row - i][col - i] == 0; } if (row - i >= 0) { ok = ok && A[row - i][col] == 0; } if (row - i >= 0 && col + i < N) { ok = ok && A[row - i][col + i] == 0; } } if (ok) { A[row][col] = 1; ret += dfs(row + 1, N); A[row][col] = 0; } } return ret;}int main() {int i; for (i = 1; i < 12; i++) { printf("%d queen: %d", i, dfs(0, i)); puts("");} return 0;}' 0

run_test 169 'int a(int b){ return b; }int main(){int i; i=1; a(i == 1? 1 : 2); return 0;}' 0
run_test 170 'int a(int b){ return b; }int main(){int i; for (i = 1; i < 11; i++) { a(i == 1? 1 : 2); } return 0;}' 0
run_test 171 'int a(int b){ return b; }int main(){int i; i=1; return a(i == 1? 174 : 2);}' 174

run_test 172 'int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf(i == 1? "a" : "b"); puts("");} return 0;}' 0
run_test 173 'int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf("%s", (i == 1? "a" : "b")); puts("");} return 0;}' 0
run_test 174 'int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf("%d %s: %d", i, (i == 1? " " : "s "), i); puts("");} return 0;}' 0
run_test 175 'int printf();int puts();int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { count=0; solve(i, 0, hist); printf("%d queen%s: %d", i, (i == 1? " " : "s "), count); puts("");} return 0;}' 0
run_test 176 'int main(){int a; int *p; p = &a; *p = 2; int *q; q = &*p; *q = 174; return a;}' 174
run_test 177 'int main(){int a; int *p; p = &a; *p = 2; int *q; q = &(*p); *q = 174; return a;}' 174
run_test 178 'char foo(char *p){char a; return a;} int main(){char q; foo(&(q)); return 174;}' 174
run_test 179 'char; char     ; char; int; int ; int; int;int;char foo(char *p){char a; return a;} int main(){char q; foo(&(q)); return 174;}' 174
run_test 180 ' struct A; char; char     ; char; int; int ; int; struct B;  int;int;  struct C; int main(){return 174;}' 174
run_test 181 ' struct A{int a; int b;}; char; char     ; char; int; int ; int; struct B{int c; int b;};  int;int;  struct C; int main(){return 174;}' 174
run_test 182 'int main(){ int; return 174;}' 174
run_test 183 'struct A{int a; int b;}; int main(){ struct A; return 174;}' 174
run_test 184 'struct A{int a; int b;}; int main(){ struct A a; return 174;}' 174
run_test 185 'struct A{int a; int b;}; int main(){ struct A a[10]; return 174;}' 174
run_test 186 'struct A{int a; int b;};  struct A a[10]; int main(){return 174;}' 174
run_test_with_supplement0 187 'int ptrdiff();struct A{int a; int b;}; int main(){ struct A *p; return ptrdiff(p+1, p);}' 8
run_test_with_supplement0 188 'int ptrdiff();struct A{int a; char c; char d; int b;}; int main(){ struct A *p; return ptrdiff(p+1, p);}' 12
run_test_with_supplement0 189 'int ptrdiff();struct A{int a; int *b; int c;};  struct A a[1][5]; int main(){return ptrdiff(a+1, a);}' 120

run_test 190 'int main(){return sizeof(int);}' 4
run_test 191 'int main(){return sizeof(int*);}' 8
run_test 192 'struct A{int a; int b;}; int main(){ return sizeof(struct A);}' 8
run_test 193 'struct A{int a; char c; char d; int b;}; int main(){ return sizeof(struct A);}' 12
run_test 194 'struct A{int a; int *b; int c;}; int main(){return sizeof(struct A [5]);}' 120
run_test 195 'struct A{int a; int *b; int c;}; struct B{char d; struct A e;}; int main(){return sizeof(struct B [4]);}' 128
run_test 196 'int *f(int *p){return p;} int main(){int a[4]; a[0] = 1; f(a)[0]++; f(a)[1] = 172; return a[1]+a[0];}' 174 
run_test 197 'struct A{char a; int b;}; int main(){struct A a; a.a = 74; return a.a;}' 74
run_test 198 'struct A{int a; int b;}; int main(){struct A a; a.a = 174; return a.a;}' 174
run_test 199 'struct A{int a; int b;}; int main(){struct A a; a.a = 174; return a.a;}' 174
run_test_with_supplement1 200 'int add_two_ints(); struct A{int a; int b;}; int main(){struct A a; a.a = 100; a.b = 74; return add_two_ints(&a);}' 174
run_test_with_supplement1 201 'int add_two_ints2(); struct A{int a; char c; char d; int b;}; int main(){struct A a; a.a = 100; a.b = 74; return add_two_ints2(&a);}' 174
run_test_with_supplement0 202 'struct A{int a; char c; char d; int b;}; struct A *get_struct_pointer(); int main(){ struct A *p; p = get_struct_pointer(100, 74); return p->a + p->b;  }' 174

run_test_with_supplement0 203 'int ptrdiff(); int main(){int *p; p = 0; return ptrdiff(p+1, p);}' 4
run_test 204 'int main(){int *p; p = 0; if(p) {return 4; } return 174;}' 174
run_test 205 'int main(){int *p; int a; p = &a; if(p) {return 4; } return 174;}' 4
run_test 206 'int main(){int *p; int a; p = &a; return p && &p;}' 1
run_test 207 'int main(){int *p; int a; p = &a; return p || &p;}' 1
run_test 208 'int main(){int *p; int a; p = &a; return p?174:1;}' 174
run_test 209 'int main(){int *p; p = 0; return p?174:1;}' 1
run_test 210 'int main(void){return 174;}' 174
run_test 211 'int main(void){void *p; p = 0; p = p; return 174;}' 174
run_test 212 'struct A{int a; int b;}; int main(){ struct A *p; void *q1; void *q2; q1 = p; q2 = p+1; char *r1; char *r2; r1 = q1; r2 = q2; return r2-r1;}' 8
run_test 213 'void f(int *p){*p = 174; return;} int main(void){ int a; f(&a); return a;}' 174
run_test 214 'int main(void){ foo: return 174;}' 174
run_test 215 'int main(void){ foo: bar: return 174;}' 174
run_test 216 'int main(void){ foo: {baz: hoge: 1;} bar: return 174;}' 174
run_test 217 'int main(void){ int a; a = 174; switch(1){a = 2; 1;} return a;}' 174
run_test 218 'int main(void){ int a; a = 174; switch(1){a = 2; break; a = 3;} return a;}' 174
run_test 219 'int main(void){ int a; a = 1; int b; b = 0; switch(1){ b = 15; default: a = 174; break; a = 3;} return a+b ;}' 174
run_test 220 'int main(void){ switch(1){ if(0){ default: return 174; } } return 3; }' 174
run_test 221 'int main(void){ int a; a = 1; switch(1){ default: a = 173; switch(0){ default: return a+1; } return 5; } return 3; }' 174
run_test 222 'int main(void){ int a; a = 1; switch(1){ case 1: a = 174; } return a; }' 174
run_test 223 'int main(void){ int a; a = 174; switch(2){ case 1: a = 1; } return a; }' 174
run_test 224 'int f(int a){switch(a){case 1: return 3; case 2: return 5; default: return 8;}} int main(void){ return (f(1)-3) || (f(2)-5) || (f(3)-8) || (f(100)-8);}' 0
run_test 225 'int main(){return _Alignof(int);}' 4
run_test 226 'int main(){return _Alignof(int*);}' 8
run_test 227 'struct A{int a; int b;}; int main(){ return _Alignof(struct A);}' 4
run_test 228 'struct A{int a; char c; char d; int b;}; int main(){ return _Alignof(struct A);}' 4
run_test 229 'struct A{int a; int *b; int c;}; int main(){return _Alignof(struct A [5]);}' 8
run_test 230 'void f(int *p){*p = 174;} int main(void){ int a; f(&a); return a;}' 174
run_test 231 'int main(void){ char a; a = 0; switch(a){case 0: a = 174; break; case 256: a = 3; break; default: a = 5; break;}  return a;}' 174
run_test 232 'enum A{B, C}; int main(void){ enum A b; return 174; }' 174
run_test 233 'enum A{B, C,}; int main(void){ enum A b; return 174; }' 174
run_test 234 'enum A{B, C,}; int main(void){ enum A b; b = 5; return 174; }' 174
run_test 235 'enum A{B, C,}; int main(void){ enum A b; b = B; return 174+b; }' 174
run_test 236 'enum A{B, C,D}; int main(void){ enum A b; b = D; return 172+b; }' 174
run_test 237 'enum A{B, C,D}; int f(enum A b){switch(b){case B: return 1; case C: return 5; case D: return 8;}} int main(void){ return (f(B) - 1) || (f(C) - 5) || (f(D) - 8);}' 0
run_test 238 'int main(){int a[5]; if(a){return 174;} return 0;}' 174

run_test 239 'int *foo(int *(p)){*p = 4;return p;} int main(){int (x);int (y); int (*(*(z))); *foo(&x) += 170;return x;}' 174
run_test 240 'int main(){int a[1][2];int (*p)[2];p = a;int *q;q = *p;return 174;}' 174
run_test 241 'int main(){int a[1][2];int (*p)[2];p = a;int *q;q = *p; *q=174; return **a;}' 174
run_test 242 'int main(){int a[74][2];int (*p)[2];p = a;int *q;q = *(p+1); *q=174; return **(a+1);}' 174
run_test 243 'int main(){int a[5][6];int (*p)[6];p = a;int *q;q = *(p+1); *(2+q)=174; return *(*(1+a)+2);}' 174
run_test 244 'int changeBoard(int (*board)[30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {*(*(board + i) + k) += d;*(*(board + k) + j) += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {*(*(board + k + (i - j)) + k) += d;}} else {for (k = 0; k < N - (j - i); k++) {*(*(board + k) + k + (j - i)) += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {*(*(board + i + j - k) + k) += d;}} else {for (k = i + j - N + 1; k < N; k++) {*(*(board + i + j - k) + k) += d;}}return 0;}int setQueen(int (*board)[30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (*(*(board+num_placed)+j) == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}' 92
run_test 245 'int changeBoard(int (*board)[30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {board[i][k] += d;board[k][j] += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {board [k + (i - j)][k] += d;}} else {for (k = 0; k < N - (j - i); k++) {board[k][k + (j - i)] += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {board[i + j - k][k] += d;}} else {for (k = i + j - N + 1; k < N; k++) {board[i + j - k][k] += d;}}return 0;}int setQueen(int (*board)[30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (board[num_placed][j] == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}' 92
run_test 246 'int main(){int a[5][6];int (*p)[6];p = a;int *q;q = p[1]; 2[q]=174; return 1[a][2];}' 174

run_test 247 'int (*func(int (*a)[5]))[5]{return a;} int main(){int a[6][5]; a[1][2] = 174; return func(a)[1][2];}' 174
run_test 248 'struct A {int a;};int main(){const struct A *a; return 174;}' 174
run_test 249 'struct A {int a;};int main(){const struct A const *const a; return 174;}' 174
run_test 250 'struct A {int a;};int f(int *const b){return 0;}int main(){const struct A const *const a; return 174;}' 174
run_test 251 'struct A {int a;};const int f(const int *const b){return 0;}int main(){const struct A const *const a; return 174;}' 174

run_test 252 'int main(void){int a = 5; int *p = &a; return 174;}' 174
run_test 253 'int main(void){int a = 4; int *p = &a; *p += 170; return a;}' 174
run_test 254 'int main(){int a; int *p = &a; *p = 2; int *q = &*p; *q = 174; return a;}' 174
run_test 255 'int main(){int a; int *p = &a; *p = 2; int *q = &(*p); *q = 174; return a;}' 174
run_test 256 'int main(){int x = 86;int *y = &x; return (*y) + x + 2;}' 174
run_test 257 'int main(){int x = 86;int *y = &x; return (*y) + (*y) + 2;}' 174
run_test 258 'int main(){int x = 86;int *y = &x;int **z = &y;return (*y) + (**z) + 2;}' 174
run_test 259 'int main(){int x = 86;int *y = &x;int **z = &y;return*y+**z+2;}' 174

#run_test 260 'int printf(); int puts(); int main(){printf("H""e" "l" "lo," " W" "or" "ld!"); puts(""); return 174;}' 174

run_test 261 'int main(void){int a = 5; return 174;}' 174
run_test 262 'int main(void){int u = 0; for(int a = 0; a < 10; a++){ u += a; } return 174+u-45;}' 174

run_test_with_supplement1 263 'int add6();int main(void){return add6(add6(1,2,3,4,5,6),7,8,9,10,174-55);}' 174
run_test 264 'int main(void){int a[5]; a[3] = 174; int *p = a; p += 3; return *p;}' 174
run_test 265 'int a[10][10]; int foo(int p[10][10]){int q;q = ((p+=1)-1)[0][0]; return q+p[0][0];} int main(){a[0][0] = 100; a[1][0] = 74; return foo(a);}' 174
run_test 266 'int main(void){int a[5]; a[1] = 174; int *p = a + 3; p -= 2; return *p;}' 174
run_test 267 'int main(void){char a[5]; a[1] = 74; char *p = a + 3; p -= 2; return *p;}' 74
run_test 268 'int a[10][10]; int foo(int p[10][10]){int q;q = p++[0][0]; return q+p[0][0];} int main(){a[0][0] = 100; a[1][0] = 74; return foo(a);}' 174
run_test 269 'int main(void){int a[5]; a[3] = 174; int *p = a + 2; p++; return *p;}' 174
run_test 270 'int main(void){int a[5]; a[3] = 174; int *p = a + 2; ++p; return *p;}' 174
run_test 271 'int main(void){int a[5]; a[3] = 174; int *p = a + 2; return *++p;}' 174
run_test 272 'int main(void){int a[5]; a[3] = 174; int *p = a + 3; return *p++;}' 174
run_test 273 'int main(void){char a[5]; a[1] = 74; char *p = a + 2; return *--p;}' 74

run_test 274 'int main(){int a[5];int *p = a;int *q = a+3;if (p < q) {return 174;} else {return 1;}}' 174
run_test 275 'int main(){int a[5];int *p = a;int *q = a+3;if (p > q) {return 174;} else {return 1;}}' 1
run_test 276 'int main(){int a[5];int *p = a;int *q = a+3;if (p <= q) {return 174;} else {return 1;}}' 174
run_test 277 'int main(){int a[5];int *p = a;int *q = a+3;if (p >= q) {return 174;} else {return 1;}}' 1
run_test 278 'int main(){int a[5];int *p = a;int *q = a;if (p < q) {return 174;} else {return 1;}}' 1
run_test 279 'int main(){int a[5];int *p = a;int *q = a;if (p > q) {return 174;} else {return 1;}}' 1
run_test 280 'int main(){int a[5];int *p = a;int *q = a;if (p <= q) {return 174;} else {return 1;}}' 174
run_test 281 'int main(){int a[5];int *p = a;int *q = a;if (p >= q) {return 174;} else {return 1;}}' 174
run_test 282 'int main(){int a[5];int *p = a;int *q = a;if (p == q) {return 174;} else {return 1;}}' 174
run_test 283 'int main(){int a[5];int *p = a;int *q = a;if (p != q) {return 174;} else {return 1;}}' 1
run_test 284 'int main(){int a[5];int *p = a;int *q = a+3;if (p == q) {return 174;} else {return 1;}}' 1
run_test 285 'int main(){int a[5];int *p = a;int *q = a+3;if (p != q) {return 174;} else {return 1;}}' 174

run_test 286 'int main(); int main(void){return 174;} int main(void);' 174

run_test 287 'int main(){int a[5];int *p = a;if (p == 0) {return 174;} else {return 1;}}' 1
run_test 288 'int main(){int a[5];int *p = 0;if (p == 0) {return 174;} else {return 1;}}' 174
run_test 289 'int main(){int a[5];int *p = 0;if (p != 0) {return 174;} else {return 1;}}' 1
run_test 290 'int main(){int a[5];int *p = a;if (p != 0) {return 174;} else {return 1;}}' 174

run_test 291 'int *foo(){return 0;} int main(){int *p = foo(); if (p == 0) {return 174;} else {return 1;} }' 174

run_test 292 'struct A{char a; int b;}; int main(){struct A a; a.a = 74; struct A b; b = a; return b.a;}' 74
run_test 293 'struct A{char a; int b;}; int main(){struct A a; a.a = 74; struct A b = a; return b.a;}' 74
run_test 294 'struct A{int a; int b;}; int main(){struct A a; a.a = 174; struct A b = a; return b.a;}' 174
run_test 295 'struct A{int a; int b;}; int main(){struct A a; a.a = 174; struct A b = a; return b.a;}' 174
run_test 296 'struct A{char a; int b; char c;}; int main(){struct A a; a.c = 74; struct A b = a; return b.c;}' 74
run_test_with_supplement1 297 'int add_two_ints(); struct A{int a; int b;}; int main(){struct A a; a.a = 100; a.b = 74; struct A b = a; return add_two_ints(&b);}' 174
run_test_with_supplement1 298 'int add_two_ints2(); struct A{int a; char c; char d; int b;}; int main(){struct A a; a.a = 100; a.b = 74; struct A b = a; return add_two_ints2(&b);}' 174
run_test_with_supplement0 299 'struct A{int a; char c; char d; int b;}; struct A *get_struct_pointer(); int main(){ struct A *p; p = get_struct_pointer(100, 74); struct A a = *p; return a.a + a.b;  }' 174
run_test_with_supplement0 300 'struct A{int a; char c; char d; int b;}; struct A *get_struct_pointer(); int main(){ struct A *p = get_struct_pointer(100, 74); struct A a = *p; return a.a + a.b;  }' 174
run_test_with_supplement0 301 'struct A{int a; char c; char d; int b;}; struct A *get_struct_pointer(); int main(){ struct A a = *get_struct_pointer(100, 74); return a.a + a.b;  }' 174

run_test_with_supplement0 302 'extern int GLOBAL_VAR; int main(){return 171 + GLOBAL_VAR;}' 174
run_test 303 'static int hidden() { return 3;} int main(){return 171 + hidden();}' 174

run_test 305 'int main(void) {int a[5]; a[3] = 174; int (*p)[5] = &a; return (*p)[3];} ' 174
run_test 306 'int main(void) {char a = 74; char *p = &a; return *p+100;} ' 174

run_test 307 'struct A{int a; int *b; int c;}; struct B{char d; struct A e;}; int main(){struct A a; a.a = 174; struct B b; b.e = a; return b.e.a;}' 174
run_test 308 'struct A{int a; int *b; int c;}; struct B{char d; struct A e;}; int main(){struct A a; a.a = 174; struct B b; b.e = a; return (b.e).a;}' 174
run_test 309 'struct A{int a; int *b; int c;}; struct B{char d; struct A e;}; int main(){struct A a; a.a = 174; struct B b; b.e.a = 174; a = b.e; return a.a;}' 174

run_test 310 'struct A{int a;}; int main(){struct A arr[5]; void *p = arr; int *q = p; q[3] = 174; return arr[3].a;}' 174

run_test 311 'struct A{int a;}; struct A f(void) {struct A u; u.a = 174; return u;} int main(void){struct A u = f(); return u.a;}' 174
run_test 312 'struct A{int a; int b; int *p;}; struct A f(void) {struct A u; u.a = 100; u.b = 74; u.p = 0; return u;} int main(void){struct A u = f(); if (u.p) {return 3;} else {return u.a + u.b;}}' 174

run_test 313 'int main(void){int p = 0; return (!p)*174; }' 174
run_test 314 'int main(void){int *p = 0; return (!p)*174; }' 174
run_test 315 'int main(void){int q; int *p = &q; return (1+!p)*174;}' 174


#run_test 316 'struct A{int a; int b; int *p;}; struct A f(void) {struct A u; u.a = 100; u.b = 74; u.p = 0; return u;} int main(void){struct A u = f(); struct A *p = &u; if (u.p) {return 3;} else {return p->a + p->b;}}' 174
#run_test 317 'struct A{int a; int b; int *p;}; struct A f(void) {struct A u; u.a = 100; u.b = 74; u.p = 0; return u;} int g (struct A *p) {return p->a + p->b;} int main(void){struct A u = f(); struct A *p = &u; if (u.p) {return 3;} else {return g(p);}}' 174
#run_test_with_supplement1 318 'struct A{int a; int b; int *p;}; struct A q(void); int g (struct A *p) {return p->a + p->b;} int main(void){struct A u = q(); struct A *p = &u; if (u.p) {return 3;} else {return g(p);}}' 174
#run_test_with_supplement1 319 'struct A{int a; int b; int *p;}; struct A q(void); int r(struct A *p); int main(void){struct A u = q(); struct A *p = &u; if (u.p) {return 3;} else {return r(p);}}' 174
#run_test 320 'struct A{int a; int b; int *p;}; struct A f(int j) {struct A u; u.a = 100; u.b = 72 + j; u.p = 0; return u;} int g (struct A *p) {return p->a + p->b;} int main(void){struct A u = f(2); struct A *p = &u; if (u.p) {return 3;} else {return g(p);}}' 174

#run_test_with_supplement1 321 'struct A{int a; int b; int *p; int *q; int *r; int *s;}; struct A test_(int *s); int main(void){int k; k = 100; struct A u = test_(&k); return u.a + *(u.s);}' 174

#run_test 322 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); return u.a + 74;}' 174
#run_test 323 'struct A{int a; int b; int *q; int *r; int *s; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100; u.b = 74; u.p = 0; return u;} int main(void){struct A u = f(); struct A *p = &u; if (u.p) {return 3;} else {return p->a + p->b;}}' 174

#run_test 324 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); return (u, 174);}' 174
#run_test 325 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); struct A v; return (v = u).a + 74;}' 174
#run_test 326 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); return (1,u).a + 74;}' 174
#run_test 326 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); return (1,2,u).a + 74;}' 174
#run_test 327 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); struct A v; v.a = 1; return (1? u : v).a + 74;}' 174
#run_test 328 'struct A{int a; int b; int *q; int *t; int *p;}; struct A f(void) {struct A u; u.a = 100;return u;} int main(void){struct A u = f(); struct A v; v.a = 1; return (0? u : v).a + 74;}' 75

run_test 329 'int main(){void *null = 0; int (*p)(void) = null; return 123;}' 123
run_test 330 'int main(){void *null = 0; int (*p)(int) = null; return 123;}' 123

run_test_with_supplement1 331 'void *return_fp(void); int call_fp(void* q); int main(){return call_fp(return_fp());}' 174
run_test 332 'int main(){int a = 1; int *b = a?&a : 0; return 123;}' 123
run_test 333 'int main(){int a = 1; int *b = a? 0 :&a; return 123;}' 123
run_test 334 'int main(){int a = 0; int *b = a?&a : 0; return 123;}' 123
run_test 335 'int main(){int a = 0; int *b = a? 0 :&a; return 123;}' 123
run_test_with_supplement1 336 'void *return_fp(void); int call_fp_(void* q){int (*p)(int) = q;return (p)(171);} int main(){return call_fp_(return_fp());}' 174
run_test_with_supplement1 337 'void *return_fp(void); int call_fp_(void* q){int (*p)(int) = q;return (*p)(171);} int main(){return call_fp_(return_fp());}' 174
run_test_with_supplement1 338 'void *return_fp(void); int call_fp_(void* q){int (*p)(int) = q;return (***********************p)(171);} int main(){return call_fp_(return_fp());}' 174
run_test_with_supplement1 339 'void *return_fp(void); int call_fp_(void* q){int (*p)(int) = q;return p(171);} int main(){return call_fp_(return_fp());}' 174
run_test_with_supplement1 340 'int add_3_(int a){return 3 + a;} void *return_fp_(void){return &add_3_;} int call_fp_(void* q){int (*p)(int) = q;return p(171);} int main(){return call_fp_(return_fp_());}' 174
run_test_with_supplement1 341 'int add_3_(int a){return 3 + a;} int (*return_fp_(void))(int){return &add_3_;} int call_fp_(int (*q)(int)){int (*p)(int) = q;return p(171);} int main(){return call_fp_(return_fp_());}' 174
run_test 342 'int printf(); int main(){(**************printf)("Hello, World!"); return 0;}' 0
run_test 343 'int puts(const char *str); int atoi(const char *str); int f(int a){int (*arr[2])(const char *str);arr[0] = &puts;arr[1] = &atoi;return arr[a]("123");} int main(){f(0); return f(1);}' 123
run_test 344 'int puts(const char *str); int atoi(const char *str); int f(int a){int (*arr[2])(const char *str);arr[0] = puts;arr[1] = atoi;return arr[a]("123");} int main(){f(0); return f(1);}' 123

run_test 344 'int main(){char a[456]; return a + 3 - a; }' 3
run_test 345 'struct A {int k[15];}; int main(){struct A s; return 3;}' 3
run_test 346 'struct A {int k[15]; int a;}; int main(){struct A s; s.a = 3; return s.a;}' 3
run_test 347 'struct A {int k[15]; int a;}; int main(){return sizeof(struct A);}' 64
run_test 348 'struct A {int k[15];}; int main(){struct A s; void *p = s.k; return 35;}' 35
run_test 349 'struct A {int k[15];}; int main(){struct A s; int *p = s.k; return 35;}' 35
run_test 350 'struct A {int k[15];}; int main(){struct A s; int (*p)[15] = &s.k; return 35;}' 35
run_test 351 'struct A {int k[15];}; int main(){struct A s; s.k[3] = 35; return s.k[3];}' 35
run_test 352 'struct A {int a; int b; int c;}; int main(){struct A a[5]; return a + 3 - a;}' 3
run_test 353 'struct A {int k[15];}; int main(){struct A a[5]; return a + 3 - a;}' 3

run_test 354 'struct A {int k[15];}; struct A f(int a, int b){struct A q; q.k[0] = a; q.k[14] = b; return q;} int main(){struct A (*g)(int a, int b) = f; struct A q = g(10, 11); return q.k[0] + q.k[14]; }' 21
run_test 355 'struct A3 {char a[3];}; struct A3 deref3(struct A3 *p){ return *p;} int main(){return 3;}' 3
run_test 356 'struct A {int a; int b;}; struct A f(int a, int b){struct A q; q.a = a; q.b = b; return q;} int main(){ struct A (*g)(int, int) = f; struct A q = g(100, 74); return q.a + q.b;}' 174

run_test 357 'int main() {return 0;} //nfsjdgkssfdvc' 0
run_test 358 'int main() {return __func__[1] - 97;} ' 0

run_test 359 'int main() {goto a; return 3; a: return 0;} ' 0
run_test 360 'int main(){ int i = 3; goto a; for (i = 0; i < 10; i++) { a: return i; } }' 3

run_test_with_supplement0 361 'int add8(); int main(){ return add8(-1,-2,3,-4,5,6,-7,8); }' 8
# run_test_with_supplement0 362 'struct INT_CHARS_INT { int a; char c[100]; int b; };struct INT_CHARS_INT merge7(); int main(){ struct INT_CHARS_INT st = merge7(1,2,3,4,5,6,7); return st.b - st.a; }' 4

run_test 363 'union A { char a[7]; int b; }; int main(void) { return sizeof(union A); }' 8
run_test 364 'union A { char a[4]; int b; }; int main(void) { union A x; x.a[0] = 0x4b; x.a[1] = 0x6f; x.a[2] = 0x72; x.a[3] = 0x79; return x.b - 0x79726f4b; }' 0

run_test 365 'int foo(void) { return 3; } int bar(void) { return 5;} int main(void) { int (*foo1)(void) = foo; int (*bar1)(void) = bar; return (1? foo1 : bar1)(); }' 3
run_test 366 'int foo(void) { return 3; } int main(void) { return (1? foo : 0)(); }' 3
run_test 367 'int foo(void) { return 3; } int bar(void) { return 5;} int main(void) { return (1? foo : bar)(); }' 3
run_test 368 'int foo(void) { return 3; } int bar(void) { return 5;} int main(void) { return (0? foo : bar)(); }' 5

run_test 369 'enum Foo { ZERO } foo() { return ZERO; } int main() { int a = foo(); return a; }' 0
# run_test 370 'enum { ZERO } foo() { return ZERO; } int main() { int a = foo(); return a; }' 0
run_test 371 'int main() { int a; if (1) { a = 3; } else { a = 7; } return a; }' 3
run_test 372 'void foo(int*p) {*p=3;} int main() { int a; if (0) { foo(&a); } else { a = 7; } return a; }' 7
run_test 373 'void foo(int*p) {*p=7;} int main() { int a; if (0) { a = 3; } else { foo(&a); } return a; }' 7
run_test 374 'int main() { int a; goto a; if (0) { a: a = 3; } else { a = 7; } return a; }' 3
run_test 375 'int main() { int a; goto a; if (1) { a = 3; } else { a: a = 7; } return a; }' 7

run_test pp_1 'int main(){\n#ifdef TEST\nreturn 170;\n#endif\nreturn 174;}' 174
run_test pp_2 'int main(){\n#ifndef TEST\nreturn 174;\n#endif\n}' 174

run_test pp_3 '#define TEST\n int main() { int TEST test; test = 174; return test;}' 174
run_test pp_4 '#define TEST int\n TEST main() { TEST test; test = 174; return test;}' 174
run_test pp_5 '#define TEST\n #undef TEST\n #ifndef TEST\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174

run_test pp_6 '#if 1\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174
run_test pp_7 '#define TEST 1 \n #if TEST\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174

run_test pp_8 '#define TEST\n #if defined(TEST)\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174
run_test pp_9 '#define TEST\n #if defined TEST\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174

run_test pp_10 '#if 1+2 -2\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174
run_test pp_11 '#if 2*2 - 3*1\n int n = 10; \n #endif\n int main() {return 164 + n;}' 174

run_test pp_12 '#ifdef TEST\n int n = 10; \n #else \n int n = 20; \n #endif\n int main() {return 154 + n;}' 174
run_test pp_13 '#if 1\n int n = 10; \n #else \n int n = 20; \n #endif\n int main() {return 164 + n;}' 174

run_test pp_14 '#define stdin (stdin)\n int main() {int stdin;return 174;}' 174

run_test typedef_1 'typedef int TEST; TEST main(){return 174;}' 174
run_test typedef_2 'typedef struct A TEST; int main(){ TEST *p; return 174;}' 174
run_test typedef_3 'typedef void *TEST; int main(){  return 166 + sizeof(TEST);}' 174

run_test parameter_type_list_1 'int test(void *);int main(){return 174;}' 174
run_test parameter_type_list_1 'int test(void *,int (*)[10] );int main(){return 174;}' 174

run_test bool_1 'int main() {return 173 + sizeof(_Bool);}' 174
run_test bool_2 'int main() {return 173 + _Alignof(_Bool);}' 174
run_test bool_3 'int main() {_Bool test; test = 1; if(test)return 174;else return 170;}' 174
run_test bool_4 'int main() {_Bool test; test = 256; if(test)return 174;else return 170;}' 174

run_test variable_arg_1 'int test(char *fmt,...){return *fmt;} int main(){int n = test("aaa",10); return n+77;}' 174
run_test variable_arg_2 'int test(char *fmt,...){__builtin_va_list ap; __builtin_va_start(ap,fmt); __builtin_va_end(ap); return *fmt;} int main(){int n = test("aaa",10); return n+77;}' 174
run_test_with_supplement0 variable_arg_3 'int sum_args();int sum(int n,...){__builtin_va_list ap;__builtin_va_start(ap,n);int ret = sum_args(n,ap);__builtin_va_end(ap);return ret;} int main(){return sum(4,160,1,3,10);}' 174

run_test union_1 'union Test {int *p;char a[6];}; int main(){union Test x; return sizeof(union Test) + 166;}' 174
run_test union_2 'union Test {int n;char a[6];}; int main(){union Test x; x.n = 170; return x.n + 4;}' 174
run_test union_2 'union Test {int n;char a[6];}; int main(){union Test x; x.n = 170; x.a[0] = 10;x.a[1] = 20; return x.a[0] + x.a[1] + 144;}' 174

run_test long_1 'int main(){ long n; return 174;}' 174
run_test long_2 'int main(){ long n = 10; return 164 + n;}' 174
run_test long_3 'int main(){ long n = 10L; return 164 + n;}' 174

run_test_with_supplement0 more_6_args_1 'int add8(); int main(){ return add8(-1,-2,3,-4,5,6,-7,8); }' 8
run_test more_6_args_2 'int printf(); int main(){ printf("%d,%d,%d,%d,%d,%d,%d,%d",1,2,3,4,5,6,7,8);return 174; }' 174
run_test more_6_args_3 'int printf(); int main(){ printf("%d,%d,%d,%d,%d,%d,%d,%d,%d",1,2,3,4,5,6,7,8,9);return 174; }' 174
run_test more_6_args_4 'int add8(int a,int b,int c,int d,int e,int f,int g,int h){return a+b+c+d+e+f+g+h;} int main(){ return add8(-1,-2,3,-4,5,6,-7,8); }' 8
run_test more_6_args_4 'int add8(int a,int b,int c,int d,int e,int f,int g,int h){return a-b*c+d/e+f*g*h;} int main(){ return add8(10,2,5,8,2,1,4,1); }' 8

run_test type_spec_1 'int main(){unsigned long n = 10; return 164+n;}' 174
run_test type_spec_2 'int main(){unsigned n = 10; return 164+n;}' 174

run_test global_init 'int test = 10; int main(){int a = 164;return a+test;}' 174

run_test multiple_declarator_1 'int a,b; int main(){a = 164;b = 10;return a+b;}' 174
run_test multiple_declarator_2 'int a,b,c,d; int main(){a = 114;b = 10;c = 20;d = 30;return a+b+c+d;}' 174
run_test multiple_declarator_3 'int main(){int a,b;a = 164;b = 10;return a+b;}' 174
run_test multiple_declarator_4 'int main(){int a,b,c,d;a = 114;b = 10;c = 20;d = 30;return a+b+c+d;}' 174
run_test multiple_declarator_5 'struct Test{int a,b;};int main(){return 166 + sizeof(struct Test);}' 174
run_test multiple_declarator_6 'struct Test{int a,b;};int main(){struct Test t;t.a = 10;t.b = 20; return 144+t.a+t.b;}' 174

run_test constexpr_cast '_Bool n = 256; int main(){return 173+n;}' 174

run_test constexpr_1 'int n = 2*5; int main() {return 164+n;}' 174
run_test constexpr_1 'int n = 6+4; int main(){return 164+n;}' 174
run_test constexpr_2 'int n = 2*3 + 1*4; int main(){return 164+n;}' 174
run_test constexpr_3 'int n = (3 | 8) ^ 1; int main(){return 164+n;}' 174
run_test constexpr_4 'char *str = "0123456789"; int main(){return str[0] - 0x30 + 174;}' 174
run_test constexpr_5 'char *str = (void *) 0; int main(){if(str)return 164;else return 174;}' 174
run_test constexpr_6 'int num; int *p = &num; int main(){return 174;}' 174
run_test constexpr_7 'int num = 0; int *p = &num; int main(){ *p = 10; return 164+num; }' 174
run_test constexpr_8 'int a,b,c,d; int *p[4] = {&a,&b,&c,&d}; int main(){ *p[0] = 10; *p[2] = 20; return a+c+144;}' 174

run_test initialize_list_1 'int a[4] = {1,2,3,4}; int main(){return 164+a[0]+a[1]+a[2]+a[3];}' 174
run_test initialize_list_2 'char *a[4] = {"a","b","c","d"}; int main(){return 174 + (a[0][0] - '\''a'\'' ) + (a[1][0] - '\''b'\'');}' 174
run_test initialize_list_3 'struct Test{int n;int *p;}; struct Test t = {10,(void *)0}; int main(){return 164 + t.n + (t.p ? 10 : 0);}' 174
run_test initialize_list_4 'int a[] = {1,2,3,4}; int main(){return 164+a[0]+a[1]+a[2]+a[3];}' 174
run_test initialize_list_5 'int main(){int a[4] = {1,2,3,4}; return 164+a[0]+a[1]+a[2]+a[3];}' 174
run_test initialize_list_6 'struct Test{int n;int *p;}; int main(){ struct Test t = {10,(void *)0}; return 164 + t.n + (t.p ? 10 : 0);}' 174
run_test initialize_list_7 'struct Register{char *alias[4];}; struct Register t = {.alias={"al","ax","eax","rax"}}; int main(){return 174+t.alias[0][0] - 97 + t.alias[3][0] - 114;}' 174

run_test function_like_macro_1 '#define DOUBLE(a) 174\n int main(){return DOUBLE(87);}' 174
run_test function_like_macro_2 '#define DOUBLE(a) 2*a\n int main(){return DOUBLE(87);}' 174
run_test function_like_macro_3 '#define TEST(a,b) 10*a + 2 * b \n int main(){return TEST(10,37);}' 174
run_test function_like_macro_4 '#define DECL(a,b) int a = b \n int main(){DECL(n,174); return n;}' 174
run_test function_like_macro_5 '#define DOUBLE(a) 2*a\n int main(){return DOUBLE((10,87));}' 174
run_test function_like_macro_6 '#define DOUBLE(a) 2*a\n int main(){return DOUBLE((10,(20,87)));}' 174

run_test enum_value_1 'enum Test{ A=10,B=14,C=16}; int main(){return 160+B;}' 174
run_test enum_value_1 'enum Test{ A=10,B,C,D,E}; int main(){return 160+E;}' 174

run_test typedef_test_1 'typedef long long TEST,ABC; int main(){TEST n = 164; ABC m = 10; return n+m;}' 174

run_test member_initialize 'struct Test{int n;int *p;}; int main(){struct Test t = {.p = (void *)0}; if(t.p)return 160;else return 174;}' 174

run_test concat_token_1 '#define TEST a ## b\n int TEST = 10; int main(){return 164 + ab;}' 174
run_test concat_token_2 '#define TEST(x) a ## x\n int TEST(1) = 10,TEST(2) = 20; int main(){return 144 + a1 + a2;}' 174
run_test concat_token_3 '#define TEST(x) CONSTANT_ ## x\n #define CONSTANT_TEN 10\n int main(){return 164 + TEST(TEN);}' 174

run_test local_struct_1 'int main(){struct Test{int a;int *p;}; struct Test t = {.a = 174,.p = (void *)0}; if(t.p)return 164;else return t.a;}' 174
run_test local_struct_2 'int main(){ { struct Test{char c;}; }  { struct Test{int *p;int *q;};  return sizeof(struct Test) + 158; } }' 174

run_test local_union_1 'int main(){union Test{int a;int *p;}; union Test t; t.a = 10; return 164 + t.a;}' 174
run_test local_union_2 'int main(){ { union Test{char c;}; }  { union Test{int *p;int *q;};  return sizeof(union Test) + 166; } }' 174

run_test local_typedef_1 'int main(){typedef char AA; AA n = 10; return 164+ n;}' 174
run_test local_typedef_1 'int main(){ { typedef int *AA; AA n; } {typedef char AA; AA n = 10; return 164+ n;} }' 174

run_test local_enum_1 'int main(){ enum Test{A,B,C}; int n = C; return 172+n;}' 174
run_test local_enum_1 'int main(){ { enum Test{A,B,C}; int n = C; } { enum Test{D,E,F}; int n = F; return 172+n;} }' 174

run_test short_1 'int main(){short t = 10; return 162 + t + sizeof(t);}' 174

run_test cast_1 'int test(_Bool a){if(a) return 174;else return 0;} int main(){return test(256); }' 174
run_test cast_2 'int main(){int *p = (void *) 0; _Bool t = p; if(t)return 164;else return 174; }' 174

run_test unsigned_1 'int main(){unsigned char n = 0xff; if(n==0xff)return 174;else return 160;}' 174
run_test unsigned_2 'int main(){unsigned char n = 0xff; unsigned char m = 0x10; if(m - n < 0)return 174;else return 160;}' 174
run_test unsigned_3 'int main(){unsigned int n = 0x7fffffffU; unsigned int m = 0x80000000U; if(n < m)return 174;else return 160;}' 174
run_test unsigned_4 'int main(){unsigned int n = 0x7fffffffU; unsigned int m = 0x80000000U; if(n <= m)return 174;else return 160;}' 174
run_test unsigned_5 'int main(){unsigned int n = 0x80000000U; unsigned int m = 0x80000000U; if(n <= m)return 174;else return 160;}' 174
run_test unsigned_6 'int main(){unsigned int n = 0x80000000U; unsigned int m = 0x400000U; if(n/m == 0x200)return 174;else return 160;}' 174
run_test unsigned_7 'int main(){unsigned int n = 0x80000000U; if(n >> 28 == 0x8)return 174;else return 160;}' 174
run_test unsigned_8 'int main(){unsigned int n = 2; unsigned int m = 5; if(n * m == 10)return 174;else return 160;}' 174
run_test unsigned_9 'int main(){unsigned long long n = 0xffffffffffffffffULL; if(n >> 60 == 0xf)return 174;else return 160;}' 174
run_test unsigned_10 'int main(){unsigned long long n = 0x8000000000000000ULL; if(n >> 60 == 0x8)return 174;else return 160;}' 174

run_test anonymous_struct_1 'struct Test{ int n; union {int *p; int m;}; }; int main(){ return 174; }' 174
run_test anonymous_struct_2 'struct Test{ int n; union {int *p; int m;}; }; int main(){ struct Test t; t.n = 10; t.m = 20; return 144+t.n+t.m; }' 174
run_test anonymous_struct_3 'struct Test{ int n; struct {int *p; int m;}; }; int main(){ struct Test t; t.n = 10; t.m = 20; return 144+t.n+t.m; }' 174
run_test anonymous_struct_4 'union Test{ int n; struct {int *p; int m;}; }; int main(){ union Test t; t.n = 10; t.m = 20; return 144+t.n+t.m; }' 174
run_test anonymous_struct_5 'union Test{ int n; union {int *p; int m;}; }; int main(){ union Test t; t.n = 10; return 164+t.n; }' 174

run_test enum_1 'enum { A,B }; int main(){ return 173+B; }' 174

run_test va_args_macro_1 '#define test(a,b,...) \n int main(){return 174;}' 174
run_test va_args_macro_2 '#define test(a,b,...) a+b+sum(__VA_ARGS__) \n int sum(int a,int b){return a+b;} int main(){return test(114,10,20,30);}' 174
run_test va_args_macro_3 '#define test(a,b,...) a+b+sum(__VA_ARGS__) \n int sum(int a,int b,int c){return a+b*c;} int main(){return test(114,10,20,5,6);}' 174

run_test float_1 'int main(){float a; return 174;}' 174
run_test float_2 'int main(){float a = 0; return 174;}' 174
run_test float_2 'int main(){float a = 0; int b = a; return 174;}' 174
run_test float_2 'int main(){float a = 10; float b = a + 20; return 174;}' 174
run_test float_2 'int main(){float a = 10; float b = a - 20; return 174;}' 174
run_test float_2 'int main(){float a = 10; float b = a * 20; return 174;}' 174
run_test float_2 'int main(){float a = 10; float b = a / 20; return 174;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b > 3) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b >= 3) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b < 4) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b <= 4) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b == 4) return 164; else return 174;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; if(a/b != 4) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; a += b; if(a > 12) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; a -= b; if(a < 8) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; a *= b; if(a > 29) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = 3; a /= b; if(a < 4) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = +a; if(b > 9) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = -a; if(b < -9) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = ++a; if(a > 10 && b > 10) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = a++; if(a > 10 && b < 11) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = --a; if(a < 10 && b < 10) return 174; else return 164;}' 174
run_test float_3 'int main(){float a = 10; float b = a--; if(a < 10 && b > 9) return 174; else return 164;}' 174

run_test double_1 'int main(){double a; return 174;}' 174
run_test double_2 'int main(){double a = 0; return 174;}' 174
run_test double_2 'int main(){double a = 0; int b = a; return 174;}' 174
run_test double_2 'int main(){double a = 10; double b = a + 20; return 174;}' 174
run_test double_2 'int main(){double a = 10; double b = a - 20; return 174;}' 174
run_test double_2 'int main(){double a = 10; double b = a * 20; return 174;}' 174
run_test double_2 'int main(){double a = 10; double b = a / 20; return 174;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b > 3) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b >= 3) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b < 4) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b <= 4) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b == 4) return 164; else return 174;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; if(a/b != 4) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; a += b; if(a > 12) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; a -= b; if(a < 8) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; a *= b; if(a > 29) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = 3; a /= b; if(a < 4) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = +a; if(b > 9) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = -a; if(b < -9) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = ++a; if(a > 10 && b > 10) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = a++; if(a > 10 && b < 11) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = --a; if(a < 10 && b < 10) return 174; else return 164;}' 174
run_test double_3 'int main(){double a = 10; double b = a--; if(a < 10 && b > 9) return 174; else return 164;}' 174

run_test floating_cast 'int main(){float a = 10;double b = 20; if(29<a+b && a+b < 31) return 174;else return 164;}' 174
run_test floating_cast 'int main(){double a = 10;float b = a; if(19<a+b && a+b < 21) return 174;else return 164;}' 174

run_test floating_constant 'int main(){float a = 1.23; return 174;}' 174
run_test floating_constant 'int main(){float a = 10.5; if(2*a > 20.5) return 174; else return 164;}' 174

run_test typedef 'typedef int T[3]; int main(){ T test; test[0] = 10; return 164 + test[0]; }' 174
run_test typedef 'typedef int T[]; int main(){ T test = {10,20,30}; return 114 + test[0] + test[1] + test[2]; }' 174

run_test floating_return 'float add(int a,int b){float sum = 0; sum+=a; sum+=b; return sum;} int main(){float a = add(10,20); if(a > 29)return 174;else return 164;}' 174

run_test_with_supplement1 floating_argument 'extern float add_float(float,float); int main(){float a = add_float(10.0,20.0); if(a > 29)return 174;else return 164;}' 174
run_test floating_argument 'float add(float a,float b){float sum = 0;sum += a;sum += b;return sum;} int main(){float a = add(10.0,20.0); if(a > 29)return 174;else return 164; }' 174
run_test floating_argument 'void calc(float a,long b,double c,int d,float *p){float sum = 0;sum += a;sum += b*c; sum -= d; *p = sum;} int main(){float a=0; calc(10,2,12.5,5,&a); if(a > 29)return 174;else return 164; }' 174
