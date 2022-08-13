# 1 "./file.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./file.c"
# 1 "/usr/local/musl/include/errno.h" 1 3 4







# 1 "/usr/local/musl/include/features.h" 1 3 4
# 9 "/usr/local/musl/include/errno.h" 2 3 4

# 1 "/usr/local/musl/include/bits/errno.h" 1 3 4
# 11 "/usr/local/musl/include/errno.h" 2 3 4



# 13 "/usr/local/musl/include/errno.h" 3 4
__attribute__((const))

int *__errno_location(void);
# 2 "./file.c" 2
# 1 "/usr/local/musl/include/stdio.h" 1 3 4
# 26 "/usr/local/musl/include/stdio.h" 3 4
# 1 "/usr/local/musl/include/bits/alltypes.h" 1 3 4
# 50 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef unsigned long size_t;
# 65 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef long ssize_t;
# 162 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef long off_t;
# 320 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef struct _IO_FILE FILE;





typedef __builtin_va_list va_list;




typedef __builtin_va_list __isoc_va_list;
# 27 "/usr/local/musl/include/stdio.h" 2 3 4
# 56 "/usr/local/musl/include/stdio.h" 3 4
typedef union _G_fpos64_t {
 char __opaque[16];
 long long __lldata;
 double __align;
} fpos_t;

extern FILE *const stdin;
extern FILE *const stdout;
extern FILE *const stderr;





FILE *fopen(const char *restrict, const char *restrict);
FILE *freopen(const char *restrict, const char *restrict, FILE *restrict);
int fclose(FILE *);

int remove(const char *);
int rename(const char *, const char *);

int feof(FILE *);
int ferror(FILE *);
int fflush(FILE *);
void clearerr(FILE *);

int fseek(FILE *, long, int);
long ftell(FILE *);
void rewind(FILE *);

int fgetpos(FILE *restrict, fpos_t *restrict);
int fsetpos(FILE *, const fpos_t *);

size_t fread(void *restrict, size_t, size_t, FILE *restrict);
size_t fwrite(const void *restrict, size_t, size_t, FILE *restrict);

int fgetc(FILE *);
int getc(FILE *);
int getchar(void);
int ungetc(int, FILE *);

int fputc(int, FILE *);
int putc(int, FILE *);
int putchar(int);

char *fgets(char *restrict, int, FILE *restrict);




int fputs(const char *restrict, FILE *restrict);
int puts(const char *);

int printf(const char *restrict, ...);
int fprintf(FILE *restrict, const char *restrict, ...);
int sprintf(char *restrict, const char *restrict, ...);
int snprintf(char *restrict, size_t, const char *restrict, ...);

int vprintf(const char *restrict, __isoc_va_list);
int vfprintf(FILE *restrict, const char *restrict, __isoc_va_list);
int vsprintf(char *restrict, const char *restrict, __isoc_va_list);
int vsnprintf(char *restrict, size_t, const char *restrict, __isoc_va_list);

int scanf(const char *restrict, ...);
int fscanf(FILE *restrict, const char *restrict, ...);
int sscanf(const char *restrict, const char *restrict, ...);
int vscanf(const char *restrict, __isoc_va_list);
int vfscanf(FILE *restrict, const char *restrict, __isoc_va_list);
int vsscanf(const char *restrict, const char *restrict, __isoc_va_list);

void perror(const char *);

int setvbuf(FILE *restrict, char *restrict, int, size_t);
void setbuf(FILE *restrict, char *restrict);

char *tmpnam(char *);
FILE *tmpfile(void);




FILE *fmemopen(void *restrict, size_t, const char *restrict);
FILE *open_memstream(char **, size_t *);
FILE *fdopen(int, const char *);
FILE *popen(const char *, const char *);
int pclose(FILE *);
int fileno(FILE *);
int fseeko(FILE *, off_t, int);
off_t ftello(FILE *);
int dprintf(int, const char *restrict, ...);
int vdprintf(int, const char *restrict, __isoc_va_list);
void flockfile(FILE *);
int ftrylockfile(FILE *);
void funlockfile(FILE *);
int getc_unlocked(FILE *);
int getchar_unlocked(void);
int putc_unlocked(int, FILE *);
int putchar_unlocked(int);
ssize_t getdelim(char **restrict, size_t *restrict, int, FILE *restrict);
ssize_t getline(char **restrict, size_t *restrict, FILE *restrict);
int renameat(int, const char *, int, const char *);
char *ctermid(char *);







char *tempnam(const char *, const char *);




char *cuserid(char *);
void setlinebuf(FILE *);
void setbuffer(FILE *, char *, size_t);
int fgetc_unlocked(FILE *);
int fputc_unlocked(int, FILE *);
int fflush_unlocked(FILE *);
size_t fread_unlocked(void *, size_t, size_t, FILE *);
size_t fwrite_unlocked(const void *, size_t, size_t, FILE *);
void clearerr_unlocked(FILE *);
int feof_unlocked(FILE *);
int ferror_unlocked(FILE *);
int fileno_unlocked(FILE *);
int getw(FILE *);
int putw(int, FILE *);
char *fgetln(FILE *, size_t *);
int asprintf(char **, const char *, ...);
int vasprintf(char **, const char *, __isoc_va_list);
# 3 "./file.c" 2
# 1 "/usr/local/musl/include/stdlib.h" 1 3 4
# 21 "/usr/local/musl/include/stdlib.h" 3 4
# 1 "/usr/local/musl/include/bits/alltypes.h" 1 3 4
# 10 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef int wchar_t;
# 22 "/usr/local/musl/include/stdlib.h" 2 3 4

int atoi (const char *);
long atol (const char *);
long long atoll (const char *);
double atof (const char *);

float strtof (const char *restrict, char **restrict);
double strtod (const char *restrict, char **restrict);
long double strtold (const char *restrict, char **restrict);

long strtol (const char *restrict, char **restrict, int);
unsigned long strtoul (const char *restrict, char **restrict, int);
long long strtoll (const char *restrict, char **restrict, int);
unsigned long long strtoull (const char *restrict, char **restrict, int);

int rand (void);
void srand (unsigned);

void *malloc (size_t);
void *calloc (size_t, size_t);
void *realloc (void *, size_t);
void free (void *);
void *aligned_alloc(size_t, size_t);

_Noreturn void abort (void);
int atexit (void (*) (void));
_Noreturn void exit (int);
_Noreturn void _Exit (int);
int at_quick_exit (void (*) (void));
_Noreturn void quick_exit (int);

char *getenv (const char *);

int system (const char *);

void *bsearch (const void *, const void *, size_t, size_t, int (*)(const void *, const void *));
void qsort (void *, size_t, size_t, int (*)(const void *, const void *));

int abs (int);
long labs (long);
long long llabs (long long);

typedef struct { int quot, rem; } div_t;
typedef struct { long quot, rem; } ldiv_t;
typedef struct { long long quot, rem; } lldiv_t;

div_t div (int, int);
ldiv_t ldiv (long, long);
lldiv_t lldiv (long long, long long);

int mblen (const char *, size_t);
int mbtowc (wchar_t *restrict, const char *restrict, size_t);
int wctomb (char *, wchar_t);
size_t mbstowcs (wchar_t *restrict, const char *restrict, size_t);
size_t wcstombs (char *restrict, const wchar_t *restrict, size_t);




size_t __ctype_get_mb_cur_max(void);
# 101 "/usr/local/musl/include/stdlib.h" 3 4
int posix_memalign (void **, size_t, size_t);
int setenv (const char *, const char *, int);
int unsetenv (const char *);
int mkstemp (char *);
int mkostemp (char *, int);
char *mkdtemp (char *);
int getsubopt (char **, char *const *, char **);
int rand_r (unsigned *);






char *realpath (const char *restrict, char *restrict);
long int random (void);
void srandom (unsigned int);
char *initstate (unsigned int, char *, size_t);
char *setstate (char *);
int putenv (char *);
int posix_openpt (int);
int grantpt (int);
int unlockpt (int);
char *ptsname (int);
char *l64a (long);
long a64l (const char *);
void setkey (const char *);
double drand48 (void);
double erand48 (unsigned short [3]);
long int lrand48 (void);
long int nrand48 (unsigned short [3]);
long mrand48 (void);
long jrand48 (unsigned short [3]);
void srand48 (long);
unsigned short *seed48 (unsigned short [3]);
void lcong48 (unsigned short [7]);



# 1 "/usr/local/musl/include/alloca.h" 1 3 4
# 9 "/usr/local/musl/include/alloca.h" 3 4
# 1 "/usr/local/musl/include/bits/alltypes.h" 1 3 4
# 10 "/usr/local/musl/include/alloca.h" 2 3 4

void *alloca(size_t);
# 141 "/usr/local/musl/include/stdlib.h" 2 3 4
char *mktemp (char *);
int mkstemps (char *, int);
int mkostemps (char *, int, int);
void *valloc (size_t);
void *memalign(size_t, size_t);
int getloadavg(double *, int);
int clearenv(void);


void *reallocarray (void *, size_t, size_t);
void qsort_r (void *, size_t, size_t, int (*)(const void *, const void *, void *), void *);
# 4 "./file.c" 2
# 1 "/usr/local/musl/include/string.h" 1 3 4
# 25 "/usr/local/musl/include/string.h" 3 4
# 1 "/usr/local/musl/include/bits/alltypes.h" 1 3 4
# 343 "/usr/local/musl/include/bits/alltypes.h" 3 4
typedef struct __locale_struct * locale_t;
# 26 "/usr/local/musl/include/string.h" 2 3 4

void *memcpy (void *restrict, const void *restrict, size_t);
void *memmove (void *, const void *, size_t);
void *memset (void *, int, size_t);
int memcmp (const void *, const void *, size_t);
void *memchr (const void *, int, size_t);

char *strcpy (char *restrict, const char *restrict);
char *strncpy (char *restrict, const char *restrict, size_t);

char *strcat (char *restrict, const char *restrict);
char *strncat (char *restrict, const char *restrict, size_t);

int strcmp (const char *, const char *);
int strncmp (const char *, const char *, size_t);

int strcoll (const char *, const char *);
size_t strxfrm (char *restrict, const char *restrict, size_t);

char *strchr (const char *, int);
char *strrchr (const char *, int);

size_t strcspn (const char *, const char *);
size_t strspn (const char *, const char *);
char *strpbrk (const char *, const char *);
char *strstr (const char *, const char *);
char *strtok (char *restrict, const char *restrict);

size_t strlen (const char *);

char *strerror (int);


# 1 "/usr/local/musl/include/strings.h" 1 3 4
# 11 "/usr/local/musl/include/strings.h" 3 4
# 1 "/usr/local/musl/include/bits/alltypes.h" 1 3 4
# 12 "/usr/local/musl/include/strings.h" 2 3 4




int bcmp (const void *, const void *, size_t);
void bcopy (const void *, void *, size_t);
void bzero (void *, size_t);
char *index (const char *, int);
char *rindex (const char *, int);



int ffs (int);
int ffsl (long);
int ffsll (long long);


int strcasecmp (const char *, const char *);
int strncasecmp (const char *, const char *, size_t);

int strcasecmp_l (const char *, const char *, locale_t);
int strncasecmp_l (const char *, const char *, size_t, locale_t);
# 60 "/usr/local/musl/include/string.h" 2 3 4





char *strtok_r (char *restrict, const char *restrict, char **restrict);
int strerror_r (int, char *, size_t);
char *stpcpy(char *restrict, const char *restrict);
char *stpncpy(char *restrict, const char *restrict, size_t);
size_t strnlen (const char *, size_t);
char *strdup (const char *);
char *strndup (const char *, size_t);
char *strsignal(int);
char *strerror_l (int, locale_t);
int strcoll_l (const char *, const char *, locale_t);
size_t strxfrm_l (char *restrict, const char *restrict, size_t, locale_t);




void *memccpy (void *restrict, const void *restrict, int, size_t);



char *strsep(char **, const char *);
size_t strlcat (char *, const char *, size_t);
size_t strlcpy (char *, const char *, size_t);
void explicit_bzero (void *, size_t);
# 5 "./file.c" 2

# 1 "./file.h" 1
       


# 3 "./file.h"
extern char *filename;
extern char *user_input;

char *read_file(char *path);
# 7 "./file.c" 2
# 1 "./error.h" 1
       

# 1 "./tokenize.h" 1
       

# 1 "/usr/local/musl/include/stdbool.h" 1 3 4
# 4 "./tokenize.h" 2

typedef enum {
  TK_RESERVED,
  TK_RETURN,
  TK_SIZEOF,
  TK_ALIGNOF,
  TK_STRUCT,
  TK_ENUM,
  TK_IF,
  TK_ELSE,
  TK_SWITCH,
  TK_DEFAULT,
  TK_CASE,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_IDENT,
  TK_VOID,
  TK_INT,
  TK_CHAR,
  TK_CONST,
  TK_EXTERN,
  TK_STATIC,
  TK_NUM,
  TK_STR,
  TK_NEWLINE,
  TK_EOF,
} TokenKind;

typedef struct Token Token;
typedef struct StrLiteral StrLiteral;

struct Token {
  TokenKind kind;
  Token *next;
  int val;
  char *str;
  int len;


  StrLiteral *str_literal;
};

struct StrLiteral {
  char *str;
  int len;
  int id;


  StrLiteral *next;
};

extern const char variable_letters[];
extern Token *dummy_token;

extern StrLiteral *str_literals;


# 63 "./tokenize.h" 3 4
_Bool 
# 63 "./tokenize.h"
    equal(Token *token, char *op);

# 64 "./tokenize.h" 3 4
_Bool 
# 64 "./tokenize.h"
    equal_kind(Token *token, TokenKind kind);

# 65 "./tokenize.h" 3 4
_Bool 
# 65 "./tokenize.h"
    consume(Token **rest, Token *token, char *op);
Token *consume_kind(Token **rest, Token *token, TokenKind kind);
void expect(Token **rest, Token *token, char *op);
Token *expect_kind(Token **rest, Token *token, TokenKind kind);
int expect_number(Token **rest, Token *token);

# 70 "./tokenize.h" 3 4
_Bool 
# 70 "./tokenize.h"
    at_eof(Token *token);


# 72 "./tokenize.h" 3 4
_Bool 
# 72 "./tokenize.h"
    is_alnum(char c);

Token *tokenize(char *p);
Token *new_token(TokenKind kind,Token *cur,char *str,int len);

void debug_token(Token *token);
# 4 "./error.h" 2

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);

void not_implemented(const char *msg);
void not_implemented_at(char *loc);
# 8 "./file.c" 2

char *read_file(char *path) {
  FILE *fp = fopen(path, "r");
  if (!fp)
    error("cannot open %s: %s", path, strerror(
# 12 "./file.c" 3 4
                                              (*__errno_location())
# 12 "./file.c"
                                                   ));

  if (fseek(fp, 0, 
# 14 "./file.c" 3 4
                  2
# 14 "./file.c"
                          ) == -1)
    error("%s: fseek: %s", path, strerror(
# 15 "./file.c" 3 4
                                         (*__errno_location())
# 15 "./file.c"
                                              ));
  size_t size = ftell(fp);
  if (fseek(fp, 0, 
# 17 "./file.c" 3 4
                  0
# 17 "./file.c"
                          ) == -1)
    error("%s: fseek: %s", path, strerror(
# 18 "./file.c" 3 4
                                         (*__errno_location())
# 18 "./file.c"
                                              ));

  char *buf = calloc(1, size + 2);
  fread(buf, size, 1, fp);

  if (size == 0 || buf[size - 1] != '\n')
    buf[size++] = '\n';
  buf[size] = '\0';
  fclose(fp);
  return buf;
}
