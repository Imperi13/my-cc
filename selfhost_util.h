#pragma once

#define bool _Bool
#define true 1
#define false 0
#define NULL 0

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

typedef struct _IO_FILE FILE;
typedef int size_t;

extern FILE *stderr;
