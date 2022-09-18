#pragma once

typedef struct Vector Vector;

Vector *new_vector(void);
long size_vector(Vector *vec);
void push_back_vector(Vector *vec, void *val);
void *get_vector(Vector *vec, long index);
