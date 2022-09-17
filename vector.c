#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "vector.h"

const long default_capacity = 4;

struct Vector {
  long size;
  long capacity;
  void **data;
};

Vector *new_vector(void) {
  Vector *vec = calloc(1, sizeof(Vector));
  vec->size = 0;
  vec->capacity = default_capacity;
  vec->data = calloc(default_capacity, sizeof(void *));
  return vec;
}

long size_vector(Vector *vec) {
  if (!vec)
    error("invalid vector");
  return vec->size;
}

void push_back_vector(Vector *vec, void *val) {
  if (!vec)
    error("invalid vector");
  if (!val)
    error("cannot append null ptr");

  if (vec->size == vec->capacity) {

    // double-up data buf
    void **new_buf = calloc(2 * vec->capacity, sizeof(void *));
    memcpy(new_buf, vec->data, vec->capacity * sizeof(void *));
    vec->capacity *= 2;

    free(vec->data);
    vec->data = new_buf;
  }

  vec->data[vec->size] = val;
  vec->size++;
}

void *get_vector(Vector *vec, long index) {
  if (!vec)
    error("invalid vector");
  if (index < 0 || index >= vec->size)
    error("out of bounds");

  return vec->data[index];
}
