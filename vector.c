#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "vector.h"

#define DEFAULT_CAPACITY 4

struct Vector {
  long size;
  long capacity;
  void **data;
};

Vector *new_vector(void) {
  Vector *vec = calloc(1, sizeof(Vector));
  vec->size = 0;
  vec->capacity = DEFAULT_CAPACITY;
  vec->data = calloc(DEFAULT_CAPACITY, sizeof(void *));
  return vec;
}

long size_vector(Vector *vec) {
  assert(vec, "vec is NULL");
  return vec->size;
}

void push_back_vector(Vector *vec, void *val) {
  assert(vec, "vec is NULL");
  assert(val, "invalid val");

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
  assert(vec, "vec is NULL");
  assert(0 <= index && index < vec->size, "out of bounds");

  return vec->data[index];
}
