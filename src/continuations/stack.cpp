#include <cassert>
#include <cstdio>

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned char u8;
typedef int i32;

enum slice_info_t {
  NONE = 0,
  FREED = 1 << 0,
};

struct slice_t {
  slice_t* next;
  slice_t* prev;
  i32 fragment;
  u32 size;
  u8 flags;
};

#define STACK_SIZE 1048576 + sizeof(slice_t)

u8 data[STACK_SIZE];
u8* sp;

slice_t* upper_bound;
slice_t* lower_bound;

u8* upper_bound_limit;
u8* lower_bound_limit;

inline u8* get_slice_start(slice_t* slice) {
  return (u8*)slice - slice->size;
}

inline u8* get_slice_end(slice_t* slice) {
  return (u8*)slice + sizeof(slice_t);
}

inline void init() {
  slice_t* f = (slice_t*)(data);
  slice_t* l = (slice_t*)(data + STACK_SIZE - sizeof(slice_t));

  f->next = l;
  f->prev = f;
  f->size = 0;

  l->next = l;
  l->prev = f;
  l->size = 0;

  f->fragment = -1 * (i32)sizeof(slice_t);
  l->fragment = 0;

  sp = data + sizeof(slice_t);

  upper_bound = f;
  lower_bound = l;

  lower_bound_limit = get_slice_start(lower_bound);
  upper_bound_limit = get_slice_end(upper_bound);
}

void debug() {
  printf(
      "[| sp = %lu, upper = %lu, lower = %lu, frag = %i |]\n",
      (u64)sp - ((u64)data),
      (u64)upper_bound_limit - (u64)data,
      (u64)lower_bound_limit - (u64)data,
      upper_bound->fragment);
}

inline void remove_upper_bound() {
  upper_bound = upper_bound->prev;
  upper_bound_limit = get_slice_end(upper_bound);
  lower_bound->prev = upper_bound;
}

inline void remove_lower_bound() {
  lower_bound = lower_bound->next;
  lower_bound_limit = get_slice_start(lower_bound);
  upper_bound->next = lower_bound;
}

inline u8* push(u64 size) {
  sp = sp + size;

  while (sp >= lower_bound_limit && lower_bound->flags & FREED) {
    remove_lower_bound();
  }

  if (unlikely(sp > lower_bound_limit)) {
    lower_bound->fragment = get_slice_start(lower_bound) - (sp - size);

    sp = get_slice_end(lower_bound);

    upper_bound = lower_bound;
    lower_bound = lower_bound->next;

    lower_bound_limit = get_slice_start(lower_bound);
    upper_bound_limit = get_slice_end(upper_bound);

    return push(size);
  }

  // debug();

  return sp - size;
}

inline void pop(u32 size) {
  sp = sp - size;

  while (sp <= upper_bound_limit && upper_bound->flags & FREED) {
    remove_upper_bound();
  }

  if (unlikely(sp < upper_bound_limit)) {
    sp = get_slice_start(upper_bound) - upper_bound->fragment;

    lower_bound = upper_bound;
    upper_bound = upper_bound->prev;

    lower_bound_limit = get_slice_start(lower_bound);
    upper_bound_limit = get_slice_end(upper_bound);

    return pop(size);
  }

  while (unlikely(sp == upper_bound_limit && sp != data + sizeof(slice_t))) {
    sp = get_slice_start(upper_bound) - upper_bound->fragment;

    lower_bound = upper_bound;
    upper_bound = upper_bound->prev;

    lower_bound_limit = get_slice_start(lower_bound);
    upper_bound_limit = get_slice_end(upper_bound);
  }

  // debug();
}

inline slice_t* allocate(u8* buffer) {
  slice_t* i = (slice_t*)push(sizeof(slice_t));

  i->flags = NONE;

  i->size = (u32)((u8*)i - (u8*)buffer);

  slice_t* n = lower_bound;
  slice_t* p = n->prev;

  p->next = i;
  i->prev = p;
  i->next = n;
  n->prev = i;

  upper_bound = i;

  upper_bound_limit = get_slice_end(i);

  return i;
}

inline void deallocate(slice_t* s) {
  s->flags |= FREED;

  if (s == upper_bound) {
    remove_upper_bound();
    pop(0);
  }

  if (s == lower_bound) {
    remove_lower_bound();
    push(0);
  }
}

int use0() {
  u8* start = sp;

  push(16);

  u8* ptr = sp;

  push(16);

	// slice_t* slice = allocate(ptr);

  push(32);

  pop(32);

  pop(16);

  push(32);

  pop(32);

  //deallocate(slice);

  return 0;
}

#include <stdlib.h>

void* a() {
  return alloca(16);
}

void* b() {
  return alloca(16);
}

void* c() {
  return alloca(32);
}

void* d() {
  return alloca(32);
}

unsigned long use1() {
  unsigned long z = 0;
  z += (unsigned long)a();
  z += (unsigned long)b();
  z += (unsigned long)c();
  z += (unsigned long)d();
  return z;
}

#include <chrono>
#include <iostream>

int main() {
  init();

  std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
  use0();
  std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::microseconds >(end - begin).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::nanoseconds >(end - begin).count() << "[ns]" << std::endl;
  begin = std::chrono::steady_clock::now();
  use1();
  end = std::chrono::steady_clock::now();

  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::microseconds >(end - begin).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::nanoseconds >(end - begin).count() << "[ns]" << std::endl;
}
