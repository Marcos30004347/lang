#include <cassert>
#include <cstdio>

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

typedef unsigned long u64;
typedef unsigned int  u32;
typedef unsigned char u8;
typedef int           i32;

struct slice_t {
  slice_t* next;
  slice_t* prev;

  i32 fragment;
  u32 size;
};

#define STACK_SIZE 1048576 + sizeof(slice_t)

u8  data[STACK_SIZE];
u8* sp;

slice_t* bound_0;
slice_t* bound_1;

u8* bound_0_end;
u8* bound_1_start;

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

  bound_0 = f;
  bound_1 = l;

  bound_1_start = (u8*)bound_1 - bound_1->size;
  bound_0_end   = (u8*)bound_0 + sizeof(slice_t);
}

void debug() {
  printf(
      "[| sp = %lu, u = %lu, d = %lu, b0 = %lu, b1 = %lu, f = %i |]\n",
      (u64)sp - ((u64)data),
      (u64)bound_0_end - ((u64)data),
      (u64)bound_1_start - ((u64)data),
      (u64)bound_0 - (u64)data,
      (u64)bound_1 - (u64)data,
      bound_0->fragment);
}

inline void push(u64 size) {
  sp = sp + size;

  if (unlikely(sp > bound_1_start)) {
    bound_1->fragment = ((u8*)bound_1 - bound_1->size) - (sp - size);

    sp = (u8*)bound_1 + sizeof(slice_t);

    bound_0 = bound_1;
    bound_1 = bound_1->next;

    bound_1_start = (u8*)bound_1 - bound_1->size;
    bound_0_end   = (u8*)bound_0 + sizeof(slice_t);

    return push(size);
  }
}

inline void pop(u32 size) {
  sp = sp - size;

  if (unlikely(sp < bound_0_end)) {
    sp = (u8*)bound_0 - bound_0->size - bound_0->fragment;

    bound_1 = bound_0;
    bound_0 = bound_0->prev;

    bound_1_start = (u8*)bound_1 - bound_1->size;
    bound_0_end   = (u8*)bound_0 + sizeof(slice_t);

    return pop(size);
  }

  if (unlikely(sp == bound_0_end)) {
    sp = (u8*)bound_0 - bound_0->size - bound_0->fragment;

    bound_1 = bound_0;
    bound_0 = bound_0->prev;

    bound_1_start = (u8*)bound_1 - bound_1->size;
    bound_0_end   = (u8*)bound_0 + sizeof(slice_t);
  }
}

inline void freeze(u64 size) {
  slice_t* i = (slice_t*)sp;

  push(sizeof(slice_t));

  i->size = size;

  slice_t* n = bound_1;
  slice_t* p = n->prev;

  p->next = i;
  i->prev = p;
  i->next = n;
  n->prev = i;

  bound_0 = i;

  bound_0_end = (u8*)bound_0 + sizeof(slice_t);
}

inline void unfreeze(u8* b, u64 size) {
  slice_t* i = (slice_t*)(b + size);

  i->prev->next = i->next;
  i->next->prev = i->prev;

  if (i == bound_0) {
    bound_0 = i->prev;
    bound_1 = i->next;

    bound_0_end   = (u8*)bound_0 + sizeof(slice_t);
    bound_1_start = (u8*)bound_1 - bound_1->size;
  }

  if (i == bound_1) {
    bound_0 = i->prev;
    bound_1 = i->next;

    bound_0_end   = (u8*)bound_0 + sizeof(slice_t);
    bound_1_start = (u8*)bound_1 - bound_1->size;
  }
}

int main() {
  init();

  u8* start = sp;

  debug();

  push(16);

  debug();

  assert(sp == start + 16);

  u8* slice = sp;

  push(16);

  debug();

  assert(sp == start + 32);

  freeze(16);

  debug();

  assert(sp == sizeof(slice_t) + start + 32);

  push(32);

  debug();

  assert(sp == sizeof(slice_t) + start + 64);

  pop(32);

  debug();

  assert(sp == start + 16);

  pop(16);

  debug();

  assert(sp == start);

  push(32);

  debug();

  pop(32);

  debug();

  unfreeze(slice, 16);

  debug();
}
