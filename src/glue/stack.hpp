#include <cassert>
#include <cstdio>

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned char u8;
typedef int i32;

enum buffer_info_enum {
  NONE = 0,
  FREED = 1 << 0,
};

struct buffer_header_t {
  buffer_header_t* next;
  buffer_header_t* prev;
  i32 fragment;
  u32 size;
  u8 flags;
};

#define STACK_SIZE 1048576 + sizeof(slice_t)

u8 data[STACK_SIZE];
u8* sp;

buffer_header_t* upper_bound;
buffer_header_t* lower_bound;

u8* upper_bound_limit;
u8* lower_bound_limit;

inline u8* get_slice_start(buffer_header_t* slice) {
  return (u8*)slice - slice->size;
}

inline u8* get_slice_end(buffer_header_t* slice) {
  return (u8*)slice + sizeof(buffer_header_t);
}

inline void init() {
  buffer_header_t* f = (buffer_header_t*)(data);
  buffer_header_t* l = (buffer_header_t*)(data + STACK_SIZE - sizeof(buffer_header_t));

  f->next = l;
  f->prev = f;
  f->size = 0;

  l->next = l;
  l->prev = f;
  l->size = 0;

  f->fragment = -1 * (i32)sizeof(buffer_header_t);
  l->fragment = 0;

  sp = data + sizeof(buffer_header_t);

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

	while(sp >= lower_bound_limit && lower_bound->flags & FREED) {
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

  debug();
	
	return sp - size;
}

inline void pop(u32 size) {
  sp = sp - size;

	while(sp <= upper_bound_limit && upper_bound->flags & FREED) {
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

  while (unlikely(sp == upper_bound_limit && sp != data + sizeof(buffer_header_t))) {
     sp = get_slice_start(upper_bound) - upper_bound->fragment;

     lower_bound = upper_bound;
     upper_bound = upper_bound->prev;

     lower_bound_limit = get_slice_start(lower_bound);
     upper_bound_limit = get_slice_end(upper_bound);
  }

  debug();
}

inline buffer_header_t* allocate(u8* buffer) {
  buffer_header_t* i = (buffer_header_t*)push(sizeof(buffer_header_t));

	i->flags = NONE;
	
	i->size = (u32)((u8*)i - (u8*)buffer);

  buffer_header_t* n = lower_bound;
  buffer_header_t* p = n->prev;

  p->next = i;
  i->prev = p;
  i->next = n;
  n->prev = i;

  upper_bound = i;

  upper_bound_limit = get_slice_end(i);
	
	return i;
}

inline void deallocate(buffer_header_t* s) {
  s->flags |= FREED;

	if(s == upper_bound) {
		remove_upper_bound();
		pop(0);
	}

	if(s == lower_bound) {
		remove_lower_bound();
		push(0);
	}
}

