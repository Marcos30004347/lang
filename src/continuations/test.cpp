#include <cassert>
#include <cstdio>
#include <ctime>

#include "tlsf.h"

#define likely(x)             __builtin_expect(!!(x), 1)
#define unlikely(x)           __builtin_expect(!!(x), 0)
#define DEBUG_FUNCTION_HEADER printf("[%s]:\n", __func__);

typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned char u8;
typedef int i32;

// enum buffer_info_enum {
//   NONE = 0,
//   FREED = 1 << 0,
//   ALLOC = 1 << 1,
// };

// struct buffer_header_t {
//   buffer_header_t* next;
//   buffer_header_t* prev;

//   i32 fragment;
//   u8 flags;
//   u32 size;
// };

// #define STACK_SIZE 1048576 + sizeof(buffer_header_t)

// u8 data[STACK_SIZE];
// u8* sp;

// buffer_header_t* upper_bound;
// buffer_header_t* lower_bound;

// u8* upper_bound_limit;
// u8* lower_bound_limit;

// inline u8* get_slice_start(buffer_header_t* slice) {
//   return (u8*)slice;
// }

// inline u8* get_slice_end(buffer_header_t* slice) {
//   return (u8*)slice + slice->size + sizeof(buffer_header_t);
// }

// inline void init() {
//   buffer_header_t* f = (buffer_header_t*)(data);
//   buffer_header_t* l = (buffer_header_t*)(data + STACK_SIZE - sizeof(buffer_header_t));
//   f->flags |= ALLOC;
//   l->flags |= ALLOC;

//   f->next = l;
//   f->prev = f;
//   f->size = 0;

//   l->next = l;
//   l->prev = f;
//   l->size = 0;

//   f->fragment = -1 * (i32)sizeof(buffer_header_t);
//   l->fragment = 0;

//   sp = data + sizeof(buffer_header_t);

//   upper_bound = f;
//   lower_bound = l;

//   lower_bound_limit = get_slice_start(lower_bound);
//   upper_bound_limit = get_slice_end(upper_bound);
// }

// void debug() {
//   u8* ptr = data;
//   buffer_header_t* upper = upper_bound;

//   while (upper->prev != upper) {
//     upper = upper->prev;
//   }

//   printf("\n");
//   printf("%lu %lu\n", ((u8*)upper_bound_limit - data), ((u8*)lower_bound_limit - data));
//   while (upper->next != upper) {
//     if (upper->flags & ALLOC)
//       printf("  A");
//     else
//       printf("  F");

//     printf("| %7lu |\n", (u64)((u8*)upper - data));
//     if (upper->flags & ALLOC)
//       printf("  A");
//     else
//       printf("  F");
//     printf("|   ...   |\n");
//     if (upper->flags & ALLOC) {
//       if (sp == (u8*)upper + upper->size + sizeof(buffer_header_t)) {
//         printf("->A");
//       } else {
//         printf("  A");
//       }
//     } else
//       printf("  F");
//     printf("| %7lu |\n", (u64)((u8*)upper + upper->size + sizeof(buffer_header_t) - data));

//     if (sp > (u8*)(upper) + upper->size + sizeof(buffer_header_t) && sp <= (u8*)upper->next) {
//       printf("   |   ...   |\n");
//       printf(" ->| %7lu |\n", (u64)(sp - data));
//       printf("   |   ...   |\n");
//     } else {
//       printf("   | %7lu |\n", (u64)((u8*)upper->next - data));
//     }
//     upper = upper->next;
//   }

//   if (upper->flags & ALLOC)
//     printf("  A");
//   else
//     printf("  F");
//   printf("| %7lu |\n", (u64)((u8*)upper - data));
//   if (upper->flags & ALLOC)
//     printf("  A");
//   else
//     printf("  F");
//   printf("|   ...   |\n");
//   if (upper->flags & ALLOC)
//     printf("  A");
//   else
//     printf("  F");
//   printf("| %7lu |\n", (u64)((u8*)upper + upper->size + sizeof(buffer_header_t) - data));
// }

// inline void remove_upper_bound() {
//   if (sp == upper_bound_limit) {
//     sp = (u8*)upper_bound - upper_bound->fragment;
//   }
//   upper_bound = upper_bound->prev;
//   upper_bound_limit = get_slice_end(upper_bound);
//   lower_bound->prev = upper_bound;
//   upper_bound->next = lower_bound;
// }

// inline void remove_lower_bound() {
//   lower_bound = lower_bound->next;

//   lower_bound_limit = get_slice_start(lower_bound);

//   upper_bound->next = lower_bound;
//   lower_bound->prev = upper_bound;
// }

// inline void insert_buffer(buffer_header_t* header) {
//   if ((u8*)header <= sp) {
//     buffer_header_t* bound = upper_bound;

//     while (bound > header) {
//       bound = bound->prev;
//     }

//     header->next = bound->next;
//     header->next->prev = header;
//     header->prev = bound;
//     bound->next = header;

//     if (sp - (u8*)header <= sp - (u8*)upper_bound) {
//       upper_bound = header;
//       upper_bound_limit = (u8*)header + header->size + sizeof(buffer_header_t);
//     }
//   }

//   if ((u8*)header >= sp) {
//     buffer_header_t* bound = lower_bound;

//     while (bound < header) {
//       bound = bound->next;
//     }

//     header->prev = bound->prev;
//     header->prev->next = header;
//     header->next = bound;
//     bound->prev = header;

//     if ((u8*)header - sp <= (u8*)lower_bound - sp) {
//       lower_bound = header;
//       lower_bound_limit = (u8*)header;
//     }
//   }
// }

// inline u8* push(u64 size) {
//   buffer_header_t* header = (buffer_header_t*)(sp);

//   sp = sp + size + sizeof(buffer_header_t);

//   while (sp >= lower_bound_limit && lower_bound->flags & FREED) {
//     remove_lower_bound();
//   }

//   if (unlikely(sp > lower_bound_limit)) {
//     lower_bound->fragment = get_slice_start(lower_bound) - (sp - size);

//     sp = get_slice_end(lower_bound);

//     upper_bound = lower_bound;
//     lower_bound = lower_bound->next;

//     lower_bound_limit = get_slice_start(lower_bound);
//     upper_bound_limit = get_slice_end(upper_bound);

//     return push(size);
//   }

//   header->flags = 0;
//   header->size = size;
//   header->next = 0;
//   header->prev = 0;

// 	// printf("PUSH %lu %lu\n", (u64)((u8*)header - data), (u64)((u8*)header + header->size + sizeof(buffer_header_t) - data));

//   //debug();
//   return sp - size;
// }

// inline void pop(u8* buffer, u32 size) {
//   buffer_header_t* header = (buffer_header_t*)(buffer - sizeof(buffer_header_t));

//   printf("POP %lu %lu\n", (u64)((u8*)header - data), (u64)(buffer + header->size - data));

//   if (header->flags & ALLOC) {
//     printf("Allocated!\n");
//     return;
//   }

//   if (likely(buffer + header->size == sp)) {
//     sp = (u8*)header;

//     while (unlikely(sp <= upper_bound_limit && upper_bound->flags & FREED)) {
//       remove_upper_bound();
//     }

//     if (unlikely(sp < upper_bound_limit)) {
//       sp = get_slice_start(upper_bound) - upper_bound->fragment;

//       lower_bound = upper_bound;
//       upper_bound = upper_bound->prev;

//       lower_bound_limit = get_slice_start(lower_bound);
//       upper_bound_limit = get_slice_end(upper_bound);

//       return pop(buffer, size);
//     }

//     while (unlikely(sp == upper_bound_limit && sp != data + sizeof(buffer_header_t))) {
//       if (upper_bound->flags & FREED) {
//         remove_upper_bound();
//       } else {
//         sp = get_slice_start(upper_bound) - upper_bound->fragment;
//         lower_bound = upper_bound;
//         upper_bound = upper_bound->prev;

//         lower_bound_limit = get_slice_start(lower_bound);
//         upper_bound_limit = get_slice_end(upper_bound);
//       }
//     }

//     debug();
//     return;
//   }

//   printf("Freed!\n");
//   header->flags |= FREED;

//   insert_buffer(header);

//   debug();
// }

// inline u8* allocate(u8* buffer) {

//   buffer_header_t* h = (buffer_header_t*)(buffer - sizeof(buffer_header_t));

//   h->flags = ALLOC;

//   insert_buffer(h);

//   // buffer_header_t* n = lower_bound;
//   // buffer_header_t* p = n->prev;

//   // p->next = h;
//   // h->prev = p;
//   // h->next = n;
//   // n->prev = h;

//   // upper_bound = h;

//   // upper_bound_limit = get_slice_end(h);

//   printf("ALLOCATE %lu %lu\n", (u64)((u8*)h - data), (u64)(buffer + h->size - data));
//   debug();

//   return buffer;
// }

// inline void deallocate(u8* buffer) {
//   buffer_header_t* s = (buffer_header_t*)(buffer - sizeof(buffer_header_t));
//   printf("DEALLOCATE %lu %lu\n", (u64)((u8*)s - data), (u64)(buffer + s->size - data));

//   s->flags = FREED;

//   while (unlikely(sp <= upper_bound_limit && upper_bound->flags & FREED)) {
//     remove_upper_bound();
//   }

//   while (sp >= lower_bound_limit && lower_bound->flags & FREED) {
//     remove_lower_bound();
//   }

//   debug();
// }

// typedef struct continuation_t continuation_t;

// struct context_t {
//   int is_yielding;
//   int yielding_to;
//   int is_returning;
//   u8* handler_args;
//   continuation_t* k;
// };

// struct continuation_t {
//   u8* (*handler)(u8*, u8*, context_t*, u8*);
//   int is_owner_of_frame;
//   u8* frame;
//   u64 retsize;
//   u8 is_prompt;
//   continuation_t* next;
// };

// int is_yielding(context_t* ctx) {
//   return ctx->is_yielding;
// }

// int yielding_to_handler(int h, context_t* ctx) {
//   return ctx->yielding_to == h;
// }

// void set_is_yielding_to(int h, context_t* ctx) {
//   ctx->is_yielding = 1;
//   ctx->yielding_to = h;
// }

// void ctx_set_returning(int r, context_t* ctx) {
//   ctx->is_returning = r;
// }

// int ctx_is_yielding_to(int h, context_t* ctx) {
//   return ctx->is_yielding && ctx->yielding_to == h;
// }

// void bubble(context_t* ctx, const int retsize, int own_frame, u8 is_prompt, u8* frame, u8* (*handler)(u8*, u8*, context_t*, u8*)) {
//   DEBUG_FUNCTION_HEADER
//   u8* buffer = allocate(push(sizeof(continuation_t)));

//   continuation_t* k = (continuation_t*)buffer;

//   k->handler = handler;
//   k->is_owner_of_frame = own_frame;
//   k->frame = frame;
//   k->retsize = retsize;
//   k->next = ctx->k;
//   k->is_prompt = is_prompt;

//   ctx->k = k;
// }

// u8* ctx_get_handler_args(context_t* ctx) {
//   return ctx->handler_args;
// }

// int ctx_is_returning(context_t* ctx) {
//   return ctx->is_returning;
// }

// u8* resume(u8* arg, context_t* ctx) {
//   DEBUG_FUNCTION_HEADER

//   continuation_t* o = ctx->k;
//   continuation_t* k = ctx->k;

//   ctx->is_yielding = 0;
//   ctx->yielding_to = -1;

//   if (k->next == NULL) {
//     u8* out = allocate(push(k->retsize));

//     k->handler(arg, out, ctx, k->frame);

//     return out;

//   } else {
//     ctx->k = k->next;

//     u8* input = resume(arg, ctx);

//     ctx->k = o;

//     u8* out = allocate(push(k->retsize));

//     k->handler(arg, out, ctx, k->frame);

//     deallocate(input);

//     return out;
//   }
// }

// inline void deallocate(u8* data, context_t* ctx) {
//   DEBUG_FUNCTION_HEADER
//   deallocate(data);
// }

// void free_allocations(context_t* ctx) {
//   DEBUG_FUNCTION_HEADER
//   while (ctx->k) {
//     continuation_t* k = ctx->k;

//     ctx->k = k->next;

//     if (k->is_owner_of_frame) {
//       deallocate((u8*)k->frame, ctx);
//     }

//     deallocate((u8*)k, ctx);
//   }
// }

// inline void pop_frame(u8* frame, u64 size) {
//   pop(frame, size);
// }

// inline u8* push_frame(u64 size) {
//   return push(size);
// }

// inline u8* escape_frame(u8* frame) {
//   DEBUG_FUNCTION_HEADER
//   return allocate(frame);
// }

// inline u8* ctx_allocate_args(u64 size, context_t* ctx) {
//   DEBUG_FUNCTION_HEADER
//   ctx->handler_args = allocate(push(size));
//   return ctx->handler_args;
// }



typedef struct continuation_t continuation_t;

struct context_t {
  int is_yielding;
  int yielding_to;
  int is_returning;
  u8* handler_args;
  continuation_t* k;
};

struct continuation_t {
  u8* (*handler)(u8*, u8*, context_t*, u8*);
  int is_owner_of_frame;
  u8* frame;
  u64 retsize;
  u8 is_prompt;
  continuation_t* prev;
  continuation_t* next;
};

int is_yielding(context_t* ctx) {
  return ctx->is_yielding;
}

int yielding_to_handler(int h, context_t* ctx) {
  return ctx->yielding_to == h;
}

void set_is_yielding_to(int h, context_t* ctx) {
  ctx->is_yielding = 1;
  ctx->yielding_to = h;
}

int ctx_is_returning(context_t* ctx) {
  return ctx->is_returning;
}

void ctx_set_returning(int r, context_t* ctx) {
  ctx->is_returning = r;
}

int ctx_is_yielding_to(int h, context_t* ctx) {
  return ctx->is_yielding && ctx->yielding_to == h;
}

u8* ctx_get_handler_args(context_t* ctx) {
  return ctx->handler_args;
}

enum { STATIC_POOL_SIZE = 1 << 20 };
static char s_pool[STATIC_POOL_SIZE];

tlsf_t instance; 

u8* push_frame(u64 size) {
	return (u8*)tlsf_malloc(instance, size);
}

void pop_frame(u8* frame, u64 size) {
	tlsf_free(instance, (void*)frame);
}

void deallocate(u8* frame, context_t*) {
	tlsf_free(instance, (void*)frame);
	return;
}

u8* escape_frame(u8* frame) {
	return frame;
}

void free_allocations(context_t* ctx) {
  DEBUG_FUNCTION_HEADER
  while (ctx->k) {
    continuation_t* k = ctx->k;

    ctx->k = k->next;

    if (k->is_owner_of_frame) {
      deallocate((u8*)k->frame, ctx);
    }

    deallocate((u8*)k, ctx);
  }
}

inline u8* ctx_allocate_args(u64 size, context_t* ctx) {
  DEBUG_FUNCTION_HEADER
  ctx->handler_args = push_frame(size);
  return ctx->handler_args;
}

inline void insert_continuation(continuation_t* k, context_t* ctx) {
	if(ctx->k && ctx->k->prev) {
		ctx->k->prev->next = k;
		k->prev = ctx->k->prev;
		ctx->k->prev = k;
		k->next = ctx->k;
	} else {
		k->next = 0;
	}
	
  ctx->k = k;
}

void bubble(context_t* ctx, const int retsize, int own_frame, u8 is_prompt, u8* frame, u8* (*handler)(u8*, u8*, context_t*, u8*)) {
  DEBUG_FUNCTION_HEADER
  u8* buffer = push_frame(sizeof(continuation_t));

  continuation_t* k = (continuation_t*)buffer;

  k->handler = handler;
  k->is_owner_of_frame = own_frame;
  k->frame = frame;
  k->retsize = retsize;

	insert_continuation(k, ctx);
	
  k->is_prompt = is_prompt;

}

u8* resume(u8* arg, context_t* ctx) {
  DEBUG_FUNCTION_HEADER

  continuation_t* k = ctx->k;

  ctx->is_yielding = 0;
  ctx->yielding_to = -1;

  if (k->next == NULL) {
		ctx->k = 0;
		
    u8* out = (u8*)tlsf_malloc(instance, k->retsize);

		k->handler(arg, out, ctx, k->frame);

		if(is_yielding(ctx)) {
			insert_continuation(k, ctx);
			return NULL;
		}

		tlsf_free(instance, k);
		
    return out;
  } else {
    ctx->k = k->next;

    u8* input = resume(arg, ctx);

		if(is_yielding(ctx)) {
			insert_continuation(k, ctx);

			return NULL;
		}

    u8* out = (u8*)tlsf_malloc(instance, k->retsize);

    k->handler(arg, out, ctx, k->frame);

		tlsf_free(instance, input);
		tlsf_free(instance, k);

		return out;
  }
}



typedef struct args_ask args_ask;
int g_ask_c1_c2(unsigned char* _, context_t* ctx, unsigned char* sp2);
unsigned char* g_ask_c1_handler_c2(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
int g_ask_c1(unsigned char** t1, context_t* ctx, unsigned char* sp2);
unsigned char* g_ask_handler_c1(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
int g_ask(args_ask* args, int* prompt_ret, context_t* ctx, unsigned char* sp1);
int g_c8_cp(unsigned char* _, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8_prompt_read_handler_c3_c4(int* t2, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_handler_c3_handler_c4(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
unsigned char* g_c8_prompt_read_handler_c3(unsigned char* _, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_handler_handler_c3(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
unsigned char* g_c8_prompt_read_handler_c6_c5(unsigned char* _, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_handler_c6_handler_c5(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
unsigned char* g_c8_prompt_read_handler_c6(unsigned char* _, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_handler_handler_c6(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
unsigned char* g_c8_prompt_read_handler(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp1);
int g_c8_c7(unsigned char* _, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8_handler_c7(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
int g_c8(unsigned char* _, context_t* ctx, unsigned char* sp1);
unsigned char* g_handler_c8(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
int f_c0(int* t0, context_t* ctx, unsigned char* sp1);
unsigned char* f_handler_c0(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp);
int ask(int a, context_t* ctx);
int f(context_t* ctx);
int g(context_t* ctx);
struct args_ask {
  int a;
};
int g_ask_c1_c2(unsigned char* _, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((int*)(sp2 + 52)))) = (*(((int*)(sp2 + 48)))) + (*(((int*)(sp1 + 12)))) + ((*((*(((args_ask***)(sp2 + 12)))))))->a;
  (*((*((*(((int***)(sp2 + 20)))))))) = (*(((int*)(sp2 + 52))));
  ctx_set_returning(1, ctx);
  int ret = 0;
  return ret;
}

unsigned char* g_ask_c1_handler_c2(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((int*)(out)))) = g_ask_c1_c2(((unsigned char*)(in)), ctx, sp);
}

int g_ask_c1(unsigned char** t1, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((unsigned char**)(sp2 + 40)))) = (*(t1));
  (*(((int*)(sp2 + 48)))) = (*(((int*)((*(((unsigned char**)(sp2 + 40))))))));
  deallocate((*(((unsigned char**)(sp2 + 40)))), ctx);
  const int yielding2 = is_yielding(ctx);
  if (yielding2) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, g_ask_c1_handler_c2);
    return 0;
  }

  int ret = g_ask_c1_c2(0, ctx, sp2);
  const int yielding3 = is_yielding(ctx);
  if (yielding3) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

unsigned char* g_ask_handler_c1(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((int*)(out)))) = g_ask_c1(((unsigned char**)(in)), ctx, sp);
}

int g_ask(args_ask* args, int* prompt_ret, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp2 = push_frame(56);
  (*(((unsigned char**)(sp2)))) = ((unsigned char*)(sp1));
  (*(((int***)(sp2 + 20)))) = &(prompt_ret);
  (*(((args_ask***)(sp2 + 12)))) = &(args);
  (*(((int*)(sp2 + 28)))) = 1;
  (*(((unsigned char**)(sp2 + 32)))) = ((unsigned char*)(&((*(((int*)(sp2 + 28)))))));

  unsigned char* t1 = resume((*(((unsigned char**)(sp2 + 32)))), ctx);

	// TODO: call free allocations
	free_allocations(ctx);

  const int yielding4 = is_yielding(ctx);
  if (yielding4) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 1, 0, frame_cast, g_ask_handler_c1);
    return 0;
  }

  int ret = g_ask_c1(&(t1), ctx, sp2);
  const int yielding5 = is_yielding(ctx);
  if (yielding5) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 1, 0, frame_cast, 0);
    return 0;
  }

  pop_frame(sp2, 56);
  return ret;
}

int g_c8_cp(unsigned char* _, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp2 = push_frame(20);
  (*(((unsigned char**)(sp2)))) = ((unsigned char*)(sp1));
  (*(((unsigned char***)(sp2 + 12)))) = &(_);
  int ret = 0;
  pop_frame(sp2, 20);
  return ret;
}

unsigned char* g_c8_prompt_read_handler_c3_c4(int* t2, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((int*)((*((*(((unsigned char***)(sp2 + 17)))))))))) = (*(t2));
  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8_prompt_read_handler_c3_handler_c4(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((unsigned char**)(out)))) = g_c8_prompt_read_handler_c3_c4(((int*)(in)), ctx, sp);
}

unsigned char* g_c8_prompt_read_handler_c3(unsigned char* _, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((int (**)(unsigned char*, context_t*, unsigned char*))(sp2 + 29)))) = ((int (*)(unsigned char*, context_t*, unsigned char*))((*((*(((unsigned char***)(sp2 + 9))))))));
  int t2 = (*(((int (**)(unsigned char*, context_t*, unsigned char*))(sp2 + 29))))(0, ctx, sp2);
  const int yielding6 = is_yielding(ctx);
  if (yielding6) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, g_c8_prompt_read_handler_c3_handler_c4);
    return 0;
  }

  unsigned char* ret = g_c8_prompt_read_handler_c3_c4(&(t2), ctx, sp2);
  const int yielding7 = is_yielding(ctx);
  if (yielding7) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

unsigned char* g_c8_prompt_read_handler_handler_c3(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((unsigned char**)(out)))) = g_c8_prompt_read_handler_c3(((unsigned char*)(in)), ctx, sp);
}

unsigned char* g_c8_prompt_read_handler_c6_c5(unsigned char* _, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  unsigned char* ret = g_c8_prompt_read_handler_c3(0, ctx, sp2);
  const int yielding8 = is_yielding(ctx);
  if (yielding8) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

unsigned char* g_c8_prompt_read_handler_c6_handler_c5(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((unsigned char**)(out)))) = g_c8_prompt_read_handler_c6_c5(((unsigned char*)(in)), ctx, sp);
}

unsigned char* g_c8_prompt_read_handler_c6(unsigned char* _, context_t* ctx, unsigned char* sp2) {
  DEBUG_FUNCTION_HEADER
  const unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((int*)(sp2 + 37)))) = ctx_is_returning(ctx);
  if ((*(((int*)(sp2 + 37))))) {
    ctx_set_returning(0, ctx);
    unsigned char* ret = 0;
    return ret;
  }

  unsigned char* ret = g_c8_prompt_read_handler_c6_c5(0, ctx, sp2);
  const int yielding9 = is_yielding(ctx);
  if (yielding9) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

unsigned char* g_c8_prompt_read_handler_handler_c6(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((unsigned char**)(out)))) = g_c8_prompt_read_handler_c6(((unsigned char*)(in)), ctx, sp);
}

unsigned char* g_c8_prompt_read_handler(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp2 = push_frame(41);
  (*(((unsigned char**)(sp2)))) = ((unsigned char*)(sp1));
  (*(((unsigned char***)(sp2 + 17)))) = &(out);
  (*(((unsigned char***)(sp2 + 9)))) = &(in);
  (*(((int*)(sp2 + 25)))) = ctx_is_yielding_to(0, ctx);
  if ((*(((int*)(sp2 + 25))))) {
    (*(((args_ask**)(sp2 + 29)))) = ((args_ask*)(ctx_get_handler_args(ctx)));
    g_ask((*(((args_ask**)(sp2 + 29)))), ((int*)((*((*(((unsigned char***)(sp2 + 17)))))))), ctx, sp1);
		// TODO insert deallocate
		deallocate(*(((u8**)(sp2 + 29))), ctx);
    const int yielding10 = is_yielding(ctx);
    if (yielding10) {
      unsigned char* frame = escape_frame(sp2);
      unsigned char* frame_cast = ((unsigned char*)(frame));
      const int tsize = sizeof(tsize);
      bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read_handler);
      return 0;
    }

    unsigned char* ret = g_c8_prompt_read_handler_c6(0, ctx, sp2);
    const int yielding11 = is_yielding(ctx);
    if (yielding11) {
      unsigned char* frame = escape_frame(sp2);
      unsigned char* frame_cast = ((unsigned char*)(frame));
      const int tsize = sizeof(tsize);
      bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read_handler);
      return 0;
    }

    pop_frame(sp2, 41);
    return ret;
  }

  unsigned char* ret = g_c8_prompt_read_handler_c3(0, ctx, sp2);
  const int yielding12 = is_yielding(ctx);
  if (yielding12) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read_handler);
    return 0;
  }

  pop_frame(sp2, 41);
  return ret;
}

int g_c8_c7(unsigned char* _, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER
  int ret = (*(((int*)(sp1 + 16))));
  return ret;
}

unsigned char* g_c8_handler_c7(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((int*)(out)))) = g_c8_c7(((unsigned char*)(in)), ctx, sp);
}

int g_c8(unsigned char* _, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER(*(((int*)(sp1 + 16)))) = 0;
  g_c8_prompt_read_handler(((unsigned char*)(g_c8_cp)), &((*(((unsigned char*)(sp1 + 16))))), ctx, sp1);
  const int yielding13 = is_yielding(ctx);

  if (yielding13) {
    unsigned char* frame = sp1;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, g_c8_handler_c7);
    return 0;
  }

  int ret = g_c8_c7(0, ctx, sp1);
  const int yielding14 = is_yielding(ctx);
  if (yielding14) {
    unsigned char* frame = sp1;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

unsigned char* g_handler_c8(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((int*)(out)))) = g_c8(((unsigned char*)(in)), ctx, sp);
}

int f_c0(int* t0, context_t* ctx, unsigned char* sp1) {
  DEBUG_FUNCTION_HEADER(*(((int*)(sp1 + 12)))) = (*(t0));
  int ret = (*(((int*)(sp1 + 12))));
  return ret;
}

unsigned char* f_handler_c0(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp) {
  DEBUG_FUNCTION_HEADER(*(((int*)(out)))) = f_c0(((int*)(in)), ctx, sp);
}

int ask(int a, context_t* ctx) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(36);
  (*(((int**)(sp1 + 12)))) = &(a);
  set_is_yielding_to(0, ctx);
  (*(((unsigned char**)(sp1 + 20)))) = ctx_allocate_args(sizeof(args_ask), ctx);
  (*(((args_ask**)(sp1 + 28)))) = ((args_ask*)((*(((unsigned char**)(sp1 + 20))))));
  ((*(((args_ask**)(sp1 + 28)))))->a = (*((*(((int**)(sp1 + 12))))));
  int ret = 0;
  pop_frame(sp1, 36);
  return ret;
}

int f(context_t* ctx) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(16);
  int t0 = ask(4, ctx);
  const int yielding0 = is_yielding(ctx);
  if (yielding0) {
    unsigned char* frame = escape_frame(sp1);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 1, 0, frame_cast, f_handler_c0);
    return 0;
  }

  int ret = f_c0(&(t0), ctx, sp1);
  const int yielding1 = is_yielding(ctx);
  if (yielding1) {
    unsigned char* frame = escape_frame(sp1);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(tsize);
    bubble(ctx, tsize, 1, 0, frame_cast, 0);
    return 0;
  }

  pop_frame(sp1, 16);
  return ret;
}

int g(context_t* ctx) {
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(20);
  (*(((int*)(sp1 + 12)))) = 0;
  f(ctx);

  int ret = g_c8(0, ctx, sp1);
  pop_frame(sp1, 20);
  return ret;
}

// #include <chrono>
// #include <iostream>

int main() {
	instance = tlsf_create_with_pool(s_pool, STATIC_POOL_SIZE);

	// std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
	// void *data = tlsf_malloc(instance, 64);
	// std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();

	// std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::nanoseconds> (end - begin).count() << "[ns]" << std::endl;

	// tlsf_free(instance, data);

  // init();

	// begin = std::chrono::steady_clock::now();
	// u8 *d = push(64);
	// end = std::chrono::steady_clock::now();

	// std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::nanoseconds> (end - begin).count() << "[ns]" << std::endl;
	// return 0;
	
  //init();
	//debug();
  context_t* ctx = new context_t();
  ctx->handler_args = 0;
  ctx->is_returning = 0;
  ctx->is_yielding = 0;
  ctx->yielding_to = -1;
  ctx->k = 0;

  int result = g(ctx);
  printf("result: %i\n", result);
  return 0;
}
