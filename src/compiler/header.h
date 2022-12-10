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

i32 ctx_effect_handled(i32 hash, context_t* ctx) {
	ctx->is_yielding = false;
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

		if(k->is_owner_of_frame) {
			tlsf_free(instance, k->frame);
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

		if(k->is_owner_of_frame) {
			tlsf_free(instance, k->frame);
		}

		tlsf_free(instance, k);

		return out;
  }
}
