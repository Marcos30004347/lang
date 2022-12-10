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

		/* if(k->is_owner_of_frame) { */
		/* 	tlsf_free(instance, k->frame); */
		/* } */
		
		/* tlsf_free(instance, k); */
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

		/* if(k->is_owner_of_frame) { */
		/* 	tlsf_free(instance, k->frame); */
		/* } */

		/* tlsf_free(instance, k); */

		return out;
  }
}

typedef struct args_ask args_ask;
unsigned char* g_ask_c1_c2(unsigned char* t2, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2);
unsigned char* g_ask_c1(unsigned char* t1, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2);
int g_ask(args_ask* args, int* prompt_ret, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8_prompt_read_c3(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_c5_c4(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read_c5(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2);
unsigned char* g_c8_prompt_read(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8_c7_c6(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8_c7(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1);
unsigned char* g_c8(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1);
unsigned char* f_c0(unsigned char* t0, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1);
int ask(int a, context_t* ctx);
int f(context_t* ctx);
int g(context_t* ctx);
struct args_ask {
  int a;
};
unsigned char* g_ask_c1_c2(unsigned char* t2, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((unsigned char**)(sp2 + 56)))) = (*(((unsigned char**)(t2))));
  (*(((int*)(sp2 + 64)))) = (*(((int*)((*(((unsigned char**)(sp2 + 56))))))));
  (*(((int*)(sp2 + 68)))) = (*(((int*)(sp2 + 44)))) + (*(((int*)(sp1 + 8)))) + (*(((int*)(sp2 + 64)))) + ((*((*(((args_ask***)(sp2 + 8)))))))->a;
  (*((*((*(((int***)(sp2 + 16)))))))) = (*(((int*)(sp2 + 68))));
  ctx_set_returning(1, ctx);
  (*(((int*)(cont_ret)))) = 0;
  (*(((int*)(cont_ret)))) = 0;
  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_ask_c1(unsigned char* t1, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((unsigned char**)(sp2 + 36)))) = (*(((unsigned char**)(t1))));
  (*(((int*)(sp2 + 44)))) = (*(((int*)((*(((unsigned char**)(sp2 + 36))))))));
  (*(((unsigned char**)(sp2 + 48)))) = ((unsigned char*)(&((*(((int*)(sp2 + 24)))))));
  unsigned char* t2 = resume((*(((unsigned char**)(sp2 + 48)))), ctx);
  const int yielding2 = is_yielding(ctx);
  if (yielding2) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(unsigned char*);
    bubble(ctx, tsize, 0, 0, frame_cast, g_ask_c1_c2);
    return 0;
  }

  unsigned char* ret = g_ask_c1_c2((t2), cont_ret, ctx, sp2);
  const int yielding3 = is_yielding(ctx);
  if (yielding3) {
    unsigned char* frame = sp2;
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(unsigned char*);
    bubble(ctx, tsize, 0, 0, frame_cast, 0);
    return 0;
  }

  return ret;
}

int g_ask(args_ask* args, int* prompt_ret, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp2 = push_frame(72);
  (*(((unsigned char**)(sp2)))) = ((unsigned char*)(sp1));
  (*(((int***)(sp2 + 16)))) = &(prompt_ret);
  (*(((args_ask***)(sp2 + 8)))) = &(args);
  (*(((int*)(sp2 + 24)))) = 1;
  (*(((unsigned char**)(sp2 + 28)))) = ((unsigned char*)(&((*(((int*)(sp2 + 24)))))));
  unsigned char* t1 = resume((*(((unsigned char**)(sp2 + 28)))), ctx);
  const int yielding4 = is_yielding(ctx);
  if (yielding4) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(int);
    bubble(ctx, tsize, 1, 0, frame_cast, g_ask_c1);
    return 0;
  }

  int cont_ret = 0;
  g_ask_c1((t1), ((unsigned char*)(&(cont_ret))), ctx, sp2);
  const int yielding5 = is_yielding(ctx);
  if (yielding5) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(int);
    bubble(ctx, tsize, 1, 0, frame_cast, 0);
    return 0;
  }

  int ret = cont_ret;
  pop_frame(sp2, 72);
  return ret;
}

unsigned char* g_c8_prompt_read_c3(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((int*)(sp2 + 28)))) = is_yielding(ctx);
  (*(((unsigned char**)(cont_ret)))) = 0;
  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8_prompt_read_c5_c4(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  (*(((unsigned char**)(cont_ret)))) = 1;
  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8_prompt_read_c5(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp2){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = (*(((unsigned char**)(sp2))));
  ctx_effect_handled(0, ctx);
  const int yielding6 = is_yielding(ctx);
  if (yielding6) {
    return 0;
  }

  (*(((unsigned char**)(cont_ret)))) = g_c8_prompt_read_c5_c4(0, cont_ret, ctx, sp2);
  const int yielding7 = is_yielding(ctx);
  if (yielding7) {
    return 0;
  }

  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8_prompt_read(unsigned char* in, unsigned char* out, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp2 = push_frame(36);
  (*(((unsigned char**)(sp2)))) = ((unsigned char*)(sp1));
  (*(((unsigned char***)(sp2 + 16)))) = &(out);
  (*(((unsigned char***)(sp2 + 8)))) = &(in);
  (*(((int*)(sp2 + 24)))) = ctx_is_yielding_to(0, ctx);
  if ((*(((int*)(sp2 + 24))))) {
    (*(((args_ask**)(sp2 + 28)))) = ((args_ask*)(ctx_get_handler_args(ctx)));
    g_ask((*(((args_ask**)(sp2 + 28)))), ((int*)((*((*(((unsigned char***)(sp2 + 16)))))))), ctx, sp1);
    const int yielding8 = is_yielding(ctx);
    if (yielding8) {
      unsigned char* frame = escape_frame(sp2);
      unsigned char* frame_cast = ((unsigned char*)(frame));
      const int tsize = sizeof(unsigned char*);
      bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read);
      return 0;
    }

    unsigned char* cont_ret = unsigned char*();
    const int yielding9 = is_yielding(ctx);
    if (yielding9) {
      unsigned char* frame = escape_frame(sp2);
      unsigned char* frame_cast = ((unsigned char*)(frame));
      const int tsize = sizeof(unsigned char*);
      bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read);
      return 0;
    }

    g_c8_prompt_read_c5(0, ((unsigned char*)(&(cont_ret))), ctx, sp2);
    const int yielding10 = is_yielding(ctx);
    if (yielding10) {
      unsigned char* frame = escape_frame(sp2);
      unsigned char* frame_cast = ((unsigned char*)(frame));
      const int tsize = sizeof(unsigned char*);
      bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read);
      return 0;
    }

    unsigned char* ret = cont_ret;
    pop_frame(sp2, 36);
    return ret;
  }

  unsigned char* cont_ret = unsigned char*();
  const int yielding11 = is_yielding(ctx);
  if (yielding11) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(unsigned char*);
    bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read);
    return 0;
  }

  g_c8_prompt_read_c3(0, ((unsigned char*)(&(cont_ret))), ctx, sp2);
  const int yielding12 = is_yielding(ctx);
  if (yielding12) {
    unsigned char* frame = escape_frame(sp2);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(unsigned char*);
    bubble(ctx, tsize, 1, 1, frame_cast, g_c8_prompt_read);
    return 0;
  }

  unsigned char* ret = cont_ret;
  pop_frame(sp2, 36);
  return ret;
}

unsigned char* g_c8_c7_c6(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  (*(((int*)(cont_ret)))) = 0;
  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8_c7(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  (*(((int*)(sp1 + 16)))) = ctx_is_returning(ctx);
  if ((*(((int*)(sp1 + 16))))) {
    ctx_set_returning(0, ctx);
    (*(((int*)(cont_ret)))) = (*(((int*)(sp1 + 12))));
    unsigned char* ret = 0;
    return ret;
  }

  (*(((int*)(cont_ret)))) = g_c8_c7_c6(0, cont_ret, ctx, sp1);
  const int yielding13 = is_yielding(ctx);
  if (yielding13) {
    return 0;
  }

  unsigned char* ret = 0;
  return ret;
}

unsigned char* g_c8(unsigned char* _, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  (*(((int*)(sp1 + 12)))) = 0;
  g_c8_prompt_read(0, ((unsigned char*)(&((*(((int*)(sp1 + 12))))))), ctx, sp1);
  const int yielding14 = is_yielding(ctx);
  if (yielding14) {
    return 0;
  }

  (*(((int*)(cont_ret)))) = g_c8_c7(0, cont_ret, ctx, sp1);
  const int yielding15 = is_yielding(ctx);
  if (yielding15) {
    return 0;
  }

  unsigned char* ret = 0;
  return ret;
}

unsigned char* f_c0(unsigned char* t0, unsigned char* cont_ret, context_t* ctx, unsigned char* sp1){
  DEBUG_FUNCTION_HEADER
  (*(((int*)(sp1 + 8)))) = (*(((int*)(t0))));
  (*(((int*)(cont_ret)))) = (*(((int*)(sp1 + 8))));
  unsigned char* ret = 0;
  return ret;
}

int ask(int a, context_t* ctx){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(32);
  (*(((int**)(sp1 + 8)))) = &(a);
  set_is_yielding_to(0, ctx);
  (*(((unsigned char**)(sp1 + 16)))) = ctx_allocate_args(sizeof(args_ask), ctx);
  (*(((args_ask**)(sp1 + 24)))) = ((args_ask*)((*(((unsigned char**)(sp1 + 16))))));
  ((*(((args_ask**)(sp1 + 24)))))->a = (*((*(((int**)(sp1 + 8))))));
  int ret = 0;
  pop_frame(sp1, 32);
  return ret;
}

int f(context_t* ctx){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(12);
  unsigned char t0 = ask(4, ctx);
  const int yielding0 = is_yielding(ctx);
  if (yielding0) {
    unsigned char* frame = escape_frame(sp1);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(int);
    bubble(ctx, tsize, 1, 0, frame_cast, f_c0);
    return 0;
  }

  int cont_ret = 0;
  f_c0(&(t0), ((unsigned char*)(&(cont_ret))), ctx, sp1);
  const int yielding1 = is_yielding(ctx);
  if (yielding1) {
    unsigned char* frame = escape_frame(sp1);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(int);
    bubble(ctx, tsize, 1, 0, frame_cast, 0);
    return 0;
  }

  int ret = cont_ret;
  pop_frame(sp1, 12);
  return ret;
}

int g(context_t* ctx){
  DEBUG_FUNCTION_HEADER
  unsigned char* sp1 = push_frame(20);
  (*(((int*)(sp1 + 8)))) = 0;
  f(ctx);
  int cont_ret = 0;
  g_c8(0, ((unsigned char*)(&(cont_ret))), ctx, sp1);
  const int yielding16 = is_yielding(ctx);
  if (yielding16) {
    unsigned char* frame = escape_frame(sp1);
    unsigned char* frame_cast = ((unsigned char*)(frame));
    const int tsize = sizeof(int);
    bubble(ctx, tsize, 1, 0, frame_cast, 0);
    return 0;
  }

  int ret = cont_ret;
  pop_frame(sp1, 20);
  return ret;
}

#include <chrono>
#include <iostream>

int main() {
  instance = tlsf_create_with_pool(s_pool, STATIC_POOL_SIZE);

  context_t* ctx = new context_t();
  ctx->handler_args = 0;
  ctx->is_returning = 0;
  ctx->is_yielding = 0;
  ctx->yielding_to = -1;
  ctx->k = 0;

  std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
  int result = g(ctx);
  std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();

  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::microseconds >(end - begin).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast< std::chrono::nanoseconds >(end - begin).count() << "[ns]" << std::endl;
  printf("result: %i\n", result);
  return 0;
}
