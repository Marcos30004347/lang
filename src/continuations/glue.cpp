

typedef struct continuation_t continuation_t;

typedef struct ctx ctx;
bool               is_yielding(ctx*);
void               compose(ctx*, continuation_t*);
continuation_t*    continuation(ctx*);
char*              arguments(ctx*);

typedef void (*call)(void*, void*, char*, ctx*);

int c0(char* sp, ctx* ctx) {
  return 0;
}

int c1(int* x, char* sp, ctx* ctx) {
  int y = *x;

  return y + 1;
}

void call_c0(void* in, void* out, char* sp, ctx* ctx) {
  *(int*)out = c0(sp, ctx);
}

void call_c1(void* in, void* out, char* sp, ctx* ctx) {
  *(int*)out = c1((int*)in, sp, ctx);
}

struct continuation_t {
  call handler;

  continuation_t* next;
  continuation_t* prev;

  bool is_prompt;
  bool own_frame;

  char* frame;
};

struct ctx {
  continuation_t* k;
};

// This function is the resume function and it resumes the captured continuations from the context
void resume(void* in, ctx* ctx) {
  continuation_t* curr = ctx->k;
  continuation_t* next = ctx->k->next;

  if (next) {
    if (curr->own_frame == 0) {
      next->frame = curr->frame;
    }

    ctx->k = next;

    resume(in, ctx);

    if (is_yielding(ctx)) {
      if (curr->is_prompt) {
        // TODO: handle effect if a handler is prompted

        if (is_yielding(ctx)) {
          return compose(ctx, curr);
        }
      }

      return compose(ctx, curr);
    }

    // Input values get stored at the end of the next continuation buffer
    // Output values get stored at the end of the current buffer
    curr->handler(next + sizeof(continuation_t), curr + sizeof(continuation_t), curr->frame, ctx);

    if (curr->own_frame) {
      delete curr->frame;
    }

    return;
  }

  return curr->handler(in, curr + sizeof(continuation_t), curr->frame, ctx);
}

/*
 This is just an example of a compiled handler that may resume a given computation
 the handler may be equivalent to:

 handler {
   example : () -> i32 {
     resume 0;
   }
 }
*/
void hd_example(ctx* ctx) {
  int t0 = 0;

  continuation_t* curr = ctx->k;

  resume(&t0, ctx);

  if (is_yielding(ctx)) {
    // Need to compose the continuation
  }

  resume(&t0, ctx);

  if (is_yielding(ctx)) {
    // Need to compose the continuation
  }
}

int f() {}

int effect0(int a, int b, continuation_t* c) {}
int effect1(int a, continuation_t* c) {}
int effect2(continuation_t* c) {}

int prompt_teste(ctx* ctx) {
  int result = f();

  if (!is_yielding(ctx)) {
    return result;
  }

  unsigned int effect0_hash = 0;
  unsigned int effect1_hash = 1;
  unsigned int effect2_hash = 2;

  continuation_t* cont = continuation(ctx);
  char*           args = arguments(ctx);

  if (is_yielding(ctx) == effect0_hash) {
    return effect0(*(int*)(args), *(int*)(args + sizeof(int)), cont);
  }

  if (is_yielding(ctx) == effect1_hash) {
    return effect1(*(int*)(args), cont);
  }

  if (is_yielding(ctx) == effect0_hash) {
    return effect2(cont);
  }

  // add normal is yielding code
  return int();
}
