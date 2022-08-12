
typedef unsigned char  u8;
typedef unsigned int  u32;
typedef unsigned long u64;

#define STACK_SIZE 2097152

typedef struct stack_s {
	u32 sp;
	u8 stack_allocator[STACK_SIZE];
} stack_t;

stack_t _global_stack = { .sp = 0 };

void* _global_stack_push(u64 size) {
	u8* t = _global_stack.stack_allocator + _global_stack.sp;

	_global_stack.sp += size;

	*(u64*)_global_stack.stack_allocator = size;

	_global_stack.sp += sizeof(u64) / sizeof(u8);

	if(_global_stack.sp >= STACK_SIZE) {
		// stack overflow
	}
	
	return t;
}

void _global_stack_pop() {
	u64 s = *((u64*)_global_stack.stack_allocator + _global_stack.sp - sizeof(u64));
	_global_stack.sp -= sizeof(u64) / sizeof(u8);
	_global_stack.sp -= s;
}

typedef struct val_t {
	u64   size;
	void* data;
} val_t;

typedef struct env_t {
	val_t* var;
	struct env_t* next;
} env_t;

typedef int (*lambda_fn) (void* arg, env_t* env);

typedef struct closure_t {} clusure_t;

typedef struct resume_t {} resume_t;

typedef struct context_t {
	u8 is_yielding;
	resume_t* continuation;
} context_t;


int perform_something(int a, context_t* ctx) {
	ctx->is_yielding = 1;
	return 0;
}

int yield_extend(void* a, context_t* ctx);

typedef struct w_join1_env_t {
	int d;
	int tmp1;
} w_join1_env_t;

int w_join1_handler(w_join1_env_t* env, context_t* ctx) {
	int e = env->d - env->tmp1;
	return e;
}

typedef int(*closure_handl)(void*, context_t*);

typedef struct w_join1_closure_t {
	closure_handl han;
	w_join1_env_t env;
} w_join1_closure_t;

w_join1_closure_t allocate_w_join1_closure(int d) {
	w_join1_closure_t t = {
		.env = {
			.d = d
		},
		.han = (closure_handl)w_join1_handler
	};
	
	return t;
};

int w_join2(int a, int b, int tmp0, context_t* ctx) {
	int c = b + tmp0;

	int d = c + 3;

	int tmp1 = perform_something(a, ctx);

	if(ctx->is_yielding) {
		yield_extend(allocate_w_join1_closure(d), ctx);
		return 0;
	}

	return w_join1(d, tmp1, ctx);
}

int w(int a, context_t* ctx) {
	int b = 2*a + a;

	int tmp0 = perform_something(a, ctx);

	if(ctx->is_yielding) {
		yield_extend(alloc_closure(w_join2, a, b), ctx);
		return 0;
	}

	int c = b + tmp0;

	int d = c + 3;

	int tmp1 = perform_something(a, ctx);

	if(ctx->is_yielding) {
		yield_extend(alloc_closure(w_join1, d), ctx);
		return 0;
	}

	int e = d - tmp1;

	return e;
}

void prompt_h(int(*handler)(int, context_t*), int x, context_t* ctx) {
	int r = handler(x, ctx);

	if (ctx->is_yielding) {
		ctx->is_yielding = 0;

		resume_t* k1 = ctx->continuation;
		resume_t* k0 = fork(k0);

		resume(k0, 0);
		resume_t_destroy(k0);

		resume(k1, 1);
		resume_t_destroy(k1);
	}	
}
