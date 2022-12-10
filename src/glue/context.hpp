#include "stack.hpp"

typedef struct continuation_t continuation_t;

struct context_t {
	int is_yielding;
	int yielding_to;
	int is_returning;
	u8* handler_args;
	continuation_t* k;
};

struct continuation_t {
	u8*(*handler)(u8*,u8*, context_t*, u8*);
	int is_owner_of_frame;
	u8* frame;
	u64 retsize;

	continuation_t* next;
};

int is_yielding(context_t* ctx) {
	return ctx->is_yielding;
}

int yielding_to_handler(int h, context_t* ctx) {
	return ctx->yielding_to == h;
}

void set_is_yielding_to(int h, context_t* ctx) {
	ctx->yielding_to = h;
}

void ctx_set_returning(int r, context_t* ctx) {
	ctx->is_returning = r;
}

int ctx_is_yielding_to(int h, context_t* ctx) {
	return ctx->yielding_to == h;
}

void bubble(context_t* ctx, const int retsize, int own_frame, u8* frame, u8*(*handler)(u8*, u8*, context_t*, u8*)) {
	continuation_t* k = (continuation_t*)push(sizeof(continuation_t));

	k->handler = handler;
	k->is_owner_of_frame = own_frame;
	k->frame = frame;
	k->retsize = retsize;
	k->next = ctx->k;
	
	ctx->k = k;
}

u8* ctx_get_handler_args(context_t* ctx) {
	return ctx->handler_args;
}

int ctx_is_returning(context_t* ctx) {
	return ctx->is_returning;
}

u8* resume(u8* arg, context_t* ctx) {
	continuation_t* k = ctx->k;

	if(k->next == NULL) {
		u8* out = push(k->retsize);
		
		k->handler(arg, out, ctx, k->frame);

		return (u8*)allocate(out);
	} else {
	  ctx->k = k->next;

	  buffer_header_t* input = (buffer_header_t*)resume(arg, ctx);

		u8* out = push(k->retsize);

	  k->handler(get_slice_start(input), out, ctx, k->frame);

		deallocate(input);

		return (u8*)allocate(out);
	}
}
