

#include <cstddef>
#include <string.h>
#include <stdio.h>
typedef char i8;
typedef int i32;
typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned long u64;

#define ARCH_64_BITS 1


#if ARCH_64_BITS
const u32 FL_INDEX_MAX = 40;
const u32 ALIGNMENT_LOG2 = 3;
#else
const u32 FL_INDEX_MAX = 30;
const u32 ALIGNMENT_LOG2 = 2;
#endif

const u32 SPLIT_SIZE_THRESHOLD = 1024;
const u32 SL_INDEX_COUNT_LOG2 = 5;
const u32 FL_INDEX_SHIFT = SL_INDEX_COUNT_LOG2 + ALIGNMENT_LOG2;
const u32 SL_INDEX_COUNT = 1 << SL_INDEX_COUNT_LOG2;
const u32 FL_INDEX_COUNT = FL_INDEX_MAX - FL_INDEX_SHIFT + 1;
const u32 SMALL_BLOCK_SIZE = 1 << FL_INDEX_SHIFT;
const u32 BLOCK_FREE_BITMASK = 1;
const u32 BLOCK_PREV_FREE_BITMASK = 2;

#define cast(t, e) ((t)(e))

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

// u32 ffs(u32 x) {
// 	u32 r = 1;
// 	if(!x) { return 0; }
// 	if(!(x & 0xffff)) { x >>= 16; r += 16; }
// 	if(!(x & 0xff))   { x >>=  8; r +=  8; }
// 	if(!(x & 0xf))    { x >>=  4; r +=  4; }
// 	if(!(x & 3))      { x >>=  2; r +=  2; }
// 	if(!(x & 1))      { x >>=  1; r +=  1; }
// 	return r;
// }

// u32 fls(u32 x) {
// 	u32 r = 32;
// 	if(!x) { return 0; }
// 	if(!(x & 0xffff0000)) { x <<= 16; r -= 16; }
// 	if(!(x & 0xff000000)) { x <<=  8; r -=  8; }
// 	if(!(x & 0xf0000000)) { x <<=  4; r -=  4; }
// 	if(!(x & 0xc0000000)) { x <<=  2; r -=  2; }
// 	if(!(x & 0x80000000)) { x <<=  1; r -=  1; }
// 	return r;
// }

struct block_t {
	block_t * phys; // previous physical block
	u32 size; // size of this block excluding header
	block_t * next; // next free block
	block_t * prev; // prev free block
};

struct allocator_t {
	u32 fl_bitmap;
	u32 sl_bitmap[FL_INDEX_COUNT];
	block_t *blocks[FL_INDEX_COUNT][SL_INDEX_COUNT]; // free lists
};

inline u8* offset_buffer(u8* b) {
	return b + 8; // offset buffer to ignore first 8 bytes (phys member) of block_t
}

inline u32 block_size(const block_t * b) {
	return b->size & ~(BLOCK_FREE_BITMASK | BLOCK_PREV_FREE_BITMASK);
}

inline void block_set_size(block_t * b, u32 size) {
	b->size = size | (b->size & (BLOCK_FREE_BITMASK | BLOCK_PREV_FREE_BITMASK));
}

inline void block_set_free(block_t * b) {
	b->size |= BLOCK_FREE_BITMASK;
}

inline void block_set_prev_free(block_t * b) {
	b->size |= BLOCK_PREV_FREE_BITMASK;
}

inline i32 block_get_free(const block_t * b) {
	return cast(i32, b->size & BLOCK_FREE_BITMASK);
}

inline void block_set_prev_used(block_t * b) {
	b->size &= ~BLOCK_PREV_FREE_BITMASK;
}

inline void block_set_used(block_t* b) {
	b->size &= ~BLOCK_FREE_BITMASK;
}

inline u8* block_to_ptr(block_t* b) {
	return cast(u8*, b) + sizeof(block_t*) + sizeof(u32);
}

inline block_t* ptr_to_block(u8* b) {
	return cast(block_t*, b - sizeof(block_t*) - sizeof(u32));
}

inline block_t* offset_to_block(u8* ptr, u32 size) {
	return cast(block_t*, ptr + size - sizeof(block_t*));
}

inline block_t* block_next(block_t* b) {
	return offset_to_block(cast(u8*, b), block_size(b));
}

inline block_t* block_link_next(block_t* b) {
	block_t* n = block_next(b);
	n->phys = b;
	return n;
}

inline void block_mask_as_free(block_t * b) {
	block_t* next = block_link_next(b);
	block_set_prev_free(next);
	block_set_free(b);
}

inline void block_mask_as_used(block_t * b) {
	block_t* next = block_next(b);
	block_set_prev_used(next);
	block_set_used(b);
}

inline void mapping_insert(u32 r, i32 * fl, i32 * sl) {
	i32 fli, sli;
	if(r < SMALL_BLOCK_SIZE) { // store small blocks in first list
		fli = 0;
		sli = r / (SMALL_BLOCK_SIZE / SL_INDEX_COUNT);
	} else {
		fli = fls(r);
		printf("------ %u %i %i\n", r, fli,  r >> (fli - SL_INDEX_COUNT_LOG2));
		sli = cast(i32, r >> (fli - SL_INDEX_COUNT_LOG2)) ^ (1 << SL_INDEX_COUNT_LOG2);
		fli = fli - (FL_INDEX_SHIFT - 1);
	}
	
	*fl = fli;
	*sl = sli;
}

inline void mapping_search(u32 * r, i32 * fl, i32 * sl) {
	if(*r >= SMALL_BLOCK_SIZE) { // round size up
		*r = *r + (1 << (fls(*r) - SL_INDEX_COUNT_LOG2)) - 1;
	} 

	mapping_insert(*r, fl, sl);
}

inline void insert_free_block(allocator_t * a, block_t * b, i32 fl, i32 sl) {
	block_t * c = a->blocks[fl][sl];

	b->next = c;
	b->prev = 0;

	if(c) {
		c->prev = b;
	}
	printf("%i %i\n", fl, sl);
	a->blocks[fl][sl] = b;
	
	a->fl_bitmap |= (1U << fl);
	a->sl_bitmap[fl] |= (1U << sl);
}

inline block_t* find_suitable_block(allocator_t* a, i32 * fl, i32 * sl) {
	u32 bitmap_tmp = a->sl_bitmap[*fl] & (0xFFFFFFFF << *sl);

	u32 non_empty_sl;
	u32 non_empty_fl;

	if (bitmap_tmp) {
		non_empty_sl = ffs(bitmap_tmp);
		non_empty_fl = *fl;
	} else {
		bitmap_tmp = a->fl_bitmap & (0xFFFFFFFF << (*fl + 1));
		// bitmam_tmp == 0 -> no memory
		non_empty_fl = ffs(bitmap_tmp);
		non_empty_sl = ffs(a->sl_bitmap[non_empty_fl]);
	}

	return a->blocks[non_empty_fl][non_empty_sl];
}

inline void remove_head(allocator_t* a, block_t*b, u32 fl, u32 sl) {
	block_t* prev = b->prev;
	block_t* next = b->next;
	
	next->prev = prev;
	prev->next = next;

	a->blocks[fl][sl] = next;

	if(next == 0) {
		a->sl_bitmap[fl] &= ~(1 << sl);

		if(a->sl_bitmap[fl]) {
			a->fl_bitmap &= ~(1 << fl);
		}
	}
}

inline block_t* split(block_t* b, u32 r) {
	block_t* remaining = offset_to_block(block_to_ptr(b), r);
	u32 remaining_size = block_size(b) - (r + sizeof(block_t*));
	block_set_size(remaining, remaining_size);
	block_set_size(b, r);
	block_set_free(remaining);
	block_mask_as_free(remaining);
	return remaining;
}

inline u8* malloc(allocator_t* a, u32 r) {
	i32 fl, sl;
	mapping_search(&r, &fl, &sl);
	block_t* free_block = find_suitable_block(a, &fl, &sl);

	if(!free_block) {
		// error
	}

	remove_head(a, free_block, fl, sl);

	if(block_size(free_block) - r > SPLIT_SIZE_THRESHOLD) {
		block_t* remaining = split(free_block, r);

		block_link_next(free_block);
		block_set_prev_free(remaining);
		
		mapping_insert(block_size(remaining), &fl, &sl);
		insert_free_block(a, remaining, fl, sl);
	}
	
	block_mask_as_used(free_block);
	return block_to_ptr(free_block);
}

inline void block_insert(allocator_t * a, block_t * b) {
	i32 fl, sl;
	mapping_insert(block_size(b), &fl, &sl);
	insert_free_block(a, b, fl, sl);
}

allocator_t* allocator_create(u8* mem, u32 size) {
	allocator_t* a = cast(allocator_t*, mem);

	a->fl_bitmap = 0;

	printf("%i %i\n", FL_INDEX_COUNT, SL_INDEX_COUNT);

	for(u32 i = 0; i < FL_INDEX_COUNT; ++i) {
		a->sl_bitmap[i] = 0;

		for(u32 j = 0; j < SL_INDEX_COUNT; ++j) {
			a->blocks[i][j] = 0;
		}
	}

	size -= sizeof(allocator_t);
	mem += sizeof(allocator_t);

	block_t* b = cast(block_t*, offset_to_block(mem, 0));
	
	block_set_size(b, size);
	block_set_free(b);
	block_set_prev_used(b);
	block_insert(a, b);

	block_t* n = block_link_next(b);
	
	block_set_size(n, 0);
	block_set_used(n);
	block_set_prev_free(n);

	return a;
} 
