#include "allocator.hpp"



int main() {
	u8 mem[2 << 20];
	printf("%lu\n", (2 << 20) - sizeof(allocator_t));
	allocator_t* a = allocator_create(mem, 2 << 20);

	//	u8* buffer = malloc(a, 256);

	//	block_t* b = ptr_to_block(buffer);

	// printf("size = %lu\n", block_size(b));
}
