/*
	This file defines a symbol registry data structure
	It can be used to store symbols and generating unique
	ids for each symbol inserted, in the case of a colision
	The Id of the original symbol inserted is returned. The
	data structure is a AVL Tree and  intenrnally uses CRC64
	to speedup symbols comparisons, only when the computed
	CRC64 hash colides that the actual string of the two symbol
	are compared.
*/

#pragma once

#include "types.hpp"
#include "parser.hpp"
#include "ast.hpp"

typedef struct sym_node sym_node;

#define REGISTRY_NOT_FOUND 0

struct registry {
	sym_node* sym_reg_root;
	
	u64 count;
};

void registry_init(registry*);

void registry_destroy(registry* a);

u64 registry_insert_symbol(registry* t, parser* p, ast* sym);

ast* registry_find(registry* t, parser* p, ast* sym);
u64 registry_find_id(registry* t, parser* p, ast* sym);
