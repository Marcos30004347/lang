#pragma once

#include "ast.hpp"

#include <unordered_map>
#include <vector>

enum Alias_Type {
	ALIAS_VALUE,
	ALIAS_REFERENCE
};

struct Alias {
	Alias_Type type;
	AST_Id node;
};


// state[n]

// a = b                    state[a] = state[b]
// a = &b                   state[b] = state[b] U {a}, state[a] = state[b]



struct Alias_Graph {
	// an alias is created when an operation like 'left-value = right-value' happen
 
	// left-value -> right-value's
	std::unordered_map<AST_Id, std::vector<Alias> > left_aliases;

	// right-value -> left-value's
	std::unordered_map<AST_Id, std::vector<Alias> > right_aliases;
};

// left = &right
void set_reference_alias(Alias_Graph* graph, AST_Id left, AST_Id right) {
	
}
// left = right
void set_value_reference(Alias_Graph* graph, AST_Id left, AST_Id right) {
	Alias left_alias;

	left_alias.node = right;
	left_alias.type = ALIAS_VALUE;

	graph->left_aliases[left].clear();
	graph->left_aliases[left].push_back(right);

	for(u32 i = 0; i < graph->right_aliases[right].size(); i++) {
	}
}

/*
	a = b
	c = &a
	a = b
*/
