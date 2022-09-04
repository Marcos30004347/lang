#pragma once

#include "ast.hpp"
#include "cps.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include <cassert>
#include <cstdio>
#include <malloc/_malloc.h>
#include <vector>

/*
	Refs:
	https://www.cs.princeton.edu/courses/archive/fall03/cs528/handouts/a%20fast%20algorithm%20for%20finding.pdf
*/

struct Basic_Block {
  AST_Id first;
  AST_Id last;
 
	std::vector<AST_Id> succ;
};

Basic_Block basic_block_create(AST_Node *first, AST_Node *last,
                               std::vector<AST_Node *> brs) {

  Basic_Block bb;

  bb.first = first->id;
  bb.last = last->id;
	
	for(u64 i = 0; i < brs.size(); i++) {
		bb.succ.push_back(brs[i]->id);
	}

	return bb;
}

struct Control_Flow_Graph {
	u64 id;
  Basic_Block bb;
	Control_Flow_Graph* dominator;
	std::vector<Control_Flow_Graph*> succ;
	std::vector<Control_Flow_Graph*> pred;
};

struct CFG_List_Node {
  Control_Flow_Graph *cfg;
  CFG_List_Node *next;
};

CFG_List_Node *cfgs_push(CFG_List_Node *root, Control_Flow_Graph *cfg) {
  CFG_List_Node *list = (CFG_List_Node *)malloc(sizeof(CFG_List_Node));

  list->cfg = cfg;

  if (root) {
    root->next = list;
  }

  return list;
}


void ast_to_ssa_form(Parser *p, AST_Node *node);

Control_Flow_Graph *
control_flow_graph_create(Basic_Block node, std::vector<Control_Flow_Graph*> childs) {
  Control_Flow_Graph *cfg =
      (Control_Flow_Graph *)malloc(sizeof(Control_Flow_Graph));
	
	cfg->id = -1;
	cfg->bb = node;
	for(u64 i = 0; i < childs.size(); i++) {
		cfg->succ.push_back(childs[i]);
	}
	
  return cfg;
}

Control_Flow_Graph *build_control_flow_graph(Parser *p, AST_Node *node,
                                             Control_Flow_Graph *join = 0) {
	if(ast_is_null_node(node)) return NULL;
  assert(node->kind == AST_PROGRAM_POINT);

  AST_Manager *m = &p->ast_man;

  AST_Node *program_point = ast_program_point_get_decl(m, node);

  AST_Node *first = program_point;
  AST_Node *last = ast_node_null(m);

  while (!ast_is_null_node(node)) {
    program_point = ast_program_point_get_decl(m, node);

    if (program_point->kind == AST_BIND_CONSTANT ||
        program_point->kind == AST_BIND_VARIABLE ||
        program_point->kind == AST_OP_BIN_ASSIGN) {
      AST_Node *expr =
          ast_manager_get_relative(m, program_point, program_point->right);

      if (expr->kind == AST_FUNCTION_LITERAL ||
          expr->kind == AST_HANDLER_LITERAL || expr->kind == AST_TYPE_STRUCT) {
        ast_to_ssa_form(p, expr);
      }
    }

    if (program_point->kind == AST_CTRL_FLOW_RETURN) {
      last = program_point;
	 
			std::vector<Control_Flow_Graph*> cfgs;
			std::vector<AST_Node*> branches;
			
      Basic_Block bb = basic_block_create(first, last, branches);

      return control_flow_graph_create(bb, cfgs);
    }

    if (program_point->kind == AST_CTRL_FLOW_IF) {
      last = program_point;

      AST_Node *br_true =
          ast_manager_get_relative(m, program_point, program_point->right);
			
      Control_Flow_Graph *to = build_control_flow_graph(p, br_true);

			std::vector<AST_Node*> branches;
			std::vector<Control_Flow_Graph*> cfgs;

			branches.push_back(br_true);
			cfgs.push_back(to);
	 
      Basic_Block bb = basic_block_create(first, last, branches);

      AST_Node *tail = ast_manager_get_relative(m, node, node->right);

      Control_Flow_Graph *continuation = build_control_flow_graph(p, tail);

      return control_flow_graph_create(bb, cfgs);
    }

    if (program_point->kind == AST_CTRL_FLOW_IF_ELSE) {
      AST_Node *tail = ast_manager_get_relative(m, node, node->right);

      Control_Flow_Graph *continuation = build_control_flow_graph(p, tail);
			
      AST_Node *path = ast_manager_get_relative(m, program_point, program_point->left);

      last = path;

      AST_Node *body_true = ast_manager_get_relative(m, path, path->right);
      Control_Flow_Graph *to_true = build_control_flow_graph(p, body_true, continuation);

      AST_Node *elif =
          ast_manager_get_relative(m, program_point, program_point->right);

      AST_Node *body_false = ast_manager_get_relative(m, elif, elif->right);

      Control_Flow_Graph *to_false =
          build_control_flow_graph(p, body_false, continuation);

      AST_Node *br_true = body_true;
      AST_Node *br_false = body_false;

			std::vector<AST_Node*> branches;
			std::vector<Control_Flow_Graph*> cfgs;

			branches.push_back(br_true);
			branches.push_back(br_false);

			cfgs.push_back(to_true);
			cfgs.push_back(to_false);
			
      Basic_Block bb = basic_block_create(first, last, branches);

      return control_flow_graph_create(bb, cfgs);
    }

		if (program_point->kind == AST_CTRL_FLOW_MATCH) {
      AST_Node *tail = ast_manager_get_relative(m, node, node->right);

      Control_Flow_Graph *continuation = build_control_flow_graph(p, tail);
			
      AST_Node *cases = ast_manager_get_relative(m, program_point, program_point->right);

      last = program_point;

			std::vector<AST_Node*> branches;
			std::vector<Control_Flow_Graph*> cfgs;

			while(!ast_is_null_node(cases)) {
				assert(cases->kind == AST_DECL_ARGS_LIST);

				AST_Node* branch = ast_manager_get_relative(m, cases, cases->left);
				AST_Node* body = ast_manager_get_relative(m, branch, branch->right);
				
				Control_Flow_Graph* to = build_control_flow_graph(p, body, continuation);
				cfgs.push_back(to);
				branches.push_back(branch);
				
				cases = ast_manager_get_relative(m, cases, cases->right);
			}

      Basic_Block bb = basic_block_create(first, last, branches);

      return control_flow_graph_create(bb, cfgs);
    }

    last = node;
    node = ast_program_point_get_tail(m, node);
  }

	std::vector<AST_Node*> branches;
	std::vector<Control_Flow_Graph*> cfgs;
 
  Basic_Block bb = basic_block_create(first, last, branches);

	cfgs.push_back(join);
	
  return control_flow_graph_create(bb, cfgs);
}


struct Dominator_Analysis {
	std::vector<u32> semi;
	std::vector<u32> idom;
	std::vector<u32> label;
	std::vector<u32> size;
	std::vector<u32> vertex;
	std::vector<u32> parent; 
	std::vector<u32> childs;
	std::vector<u32> ancestor;
	std::vector<std::vector<u32> > succ;
	std::vector<std::vector<u32> > pred;
	std::vector<std::vector<u32> > edges;
	std::vector<std::vector<u32> > bucket;
};

struct Control_Flow_Graph_Map {
	u64 n;
	std::vector<Control_Flow_Graph*> nodes;
};


u32 dfs_init(Dominator_Analysis* dfs, Control_Flow_Graph_Map* map, Control_Flow_Graph* cfg) {
	if(cfg == NULL) return 0;
	if(cfg->id != -1) return cfg->id;
										
	cfg->id = map->nodes.size();
	map->nodes.push_back(cfg);

	dfs->semi.push_back(0);
	dfs->vertex.push_back(0);
	dfs->parent.push_back(0);
	
	dfs->pred.push_back(std::vector<u32>());
	dfs->edges.push_back(std::vector<u32>());
	dfs->bucket.push_back(std::vector<u32>());
	
	dfs->size.push_back(0);
	dfs->childs.push_back(0);
	dfs->ancestor.push_back(0);
	dfs->label.push_back(cfg->id);
	dfs->idom.push_back(0);

	dfs->succ.push_back(std::vector<u32>());

	for(u64 i = 0; i < cfg->succ.size(); i++) {
		u64 b = dfs_init(dfs, map, cfg->succ[i]);

		cfg->succ[i]->pred.push_back(cfg);
		dfs->succ[cfg->id].push_back(b);
	}

	return cfg->id;
}

void dfs_rec(Dominator_Analysis* a, Control_Flow_Graph_Map*map, u64 v) {
	a->semi[v] = map->n = map->n + 1;

	a->vertex[map->n] = a->label[v] = v;

	a->size[v] = 1;	
	a->childs[v] = 0;
	a->ancestor[v] = 0;

	for(u64 i = 0; i < a->succ[v].size(); i++) {
		u64 w = a->succ[v][i];
		//printf("to: %u\n", a->nodes[w]->bb.first);

		if (a->semi[w] == 0) {
			a->parent[w] = v;
			dfs_rec(a, map, w);
		}
		
		a->pred[w].push_back(v);
	}
}

u64 dfs(Dominator_Analysis* a, Control_Flow_Graph_Map* map, Control_Flow_Graph* cfg) {
	map->n = 0;

	a->bucket.push_back(std::vector<u32>());
	a->edges.push_back(std::vector<u32>());
	a->pred.push_back(std::vector<u32>());

	a->ancestor.push_back(0);
	a->childs.push_back(0);
	a->label.push_back(0);
	a->parent.push_back(0);
	a->semi.push_back(0);
	a->size.push_back(0);
	a->semi.push_back(0);
	a->idom.push_back(0);
	
	map->nodes.push_back(NULL);
	a->succ.push_back(std::vector<u32>());
	a->vertex.push_back(0);
	
	u32 root = dfs_init(a,map, cfg);
	
	dfs_rec(a, map, root);

	return root;
}


void compress(Dominator_Analysis* a, u64 v) {
	if(a->ancestor[a->ancestor[v]] != 0) {
		compress(a, a->ancestor[v]);
		
		if (a->semi[a->label[a->ancestor[v]]] <= a->semi[a->label[v]]) {
			a->label[v] = a->label[a->ancestor[v]];
		}

		a->ancestor[v] = a->ancestor[a->ancestor[v]];
	}
}
void link(Dominator_Analysis* a, u64 v, u64 w) {
	u64 s = w;
	
	while(a->semi[a->label[w]] <= a->semi[a->label[a->childs[s]]]) {
		if(a->size[s] + a->semi[a->childs[a->childs[s]]] >= 2*a->size[a->childs[s]]) {
			a->parent[a->childs[s]] = s;
			a->childs[s] = a->childs[a->childs[s]];
		}

		else a->size[a->childs[s]] = a->size[s];
	 
		a->parent[s] = a->childs[s];
		s = a->parent[s];
	}
	
	a->label[s] = a->label[w];
	a->size[v] = a->size[v] + a->size[w];

	if(a->size[v] < 2*a->size[w]) {
		u64 t = s;

		s = a->childs[v];
		a->childs[v] = t;
	}

	while(s) {
		a->parent[s] = v;
		s = a->childs[s];
	}
}

u64 eval(Dominator_Analysis* a, u64 v) {
	if(a->ancestor[v] == 0) {
		return v;
	}

	compress(a, v);

	return a->label[v];
}

void compute_dominators(Control_Flow_Graph_Map* map, Control_Flow_Graph* cfg) {
	Dominator_Analysis a;
	
	u64 root = dfs(&a, map, cfg);
	
	// NOTE(Marcos): 1 is the index of the root node, we can ignore it
	for(u64 i = map->n; i >= 2; i--) { // iterate until root
		u64 w = a.vertex[i];

		for(u64 j = 0; j < a.pred[w].size(); j++) {
			u64 v = a.pred[w][j];

			u64 u = eval(&a, v);

			if(a.semi[u] < a.semi[w]) {
				a.semi[w] = a.semi[u];
			}
		}

		a.bucket[a.vertex[a.semi[w]]].push_back(w);

		link(&a, a.parent[w], w);

		while(a.bucket[a.parent[w]].size()) {
			u64 v = a.bucket[a.parent[w]].back();

			a.bucket[a.parent[w]].pop_back();

			u64 u = eval(&a, v);

			if(a.semi[u] < a.semi[v]) {
				a.idom[v] = u;
			} else {
				a.idom[v] = a.semi[v];
			}
		}
	}
	
	for(u64 i = 2; i <= map->n; i++) {
		u64 w = a.vertex[i];
		if(a.idom[w] != a.vertex[a.semi[w]]) {
			a.idom[w] = a.idom[a.idom[w]];
		}
	}

	a.idom[1] = 0;
	map->nodes[a.vertex[1]]->dominator = NULL;
	
	for(u64 i = 2; i < a.vertex.size(); i++) {
		map->nodes[a.vertex[i]]->dominator = map->nodes[a.vertex[a.idom[i]]];
	}

}

struct Dominator_Frontier {
	std::vector<std::vector<u64> > frontier; 
};

void init_dominator_frontier (Dominator_Frontier* df, u64 n) {
	df->frontier = std::vector<std::vector<u64> >(n, std::vector<u64>());
}

void add_path_to_df_rec(Dominator_Frontier* df, Control_Flow_Graph* node, Control_Flow_Graph* prev, Control_Flow_Graph* idom) {
	if(prev == idom) return;

	if(node != prev) {
		df->frontier[node->id].push_back(prev->id);
	}
	
	for(u64 i = 0; i < node->pred.size(); i++) {
		add_path_to_df_rec(df, node, node->pred[i], idom);
	}
}

void compute_dominator_frontier(Dominator_Frontier *df,Control_Flow_Graph_Map* map, Control_Flow_Graph* cfg) {
	init_dominator_frontier(df, map->n);

	for (u64 i = 0; i < map->nodes.size(); i++) {
    Control_Flow_Graph *cfg = map->nodes[i];

    // TODO(marcos): test if this node is a joint point, move to a separete
    // function
    if (cfg->pred.size() >= 2) {
      Control_Flow_Graph *node = cfg;

      for (u64 i = 0; i < node->pred.size(); i++) {
        Control_Flow_Graph *runner = cfg->pred[i];
        Control_Flow_Graph *idom = runner->dominator;

        while (runner != idom) {
          df->frontier[runner->id].push_back(cfg->id);
					runner = runner->dominator;
        }
      }
    }
  }
}


void ast_to_ssa(Parser* p, AST_Node* body) {
  AST_Manager *m = &p->ast_man;

	Control_Flow_Graph *cfg = build_control_flow_graph(p, body);
	Dominator_Frontier df;
	Control_Flow_Graph_Map map;
	
	compute_dominators(&map, cfg);

	compute_dominator_frontier(&df, &map, cfg);

	
}


void ast_to_ssa_form(Parser *p, AST_Node *node) {
  AST_Manager *m = &p->ast_man;

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    node = ast_bind_get_expr(m, node);
  }

  if (node->kind == AST_FUNCTION_LITERAL) {
    AST_Node *body = ast_function_literal_get_body(m, node);

    Control_Flow_Graph *cfg = build_control_flow_graph(p, body);

		u64 n = compute_dominators(cfg);

		// TODO: to_ssa(program_point, cfg);
		return;
  }

	
	
  if (node->kind == AST_HANDLER_LITERAL) {
    AST_Node *decl = ast_manager_get_relative(m, node, node->right);
    return ast_to_ssa_form(p, decl);
  }

  if (node->kind == AST_TYPE_STRUCT) {
    AST_Node *decl = ast_manager_get_relative(m, node, node->right);
    return ast_to_ssa_form(p, decl);
  }

  assert(node->kind == AST_PROGRAM_POINT);

  while (!ast_is_null_node(node)) {
    AST_Node *program_point = ast_program_point_get_decl(m, node);

    ast_to_ssa_form(p, program_point);

    node = ast_program_point_get_tail(m, node);
  }
}
