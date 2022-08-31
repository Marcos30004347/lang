#include "ast.hpp"
#include "context.hpp"
#include "cps.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "tests.hpp"
#include "transformations.hpp"
#include "utils.hpp"

#include <cstring>
#include <vector>

i8* buf;

void should_closure_convert_ast() {
  Parser p;

  parser_init(&p, 0, buf, strlen(buf));

  AST_Node* root = parser_parse(&p);

  Scope* scope = scope_create(0);

  // print_ast(&p, root);

  closure_conversion(&p, root);

  parser_destroy(&p);
}

// void outro() {
// 	Basic_Block A_bb;
// 	Basic_Block B_bb;
// 	Basic_Block C_bb;
// 	Basic_Block D_bb;
// 	Basic_Block E_bb;

// 	A_bb.first = 1;
// 	B_bb.first = 2;
// 	C_bb.first = 3;
// 	D_bb.first = 4;
// 	E_bb.first = 5;

// 	std::vector<Control_Flow_Graph*> A_childs;
// 	std::vector<Control_Flow_Graph*> B_childs;
// 	std::vector<Control_Flow_Graph*> C_childs;
// 	std::vector<Control_Flow_Graph*> D_childs;
// 	std::vector<Control_Flow_Graph*> E_childs;

// 	Control_Flow_Graph* e = control_flow_graph_create(E_bb, E_childs);
// 	D_childs.push_back(e);
// 	C_childs.push_back(e);
// 	Control_Flow_Graph* d = control_flow_graph_create(D_bb, D_childs);
// 	Control_Flow_Graph* c = control_flow_graph_create(C_bb, C_childs);
// 	B_childs.push_back(c);
// 	B_childs.push_back(d);
// 	Control_Flow_Graph* b = control_flow_graph_create(B_bb, B_childs);
// 	A_childs.push_back(b);
// 	Control_Flow_Graph* a = control_flow_graph_create(A_bb, A_childs);

// 	dominator(a);
// }

int main() {
  buf = read_from_file("../examples/closures.magic");

  TEST(should_closure_convert_ast);

  return 0;
}
