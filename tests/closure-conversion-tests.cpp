#include "ast.hpp"
#include "error.hpp"
#include "tests.hpp"
#include "parser.hpp"
#include "utils.hpp"
#include "transformations.hpp"
#include "cps.hpp"

#include <cstring>

i8* buf;

void should_closure_convert_ast() {
	Parser p;

	parser_init(&p, 0, buf, strlen(buf));

	AST_Node* root = parser_parse(&p);

	CPS_Analysis analysis;
	init_cps_analysis(&analysis);
	
	ast_to_cps(&analysis, &p, root);
	// closure_convert(&p, root);
	print_ast(&p, root);
	
	print_ast_to_program(&p, root);
	
	parser_destroy(&p);
}

int main() {
	buf = read_from_file("../examples/closures.magic");

	TEST(should_closure_convert_ast);
	return 0;
}
