#include "ast.hpp"
#include "error.hpp"
#include "tests.hpp"
#include "parser.hpp"
#include "utils.hpp"
#include "transformations.hpp"

#include <cstring>

i8* buf;

void should_closure_convert_ast() {
	Parser p;

	parser_init(&p, 0, buf, strlen(buf));

	AST_Node* root = parser_parse(&p);

	// closure_convert(&p, root);

	print_ast(&p, root);
	
	parser_destroy(&p);
}

int main() {
	buf = read_from_file("../examples/closures.magic");

	TEST(should_closure_convert_ast);
	return 0;
}
