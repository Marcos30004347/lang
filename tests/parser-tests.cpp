#include "ast.hpp"
#include "tests.hpp"
#include "parser.hpp"
#include "token.hpp"
#include "utils.hpp"
#include <cstring>

void print_ast(parser* p, ast* a, int tabs = 0) {
	printf(" AST:{ kind: %s", ast_kind_to_cstr(a->kind));
	if(a->kind == AST_I32_DECL) {
		printf(",val: %lu", a->tok.buf);
	}
	
 	if(a->kind == AST_SYM_DECL) {
		i8 buff[256];
		token_get_id(&p->lex, a->tok, buff);
		printf(",sym: '%s'", buff);
	}

	printf(" }\n");
	if(a->left) {
		printf("%*c|__", tabs, ' ');
		print_ast(p, a->left, tabs + 4);
	}

	if(a->right) {
		printf("%*c|__", tabs, ' ');
		print_ast(p, a->right, tabs + 4);
	}
} 

void should_parse_expressions() {
	i8* buf = read_from_file("../examples/state.magic");

	printf("%s\n", buf);

	parser p;
	parser_init(&p, 0, buf, strlen(buf));

	ast* root = parser_parse(&p);
	print_ast(&p, root);

	parser_destroy(&p);
	ast_destroy(root);
}

int main() {
	TEST(should_parse_expressions);
	return 0;
}
