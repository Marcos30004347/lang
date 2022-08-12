#include "error.hpp"
#include "tests.hpp"
#include "parser.hpp"
#include "utils.hpp"

void print_ast(Parser* p, ASTNode* a, int tabs = 0) {
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
		print_ast(p, ast_manager_get_relative(&p->ast_man, a, a->left), tabs + 4);
	}

	if(a->right) {
		printf("%*c|__", tabs, ' ');
		print_ast(p, ast_manager_get_relative(&p->ast_man, a, a->right), tabs + 4);
	}
} 

	i8* buf;
void should_parse_expressions() {

	// printf("%s\n", buf);

	Parser p;

	parser_init(&p, 0, buf, strlen(buf));
	ASTNode* root = parser_parse(&p);
	// printf("%lu\n", p.ast_man.size);
	// print_ast(&p, root);
	parser_destroy(&p);
}

int main() {
	buf = read_from_file("../examples/state.magic");

	TEST(should_parse_expressions);
	return 0;
}
