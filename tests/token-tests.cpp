#include "tests.hpp"
#include "token.hpp"
#include "utils.hpp"

#include <cstring>


void print_token(lexer* t, token tok) {
	i8 buf[tok.size + 1];
	token_get_id(t, tok, buf);
	printf("token:{ ty:%i, id:'%s', row:%lu, col:%lu, size:%lu }\n", tok.type, buf, tok.row, tok.col, tok.size);
}

// TODO: this does test anything, at this stage
// is serves just as a quick way to see if things
// are behaving as expected.
void should_parse_simple_sentence() {
	lexer t;

	i8* buf = copy_str_to_buffer(
		"f :: fn (a:i32, b:i32) -> i32 {}\n"
		"g :: fn (a:i32, b:i32) -> i32 {}"
	);

	printf("%s\n", buf);
	lexer_init(&t, 0, buf, strlen(buf));

	while(!lexer_is_eof(&t)) {
		token tok = lexer_read_token(&t);
		print_token(&t, tok);
	}

	lexer_destroy(&t);
}


int main() {
	TEST(should_parse_simple_sentence);
	return 0;
}
