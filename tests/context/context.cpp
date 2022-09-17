#include "tests.hpp"

#include "context.hpp"

void should_create_and_delete_context() {
  Context* context = context_create(NULL);
  context_destroy(context);
}

int main() {
  TEST(should_create_and_delete_context);
}
