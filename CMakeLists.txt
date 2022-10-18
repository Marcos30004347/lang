cmake_minimum_required(VERSION 3.1O)

project(magic)

add_library(magic
	src/ast/ast_manager.cpp
	src/ast/ast_types.cpp
	src/ast/ast_literals.cpp
	src/ast/ast_declaration.cpp
	src/ast/ast_control_flow.cpp
	src/ast/ast_program_point.cpp
	src/ast/ast_operations.cpp
	src/ast/ast_function.cpp
	src/ast/ast_pointer.cpp
	src/ast/ast_kind.cpp
	src/compiler/compiler.cpp
	src/compiler/symbol_table.cpp
	src/compiler/file_manager.cpp
	src/compiler/symbol_table.cpp
	src/parser/parser.cpp
	src/continuations/continuations.cpp
	src/continuations/closures.cpp
	src/continuations/bubbling.cpp
	src/continuations/handler.cpp
	src/utils.cpp
	src/lexer.cpp
	src/crc64.cpp
	src/context/context.cpp
	src/stackframe/stackframe.cpp
	#src/context.cpp
)

target_include_directories(magic PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")

enable_testing()

add_subdirectory(tests)
