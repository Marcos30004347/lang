cmake_minimum_required(VERSION 3.1O)

project(magic)

add_library(magic
	src/error.cpp
	src/utils.cpp
	src/lexer.cpp
	src/ast.cpp
	src/parser.cpp
	src/crc64.cpp
)

target_include_directories(magic PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")

enable_testing()

add_subdirectory(tests)
