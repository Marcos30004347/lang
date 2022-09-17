cmake_minimum_required(VERSION 3.10)

include(CTest)

project(LexerTests)
add_executable(LexerTests lexer-tests.cpp)
target_link_libraries(LexerTests magic)
target_include_directories(LexerTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME LexerTests COMMAND LexerTests)

project(SymbolTableTests)
add_executable(SymbolTableTests compiler/symbol_table.cpp)
target_link_libraries(SymbolTableTests magic)
target_include_directories(SymbolTableTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME SymbolTableTests COMMAND SymbolTableTests)

project(ParserTests)
add_executable(ParserTests parser/parser.cpp)
target_link_libraries(ParserTests magic)
target_include_directories(ParserTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME ParserTests COMMAND ParserTests)


# project(ParserTests)
# add_executable(ParserTests parser-tests.cpp)
# target_link_libraries(ParserTests magic)
# target_include_directories(ParserTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
# add_test(NAME ParserTests COMMAND ParserTests)

# project(ClosConvTests)
# add_executable(ClosConvTests closure-conversion-tests.cpp)
# target_link_libraries(ClosConvTests magic)
# target_include_directories(ClosConvTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
# add_test(NAME ClosConvTests COMMAND ClosConvTests)

