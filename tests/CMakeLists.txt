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

add_executable(ContextTests context/context.cpp)
target_link_libraries(ContextTests magic)
target_include_directories(ContextTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME ContextTests COMMAND ContextTests)

add_executable(StackFrameTests stackframe/stackframe.cpp)
target_link_libraries(StackFrameTests magic)
target_include_directories(StackFrameTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME StackFrameTests COMMAND StackFrameTests)

add_executable(HandlerConversionTests continuations/handler.cpp)
target_link_libraries(HandlerConversionTests magic)
target_include_directories(HandlerConversionTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME HandlerConversionTests COMMAND HandlerConversionTests)

add_executable(CPSConversionTests continuations/continuations.cpp)
target_link_libraries(CPSConversionTests magic)
target_include_directories(CPSConversionTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME CPSConversionTests COMMAND CPSConversionTests)

add_executable(CPSClosureConversionTests continuations/closures.cpp)
target_link_libraries(CPSClosureConversionTests magic)
target_include_directories(CPSClosureConversionTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME CPSClosureConversionTests COMMAND CPSClosureConversionTests)

add_executable(CPSBubblingTests continuations/bubbling.cpp)
target_link_libraries(CPSBubblingTests magic)
target_include_directories(CPSBubblingTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME CPSBubblingTests COMMAND CPSBubblingTests)

