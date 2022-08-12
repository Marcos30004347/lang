cmake_minimum_required(VERSION 3.10)

include(CTest)

project(TokenTests)
add_executable(TokenTests token-tests.cpp)
target_link_libraries(TokenTests magic)
target_include_directories(TokenTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME TokenTests COMMAND TokenTests)

project(ParserTests)
add_executable(ParserTests parser-tests.cpp)
target_link_libraries(ParserTests magic)
target_include_directories(ParserTests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME ParserTests COMMAND ParserTests)

