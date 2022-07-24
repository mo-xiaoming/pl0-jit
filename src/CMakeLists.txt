init_llvm()

add_library(pl0-jit-lib lexer.cpp parser.cpp annotation.cpp)
target_link_libraries(pl0-jit-lib PUBLIC project_defaults)
target_add_llvm(pl0-jit-lib "core;nativecodegen;mcjit;ipo")

add_executable(pl0-jit-test lexer_test.cpp parser_test.cpp)
target_link_libraries(pl0-jit-test PRIVATE pl0-jit-lib GTest::gtest GTest::gtest_main)
gtest_discover_tests(pl0-jit-test)

add_executable(pl0-jit main.cpp)
target_link_libraries(pl0-jit PRIVATE pl0-jit-lib)

install(TARGETS
  pl0-jit-lib
  pl0-jit
  pl0-jit-test
  DESTINATION ${CMAKE_BINARY_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
