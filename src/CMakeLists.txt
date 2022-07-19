init_llvm()

add_library(pl0-jit-lib lexer.cpp parser.cpp annotation.cpp)
target_link_libraries(pl0-jit-lib PUBLIC project_defaults)
target_add_llvm(pl0-jit-lib "core;native;mcjit")
install(TARGETS pl0-jit-lib EXPORT pl0-jit-lib-Targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

add_executable(pl0-jit-test lexer_test.cpp parser_test.cpp)
target_link_libraries(pl0-jit-test PRIVATE pl0-jit-lib GTest::gtest GTest::gtest_main)
gtest_discover_tests(pl0-jit-test)
install(TARGETS pl0-jit-test EXPORT pl0-jit-test-Targets
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

add_executable(pl0-jit main.cpp)
target_link_libraries(pl0-jit PRIVATE pl0-jit-lib)
install(TARGETS pl0-jit EXPORT pl0-jit-Targets
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
