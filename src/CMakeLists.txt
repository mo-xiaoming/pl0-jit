find_package(fmt REQUIRED)
find_package(Boost REQUIRED COMPONENTS filesystem)
find_package(mio REQUIRED)

add_library(pl0-jit INTERFACE)
target_include_directories(pl0-jit INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include> $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

add_executable(pl0-jit-test lexer_test.cpp)
target_link_libraries(pl0-jit-test PRIVATE project_defaults Catch2::Catch2WithMain mio::mio Boost::filesystem fmt::fmt)
catch_discover_tests(pl0-jit-test)

add_executable(pl-0 main.cpp)
target_link_libraries(pl-0 PRIVATE project_defaults fmt::fmt)
target_compile_definitions(pl-0 PRIVATE FMT_ENFORCE_COMPILE_STRING)

install(TARGETS pl0-jit EXPORT pl0-jit-Targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
