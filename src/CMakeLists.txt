find_package(fmt REQUIRED)
find_package(Boost REQUIRED COMPONENTS filesystem)
find_package(mio REQUIRED)

add_executable(mx-lib-test lexer_test.cpp)
target_link_libraries(mx-lib-test PRIVATE project_defaults Catch2::Catch2WithMain mio::mio Boost::filesystem fmt::fmt)
catch_discover_tests(mx-lib-test)

add_executable(pl-0 main.cpp)
target_link_libraries(pl-0 PRIVATE project_defaults fmt::fmt)
target_compile_definitions(pl-0 PRIVATE FMT_ENFORCE_COMPILE_STRING)
