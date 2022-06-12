find_package(spdlog REQUIRED)
find_package(Boost REQUIRED COMPONENTS filesystem)
find_package(mio REQUIRED)

add_library(doctest-main DoctestMain.cpp)
target_link_libraries(doctest-main PUBLIC project_defaults_lt doctest::doctest)

add_library(mx-lib Utils.cpp)
target_link_libraries(mx-lib PUBLIC project_defaults_lt mio::mio Boost::filesystem)

add_executable(mx-lib-test UtilsTest.cpp lexer_test.cpp)
target_link_libraries(mx-lib-test PRIVATE project_defaults_lt doctest-main mx-lib)
# https://github.com/onqtam/doctest/pull/464 requires ADD_LABELS
doctest_discover_tests(mx-lib-test ADD_LABELS 0)

add_executable(pl-0 main.cpp)
target_link_libraries(pl-0 PRIVATE project_defaults_lt mx-lib spdlog::spdlog Boost::headers)
target_compile_definitions(pl-0 PRIVATE FMT_ENFORCE_COMPILE_STRING)
