add_library(doctest-main DoctestMain.cpp)
target_link_libraries(doctest-main PUBLIC project_defaults doctest::doctest)

add_library(mx-lib Utils.cpp)
target_link_libraries(mx-lib PUBLIC project_defaults)

add_executable(mx-lib-test UtilsTest.cpp)
target_link_libraries(mx-lib-test PRIVATE doctest-main mx-lib)
# https://github.com/onqtam/doctest/pull/464 requires ADD_LABELS
doctest_discover_tests(mx-lib-test ADD_LABELS 0)

find_package(spdlog REQUIRED)
find_package(Boost REQUIRED)

add_executable(cnt main.cpp)
target_link_libraries(cnt PRIVATE mx-lib spdlog::spdlog Boost::headers)
target_compile_definitions(cnt PRIVATE FMT_ENFORCE_COMPILE_STRING)

install(TARGETS cnt DESTINATION bin)
