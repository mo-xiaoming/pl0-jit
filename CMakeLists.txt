cmake_minimum_required(VERSION 3.22)
project(cpp-vcpkg-template)

include(cmake/ProjectDefaults.cmake)

list(APPEND CMAKE_CTEST_ARGUMENTS "--output-on-failure")
find_package(doctest REQUIRED)
enable_testing()
include(CTest)
include(doctest)

add_subdirectory(src)
