cmake_minimum_required(VERSION 3.12) # needs this for c++20
project(pl0-jit VERSION 0.1.0 DESCRIPTION "jitted pl0 compiler" HOMEPAGE_URL "https://github.com/mo-xiaoming/pl0-jit")

include(GNUInstallDirs)

include(cmake/ProjectDefaults.cmake)

list(APPEND CMAKE_CTEST_ARGUMENTS "--output-on-failure")
find_package(Catch2 3 REQUIRED)
enable_testing()
include(CTest)
include(Catch)

add_subdirectory(src)
