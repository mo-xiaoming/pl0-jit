set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(CheckIPOSupported)
check_ipo_supported(RESULT result OUTPUT output)
if(result)
  message(STATUS "IPO/LTO enabed")
  set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_Release TRUE)
else()
  message(WARNING "IPO/LTO not supported: ${output}")
endif()

add_library(default_compile_features INTERFACE)
target_compile_features(default_compile_features INTERFACE cxx_std_23)

add_library(default_compile_options INTERFACE)
target_compile_options(default_compile_options INTERFACE "$<$<NOT:$<CONFIG:Release>>:-U_FORTIFY_SOURCE;-O0;-ggdb3;-fno-omit-frame-pointer;-fno-inline;-fno-sanitize-recover=all>")
target_compile_options(default_compile_options INTERFACE "$<$<CONFIG:Release>:-march=native>")
target_compile_options(default_compile_options INTERFACE "$<$<CXX_COMPILER_ID:GNU>:-fdiagnostics-color=always>")
target_compile_options(default_compile_options INTERFACE "$<$<CXX_COMPILER_ID:AppleClang,Clang>:-fcolor-diagnostics>")

add_library(default_compile_warnings INTERFACE)
set(CMAKE_CXX_FLAGS_DEBUG "")
target_compile_options(default_compile_warnings INTERFACE "$<$<CONFIG:Release>:-Werror;-Wfatal-errors>")
target_compile_options(default_compile_warnings INTERFACE -Wall -Wextra -Wshadow -Wnon-virtual-dtor -Wold-style-cast -Wcast-align -Wcast-qual -Wunused -Woverloaded-virtual -Wconversion -Wsign-conversion -Wnull-dereference -Wdouble-promotion -Wformat=2 -Wfloat-equal -Wmissing-declarations -Wmissing-include-dirs -Wredundant-decls -Wswitch-enum -Wundef -Wzero-as-null-pointer-constant)
target_compile_options(default_compile_warnings INTERFACE "$<$<CXX_COMPILER_ID:GNU>:-Wmisleading-indentation;-Wduplicated-cond;-Wduplicated-branches;-Wlogical-op;-Wuseless-cast>")

add_library(default_sanitizer_compile_options INTERFACE)
add_library(default_sanitizer_link_options INTERFACE)
target_compile_options(default_sanitizer_compile_options INTERFACE "$<$<CONFIG:ASanAndUBSan>:-fsanitize=address,undefined;-fno-optimize-sibling-calls;-fsanitize=float-divide-by-zero;-fsanitize=float-cast-overflow>")
target_link_options(default_sanitizer_link_options INTERFACE "$<$<CONFIG:ASanAndUBSan>:-fsanitize=address,undefined;-fno-optimize-sibling-calls;-fsanitize=float-divide-by-zero;-fsanitize=float-cast-overflow>")
target_compile_options(default_sanitizer_compile_options INTERFACE "$<$<CXX_COMPILER_ID:AppleClang,Clang>:-fsanitize=local-bounds,float-divide-by-zero,implicit-conversion,nullability,integer>")
target_link_options(default_sanitizer_link_options INTERFACE "$<$<CXX_COMPILER_ID:AppleClang,Clang>:-fsanitize=local-bounds,float-divide-by-zero,implicit-conversion,nullability,integer>")

add_library(project_defaults INTERFACE)
target_link_libraries(project_defaults INTERFACE
  default_compile_features
  default_compile_options
  default_compile_warnings
  default_sanitizer_compile_options
  default_sanitizer_link_options
  )

