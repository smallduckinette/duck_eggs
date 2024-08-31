cmake_minimum_required (VERSION 3.30)
project (Control)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_compile_options(-Wall -Wextra -pedantic -Werror -ggdb)

find_package(GTest REQUIRED)
find_package(SDL2 REQUIRED)

include(CTest)

add_executable(
  control
  control.cpp)

target_link_libraries(
  control
  SDL2::SDL2)
