cmake_minimum_required(VERSION 2.8)

set(CMAKE_CXX_FLAGS "-O3 -ggdb -W -Wall -Werror -pedantic")

find_package(Bullet REQUIRED)

include_directories(SYSTEM ${BULLET_INCLUDE_DIRS})
add_executable(bounce bounce.cpp)
target_link_libraries(bounce ${BULLET_LIBRARIES})
