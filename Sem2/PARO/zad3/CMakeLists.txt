cmake_minimum_required(VERSION 2.8)
project(paro_stl)

set(CMAKE_CXX_FLAGS "-std=c++20 -Wall -Wfatal-errors")

add_subdirectory(gmock-1.7.0)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
set(LIBS gtest gtest_main)

add_executable(unique unique.cpp)
target_link_libraries(unique ${LIBS})

add_executable(cheap cheap.cpp)
target_link_libraries(cheap ${LIBS})

add_executable(pow2 pow2.cpp)
target_link_libraries(pow2 ${LIBS})
