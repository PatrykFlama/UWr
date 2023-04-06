cmake_minimum_required(VERSION 3.18)
project(memory-management-exercises LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

find_program(MEMCHECK valgrind)
set(MEMCHECK_OPTIONS --tool=memcheck --leak-check=full --error-exitcode=1 --errors-for-leak-kinds=all)
set(FDS_OPTIONS -q --tool=none --track-fds=yes)

enable_testing()

option(BUILD_RESOURCES_TESTS "Build tests for kernel-resource-libs (Requires GTest)" OFF)
add_subdirectory(resources)

add_executable(example_leak example_leak.cpp)

add_executable(example_leak_array example_leak_array.cpp)

add_executable(example_leak_exception example_leak_exception.cpp)
target_link_libraries(example_leak_exception PRIVATE kernel-resource)

add_executable(example_leak_raii example_leak_raii.cpp)
target_link_libraries(example_leak_raii PRIVATE kernel-resource)

add_executable(example_leak_fds example_leak_fds.cpp)

add_test(task1-leak ${MEMCHECK} ${MEMCHECK_OPTIONS} ./example_leak)
add_test(task2-leak-array ${MEMCHECK} ${MEMCHECK_OPTIONS} ./example_leak_array)
add_test(task3-leak-exception ${MEMCHECK} ${MEMCHECK_OPTIONS} ./example_leak_exception)
add_test(task4-leak-raii ${MEMCHECK} ${MEMCHECK_OPTIONS} ./example_leak_raii)
add_test(task5-leak-fds ${MEMCHECK} ${FDS_OPTIONS} ./example_leak_fds)

set_tests_properties(task5-leak-fds PROPERTIES FAIL_REGULAR_EXPRESSION "Open file descriptor [0-9]+: log.txt")
