cmake_minimum_required(VERSION 3.18)
project(memory-management-research LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

enable_testing()

add_subdirectory(buddy_allocation)
