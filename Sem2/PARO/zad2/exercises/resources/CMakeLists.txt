cmake_minimum_required(VERSION 3.18)
project(kernel-resource-lib)

add_library(kernel-resource src/kernel_resource.cpp)
target_include_directories(kernel-resource PUBLIC include)

option(BUILD_RESOURCES_TESTS "Build tests for kernel-resource-libs (Requires GTest)" ON)
if(BUILD_RESOURCES_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
