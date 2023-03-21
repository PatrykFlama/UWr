add_library(logger STATIC logger.cpp)
target_include_directories(logger PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_library(buddy-allocation Heap.cpp heap_print.cpp heap_allocation.cpp heap_deallocation.cpp)
target_link_libraries(buddy-allocation PRIVATE logger)
target_include_directories(buddy-allocation PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(buddy main.cpp)
target_link_libraries(buddy PRIVATE buddy-allocation)

find_package(GTest)

if(NOT GTest_FOUND)
    message("Cannot find Google Test Framework!")
else()
    add_executable(buddy_allocation_ut tests_allocation.cpp)
    target_link_libraries(buddy_allocation_ut PRIVATE buddy-allocation GTest::gtest_main)
    add_test(NAME buddy_allocation_ut COMMAND buddy_allocation_ut)

    add_executable(buddy_deallocation_ut tests_deallocation.cpp)
    target_link_libraries(buddy_deallocation_ut PRIVATE buddy-allocation GTest::gtest_main)
    add_test(NAME buddy_deallocation_ut COMMAND buddy_deallocation_ut)
endif()
