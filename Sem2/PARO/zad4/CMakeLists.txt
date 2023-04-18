cmake_minimum_required(VERSION 3.10)
project(paro-tdd C CXX)

add_library(bowling_game src/BowlingGame.cpp)
target_compile_features(bowling_game PUBLIC cxx_std_17)
target_include_directories(bowling_game PUBLIC include)

add_executable(tests catch/catch_main.cpp src/BowlingGameTests.cpp)
target_include_directories(tests PUBLIC catch/include)
target_link_libraries(tests bowling_game)
target_compile_features(tests PUBLIC cxx_std_17)

add_custom_target(run_tests
                  COMMAND tests
                  DEPENDS tests)
