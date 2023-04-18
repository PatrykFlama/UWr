# TDD exercise instructions
## Setup
1. Get this repository:
```
git clone https://github.com/heireann/paro2023.git
```
2. Prepare a build directory and generate build scripts with cmake:
```
$ mkdir build && cd build && cmake ..
```

Expect output like:
~~~
$ mkdir build && cd build && cmake ..
-- The C compiler identification is GNU 11.3.0
-- The CXX compiler identification is GNU 11.3.0
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Check for working C compiler: /usr/bin/cc - skipped
-- Detecting C compile features
-- Detecting C compile features - done
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Configuring done
-- Generating done
-- Build files have been written to: /home/user/paro2022/build
~~~
3. Build and run the tests
`make run_tests`    

Expected output:

```
$ make run_tests
-- Configuring done
-- Generating done
-- Build files have been written to: /home/user/paro2022/build
[ 20%] Building CXX object CMakeFiles/bowling_game.dir/src/BowlingGame.cpp.o
[ 40%] Linking CXX static library libbowling_game.a
[ 40%] Built target bowling_game
Consolidate compiler generated dependencies of target tests
[ 60%] Building CXX object CMakeFiles/tests.dir/catch/catch_main.cpp.o
[ 80%] Building CXX object CMakeFiles/tests.dir/src/BowlingGameTests.cpp.o
[100%] Linking CXX executable tests
[100%] Built target tests


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tests is a Catch v2.13.9 host application.
Run with -? for options

-------------------------------------------------------------------------------
failed_succesfully
-------------------------------------------------------------------------------
/home/user/paro2022/src/BowlingGameTests.cpp:3
...............................................................................

/home/user/paro2022/src/BowlingGameTests.cpp:5: FAILED:
  REQUIRE( true == false )

===============================================================================
test cases: 1 | 1 failed
assertions: 1 | 1 failed

make[3]: *** [CMakeFiles/run_tests.dir/build.make:70: CMakeFiles/run_tests] Error 1
make[2]: *** [CMakeFiles/Makefile2:138: CMakeFiles/run_tests.dir/all] Error 2
make[1]: *** [CMakeFiles/Makefile2:145: CMakeFiles/run_tests.dir/rule] Error 2
make: *** [Makefile:150: run_tests] Error 2
```

## The task
1. Write a test in `src/BowlingGameTests.cpp` for not covered scenario and make sure it does not pass. Don't touch the implementation.
```
$ make run_tests
[ 33%] Building CXX object CMakeFiles/tests.dir/src/BowlingGameTests.cpp.o
[ 66%] Linking CXX executable tests
[100%] Built target tests

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tests is a Catch v2.13.9 host application.
Run with -? for options

-------------------------------------------------------------------------------
failed_succesfully
-------------------------------------------------------------------------------
/home/user/paro2022/src/BowlingGameTests.cpp:3
...............................................................................

/home/user/paro2022/src/BowlingGameTests.cpp:5: FAILED:
  REQUIRE( true == false )

===============================================================================
test cases: 6 | 5 passed | 1 failed
assertions: 6 | 5 passed | 1 failed

```

2. Write only enough implementation in `src/BowlingGame.cpp` to pass all the tests. Don't touch the tests.
```
$ make run_tests
Consolidate compiler generated dependencies of target tests
[ 33%] Building CXX object CMakeFiles/tests.dir/src/BowlingGameTests.cpp.o
[ 66%] Linking CXX executable tests
[100%] Built target tests
===============================================================================
All tests passed (6 assertions in 6 test cases)
```

3. Clean up the code while not breaking the tests
```
$ make run_tests
Consolidate compiler generated dependencies of target tests
[ 33%] Building CXX object CMakeFiles/tests.dir/src/BowlingGameTests.cpp.o
[ 66%] Linking CXX executable tests
[100%] Built target tests
===============================================================================
All tests passed (6 assertions in 6 test cases)
```

4. Repeat steps 1-3 until there are no more scenarios not covered by tests

