# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.26

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/linux/.local/lib/python3.8/site-packages/cmake/data/bin/cmake

# The command to remove a file.
RM = /home/linux/.local/lib/python3.8/site-packages/cmake/data/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build

# Include any dependencies generated for this target.
include CMakeFiles/example_leak_fds.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/example_leak_fds.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/example_leak_fds.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/example_leak_fds.dir/flags.make

CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o: CMakeFiles/example_leak_fds.dir/flags.make
CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o: /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/example_leak_fds.cpp
CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o: CMakeFiles/example_leak_fds.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o -MF CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o.d -o CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o -c /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/example_leak_fds.cpp

CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/example_leak_fds.cpp > CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.i

CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/example_leak_fds.cpp -o CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.s

# Object files for target example_leak_fds
example_leak_fds_OBJECTS = \
"CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o"

# External object files for target example_leak_fds
example_leak_fds_EXTERNAL_OBJECTS =

example_leak_fds: CMakeFiles/example_leak_fds.dir/example_leak_fds.cpp.o
example_leak_fds: CMakeFiles/example_leak_fds.dir/build.make
example_leak_fds: CMakeFiles/example_leak_fds.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable example_leak_fds"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/example_leak_fds.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/example_leak_fds.dir/build: example_leak_fds
.PHONY : CMakeFiles/example_leak_fds.dir/build

CMakeFiles/example_leak_fds.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/example_leak_fds.dir/cmake_clean.cmake
.PHONY : CMakeFiles/example_leak_fds.dir/clean

CMakeFiles/example_leak_fds.dir/depend:
	cd /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build /mnt/c/Users/patry/Documents/Programming/UWr/Sem2/PARO/zad2/exercises/build/CMakeFiles/example_leak_fds.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/example_leak_fds.dir/depend
