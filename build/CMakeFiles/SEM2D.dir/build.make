# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.21

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
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/mus/Documents/Modelling/MyProjects/SEM2D

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/mus/Documents/Modelling/MyProjects/SEM2D/build

# Include any dependencies generated for this target.
include CMakeFiles/SEM2D.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/SEM2D.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/SEM2D.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/SEM2D.dir/flags.make

CMakeFiles/SEM2D.dir/main.f90.o: CMakeFiles/SEM2D.dir/flags.make
CMakeFiles/SEM2D.dir/main.f90.o: ../main.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/SEM2D.dir/main.f90.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/mus/Documents/Modelling/MyProjects/SEM2D/main.f90 -o CMakeFiles/SEM2D.dir/main.f90.o

CMakeFiles/SEM2D.dir/main.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/SEM2D.dir/main.f90.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/mus/Documents/Modelling/MyProjects/SEM2D/main.f90 > CMakeFiles/SEM2D.dir/main.f90.i

CMakeFiles/SEM2D.dir/main.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/SEM2D.dir/main.f90.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/mus/Documents/Modelling/MyProjects/SEM2D/main.f90 -o CMakeFiles/SEM2D.dir/main.f90.s

CMakeFiles/SEM2D.dir/buildmesh.f90.o: CMakeFiles/SEM2D.dir/flags.make
CMakeFiles/SEM2D.dir/buildmesh.f90.o: ../buildmesh.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object CMakeFiles/SEM2D.dir/buildmesh.f90.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/mus/Documents/Modelling/MyProjects/SEM2D/buildmesh.f90 -o CMakeFiles/SEM2D.dir/buildmesh.f90.o

CMakeFiles/SEM2D.dir/buildmesh.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/SEM2D.dir/buildmesh.f90.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/mus/Documents/Modelling/MyProjects/SEM2D/buildmesh.f90 > CMakeFiles/SEM2D.dir/buildmesh.f90.i

CMakeFiles/SEM2D.dir/buildmesh.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/SEM2D.dir/buildmesh.f90.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/mus/Documents/Modelling/MyProjects/SEM2D/buildmesh.f90 -o CMakeFiles/SEM2D.dir/buildmesh.f90.s

CMakeFiles/SEM2D.dir/shapefunc2D.f90.o: CMakeFiles/SEM2D.dir/flags.make
CMakeFiles/SEM2D.dir/shapefunc2D.f90.o: ../shapefunc2D.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object CMakeFiles/SEM2D.dir/shapefunc2D.f90.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/mus/Documents/Modelling/MyProjects/SEM2D/shapefunc2D.f90 -o CMakeFiles/SEM2D.dir/shapefunc2D.f90.o

CMakeFiles/SEM2D.dir/shapefunc2D.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/SEM2D.dir/shapefunc2D.f90.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/mus/Documents/Modelling/MyProjects/SEM2D/shapefunc2D.f90 > CMakeFiles/SEM2D.dir/shapefunc2D.f90.i

CMakeFiles/SEM2D.dir/shapefunc2D.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/SEM2D.dir/shapefunc2D.f90.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/mus/Documents/Modelling/MyProjects/SEM2D/shapefunc2D.f90 -o CMakeFiles/SEM2D.dir/shapefunc2D.f90.s

CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o: CMakeFiles/SEM2D.dir/flags.make
CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o: ../connectivity_matrix.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/mus/Documents/Modelling/MyProjects/SEM2D/connectivity_matrix.f90 -o CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o

CMakeFiles/SEM2D.dir/connectivity_matrix.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/SEM2D.dir/connectivity_matrix.f90.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/mus/Documents/Modelling/MyProjects/SEM2D/connectivity_matrix.f90 > CMakeFiles/SEM2D.dir/connectivity_matrix.f90.i

CMakeFiles/SEM2D.dir/connectivity_matrix.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/SEM2D.dir/connectivity_matrix.f90.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/mus/Documents/Modelling/MyProjects/SEM2D/connectivity_matrix.f90 -o CMakeFiles/SEM2D.dir/connectivity_matrix.f90.s

CMakeFiles/SEM2D.dir/gll.f90.o: CMakeFiles/SEM2D.dir/flags.make
CMakeFiles/SEM2D.dir/gll.f90.o: ../gll.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object CMakeFiles/SEM2D.dir/gll.f90.o"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/mus/Documents/Modelling/MyProjects/SEM2D/gll.f90 -o CMakeFiles/SEM2D.dir/gll.f90.o

CMakeFiles/SEM2D.dir/gll.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/SEM2D.dir/gll.f90.i"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/mus/Documents/Modelling/MyProjects/SEM2D/gll.f90 > CMakeFiles/SEM2D.dir/gll.f90.i

CMakeFiles/SEM2D.dir/gll.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/SEM2D.dir/gll.f90.s"
	/usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/mus/Documents/Modelling/MyProjects/SEM2D/gll.f90 -o CMakeFiles/SEM2D.dir/gll.f90.s

# Object files for target SEM2D
SEM2D_OBJECTS = \
"CMakeFiles/SEM2D.dir/main.f90.o" \
"CMakeFiles/SEM2D.dir/buildmesh.f90.o" \
"CMakeFiles/SEM2D.dir/shapefunc2D.f90.o" \
"CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o" \
"CMakeFiles/SEM2D.dir/gll.f90.o"

# External object files for target SEM2D
SEM2D_EXTERNAL_OBJECTS =

SEM2D: CMakeFiles/SEM2D.dir/main.f90.o
SEM2D: CMakeFiles/SEM2D.dir/buildmesh.f90.o
SEM2D: CMakeFiles/SEM2D.dir/shapefunc2D.f90.o
SEM2D: CMakeFiles/SEM2D.dir/connectivity_matrix.f90.o
SEM2D: CMakeFiles/SEM2D.dir/gll.f90.o
SEM2D: CMakeFiles/SEM2D.dir/build.make
SEM2D: CMakeFiles/SEM2D.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Linking Fortran executable SEM2D"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/SEM2D.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/SEM2D.dir/build: SEM2D
.PHONY : CMakeFiles/SEM2D.dir/build

CMakeFiles/SEM2D.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/SEM2D.dir/cmake_clean.cmake
.PHONY : CMakeFiles/SEM2D.dir/clean

CMakeFiles/SEM2D.dir/depend:
	cd /home/mus/Documents/Modelling/MyProjects/SEM2D/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/mus/Documents/Modelling/MyProjects/SEM2D /home/mus/Documents/Modelling/MyProjects/SEM2D /home/mus/Documents/Modelling/MyProjects/SEM2D/build /home/mus/Documents/Modelling/MyProjects/SEM2D/build /home/mus/Documents/Modelling/MyProjects/SEM2D/build/CMakeFiles/SEM2D.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/SEM2D.dir/depend

