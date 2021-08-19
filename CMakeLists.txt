cmake_minimum_required(VERSION 3.21)
project(SEM2D Fortran)


enable_language(Fortran)
add_executable(SEM2D main.f90 buildmesh.f90  shapefunc2D.f90 connectivity_matrix.f90 gll.f90)


