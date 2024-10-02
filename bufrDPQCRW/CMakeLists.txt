set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "mpif90 -f90=ifort")

cmake_minimum_required (VERSION 2.8)

project (EncodeRW_DPQC)

enable_language (Fortran)

MESSAGE( STATUS "Binary directory " ${PROJECT_BINARY_DIR})
MESSAGE( STATUS "SOURCE directory " ${PROJECT_SOURCE_DIR})

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(NETCDF_LIB "-L/apps/netcdf/4.7.0/intel/18.0.5.274/lib/ -lnetcdf -lnetcdff")
SET(BUFR_LIB "/lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrlib/libbufr_v.a")
SET(BASELIBPATH "/lfs4/BMC/wrfruc/jjhu/vr/rw_process/baselib/build")

#SET(Fortran_FLAGS "-O0 -fp-model source -check bounds -g -traceback")
SET(Fortran_FLAGS "-O0 -fp-model source -check all -debug all -g -traceback")

add_subdirectory (${PROJECT_SOURCE_DIR}/src)

