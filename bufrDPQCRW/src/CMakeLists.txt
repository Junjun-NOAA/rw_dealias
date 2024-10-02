 
MESSAGE(STATUS "NETCDF dirctory: $ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("$ENV{NETCDF}/include")
INCLUDE_DIRECTORIES("${BASELIBPATH}/src")

set(SRC_ENCODERW EncodeRW_DPQC.f90 module_RW_DPQC.f90 module_radar_station_config.f90 module_RW_dealiasing.f90 module_background.f90 vad.f90)
set_source_files_properties(${SRC_ENCODERW} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_EXECUTABLE (encoderw_dealiasing.exe ${SRC_ENCODERW})
TARGET_LINK_LIBRARIES(encoderw_dealiasing.exe ${BASELIBPATH}/lib/libbase.a ${BASELIBPATH}/lib/libncio.a)
TARGET_LINK_LIBRARIES(encoderw_dealiasing.exe ${NETCDF_LIB} ${BUFR_LIB})


set(SRC_BASE kinds.f90 module_map_utils.f90 module_time.f90)
set_source_files_properties(module_ncio.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
set_source_files_properties(${SRC_BASE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )


