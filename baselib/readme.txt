   The compiling should use cmake and make

   e.g. compiling on jet:
   cd to the baselib directory
   module load intel/2022.1.2
   module load netcdf/4.7.0
   module load impi/2022.1.2
   edit CMakeLists.txt, set the path for NETCDF_LIB 
   mkdir build
   cd build/
   cmake ../
   make
   when the testlib.exe is generated under bin, and libbase.a and libncio.a exist under lib, the compling is successful.
