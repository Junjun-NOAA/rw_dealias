1.Compile:
   Before compiling the src, it is required to compile baselib first.
   The compiling should use cmake and make

   e.g. compiling on jet:
   cd to the bufrDPQCRW directory
   module load intel/2022.1.2
   module load netcdf/4.7.0
   module load impi/2022.1.2 
   edit CMakeLists.txt, set the path for NETCDF_LIB,BUFR_LIB and BASELIBPATH
   mkdir build
   cd build/
   cmake ../
   make
   when the encoderw_dealiasing.exe is generated under bin, the compling is successful. 


2.Run:
   If a foreacst RW is used to do dealiasing, a HRRR forecast should be copied to the run directroy.

   files needed:
   (a) namelist.input: setting the right cycle time and pointing to the HRRR forecast file
   (b) rwfilelist: listing the radar files

3.Output:
  output files are in BUFR format 
  sub_l2rwbufr_nssl_*,  before dealiasing
  sub_l2rwbufr_nssl_bkrw_*, background rw bufr file, when if_save_bkrw=.true. in namelist.input
  sub_l2rwbufr_nssl_dealiasing_*, after dealiasing
  * is the node number

4. The GSI needs to turn off VAD QC.


