#!/bin/ksh 
set -x

cd /lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrDPQCRW/run
cp /lfs4/BMC/wrfruc/jjhu/vr/code/nexradbufr_check/r2bufr_new ./

op=3 # 1 bkg; 2 dpqc; 3 dealias

# 0509, KHTX; 0428 KTLX; 0426 KOAX; 0402 KRLX 
YYYYMMDD=20240426
radar='KOAX'
#run="GrGlLc"
#run="GlGrLc"
#run="GlGr"
#run="Gl"
#run="Adjusted"
run="Adjusted_largerlc2"

filedir='/lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrDPQCRW/run/'

# for background
if [ ${op} -eq 1 ]; then
   dealias="bkrw"
   infilepath=${filedir}/${YYYYMMDD}/${radar}/${run}/sub_l2rwbufr_nssl_${dealias}_001
   outpath=${filedir}/r2bufr_out/${YYYYMMDD}/bkg
fi

# for dpqc
if [ ${op} -eq 2 ]; then
   infilepath=${filedir}/${YYYYMMDD}/${radar}/${run}/sub_l2rwbufr_nssl_001
   outpath=${filedir}/r2bufr_out/${YYYYMMDD}/dpqc
fi


# for dealiasing
if [ ${op} -eq 3 ]; then
   dealias="dealiasing"
   infilepath=${filedir}/${YYYYMMDD}/${radar}/${run}/sub_l2rwbufr_nssl_${dealias}_001
   outpath=${filedir}/r2bufr_out/${YYYYMMDD}/dealias/${run}
fi

if [ ${YYYYMMDD} -eq "20240509" ]; then
   sw=1
   ./r2bufr_new ${infilepath} ${radar} 090249 ${sw} 
   mv sweep* ${outpath}/sw${sw}/

   sw=2
   ./r2bufr_new ${infilepath} ${radar} 090249 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=3
   ./r2bufr_new ${infilepath} ${radar} 090251 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=4
   ./r2bufr_new ${infilepath} ${radar} 090251 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=5
   ./r2bufr_new ${infilepath} ${radar} 090252 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=6
   ./r2bufr_new ${infilepath} ${radar} 090252 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=7
   ./r2bufr_new ${infilepath} ${radar} 090252 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=8
   ./r2bufr_new ${infilepath} ${radar} 090252 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=9
   ./r2bufr_new ${infilepath} ${radar} 090252 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=10
   ./r2bufr_new ${infilepath} ${radar} 090254 ${sw}
   mv sweep* ${outpath}/sw${sw}/
fi

if [ ${YYYYMMDD} -eq "20240426" ]; then
   sw=1
   ./r2bufr_new ${infilepath} ${radar} 262058 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=2
   ./r2bufr_new ${infilepath} ${radar} 262058 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=3
   ./r2bufr_new ${infilepath} ${radar} 262058 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=4
   ./r2bufr_new ${infilepath} ${radar} 262100 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=5
   ./r2bufr_new ${infilepath} ${radar} 262100 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=6
   ./r2bufr_new ${infilepath} ${radar} 262100 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=7
   ./r2bufr_new ${infilepath} ${radar} 262100 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=8
   ./r2bufr_new ${infilepath} ${radar} 262100 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=9
   ./r2bufr_new ${infilepath} ${radar} 262102 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=10
   ./r2bufr_new ${infilepath} ${radar} 262102 ${sw}
   mv sweep* ${outpath}/sw${sw}/

fi

if [ ${YYYYMMDD} -eq "20240428" ]; then
  sw=1
   ./r2bufr_new ${infilepath} ${radar} 280326 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=2
   ./r2bufr_new ${infilepath} ${radar} 280326 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=3
   ./r2bufr_new ${infilepath} ${radar} 280328 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=4
   ./r2bufr_new ${infilepath} ${radar} 280328 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=5
   ./r2bufr_new ${infilepath} ${radar} 280328 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=6
   ./r2bufr_new ${infilepath} ${radar} 280329 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=7
   ./r2bufr_new ${infilepath} ${radar} 280329 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=8
   ./r2bufr_new ${infilepath} ${radar} 280329 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=9
   ./r2bufr_new ${infilepath} ${radar} 280329 ${sw}
   mv sweep* ${outpath}/sw${sw}/

   sw=10
   ./r2bufr_new ${infilepath} ${radar} 280331 ${sw}
   mv sweep* ${outpath}/sw${sw}/

fi

if [ ${YYYYMMDD} -eq "20240402" ]; then
  sw=1
   ./r2bufr_new ${infilepath} ${radar} 021451 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=2
   ./r2bufr_new ${infilepath} ${radar} 021451 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=3
   ./r2bufr_new ${infilepath} ${radar} 021451 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=4
   ./r2bufr_new ${infilepath} ${radar} 021451 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=5
   ./r2bufr_new ${infilepath} ${radar} 021451 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=6
   ./r2bufr_new ${infilepath} ${radar} 021454 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=7
   ./r2bufr_new ${infilepath} ${radar} 021454 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=8
   ./r2bufr_new ${infilepath} ${radar} 021454 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=9
   ./r2bufr_new ${infilepath} ${radar} 021454 ${sw}
   mv sweep* ${outpath}/sw${sw}/

     sw=10
   ./r2bufr_new ${infilepath} ${radar} 021454 ${sw}
   mv sweep* ${outpath}/sw${sw}/
fi
