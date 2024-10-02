#!/bin/ksh

module load intel/2022.1.2
module load netcdf
module load impi/2022.1.2

ulimit -a 
set -x

echo "this is a test"

ctime1=$( date +"%Y-%m-%d %H:%M:%S" ) # current time

echo "ctime="
ctime=$CDATE
echo $ctime

ptime=$PDATE
echo "ptime="
echo $ptime

workdir=$WORKDIR
mkdir -p $workdir

outdir=$OUTDIR
mkdir -p  $outdir

dpqcdir=$DPQCDIR
mkdir -p  $dpqcdir

bkrwdir=$BKRWDIR
mkdir -p  $bkrwdir

cd $workdir

########################## get DPQC data list ##########################
echo "starting DPQC data list!"
rwfilepath="/public/data/radar/nssl/AliasedVelocityDPQC"

# get file list
# -30 min
YYYYMMDD=${ptime:0:8}
HH=${ptime:8:2}

MIN="3"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist1

MIN="4"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist2

MIN="5"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist3

# +30 min
YYYYMMDD=${ctime:0:8}
HH=${ctime:8:2}

MIN="0"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist4

MIN="1"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist5

MIN="2"
radarlist=${YYYYMMDD}-${HH}${MIN}???.????.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/????/${radarlist} > rwfilelist6

rm rwfilelist

cat rwfilelist? > rwfilelist

rm rwfilelist?

echo "finishing DPQC data list!"
########################## get DPQC data list ##########################
#
########################## get HRRR background ##########################
echo "starting HRRR background!"

YYYYMMDD=${ctime:0:8}
HH=${ctime:8:2}

hrrrfilepath="/mnt/lfs4/BMC/rtwbl/mhu/wcoss/nco/com/hrrr/prod/hrrr."
hrrrfilepath=$hrrrfilepath$YYYYMMDD/conus
hrrrfile=hrrr.t${HH}z.wrf_inout

echo "finishing HRRR background!"
########################## get HRRR background ##########################


########################## run data dealiasing ##########################
echo "starting data dealiasing!"

rundir="/lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrDPQCRW/workflow"

rm $workdir/sub_l2rwbufr_nssl*
rm $workdir/*.log
rm $workdir/stdout_test
rm $workdir/namelist.input

cp $rundir/namelist.input.template $workdir/namelist.input 
sed -i "s/YYYYMMDDHH/$YYYYMMDD$HH/g" $workdir/namelist.input
sed -i "s/YYYYMMDD/$YYYYMMDD/g" $workdir/namelist.input
sed -i "s/BACKGROUND/"$hrrrfile"/g" $workdir/namelist.input

cp $rundir/radar_station_list.txt $workdir/radar_station_list.txt 
cp $rundir/encoderw_dealiasing.exe $workdir/encoderw_dealiasing.exe
cp $rundir/bufr_radar.table $workdir/bufr_radar.table

srun $workdir/encoderw_dealiasing.exe > $workdir/stdout_test 2>&1

#  run time error check
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

cat sub_l2rwbufr_nssl_dealiasing_??? > $outdir/$YYYYMMDD$HH.dpqc.dealiasing.t${HH}z.bufr
cat sub_l2rwbufr_nssl_bkrw_??? > $bkrwdir/$YYYYMMDD$HH.bkrw.t${HH}z.bufr
cat sub_l2rwbufr_nssl_??? > $dpqcdir/$YYYYMMDD$HH.dpqc.t${HH}z.bufr

#mv stdout_test $logpath/rw_output_h${HH}

echo "finishing data dealiasing!"
########################## run data dealiasing ##########################

ctime2=$( date +"%Y-%m-%d %H:%M:%S" ) # current time

echo $ctime2

echo "job started at: "
echo $ctime1

echo "job finished at: "
echo $ctime2

exit 0
