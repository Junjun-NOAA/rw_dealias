#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################

# Jet
#SBATCH --account=rtwrfruc
#SBATCH --partition=kjet,xjet,sjet,ujet,vjet,tjet
##SBATCH --nodes=60
#SBATCH --ntasks=320
#SBATCH --exclusive
#SBATCH --time=03:00:00
#SBATCH --job-name=dealias
#SBATCH --reservation=rrfsens
#SBATCH -o log.dealias
#SBATCH -e err.dealias

module load intel/2022.1.2
module load netcdf
module load impi/2022.1.2

ulimit -a 
set -x

ctime1=$( date +"%Y-%m-%d %H:%M:%S" ) # current time

########################## run data dealiasing ##########################
echo "starting data dealiasing!"

rundir="/lfs5/BMC/wrfruc/jjhu/rw_dealiasing/work/20240606/14"

outdir="/lfs5/BMC/wrfruc/jjhu/rw_dealiasing/data/DealiasedVelocityDPQC"
dealiaspath=$outdir/dealiased/$YYYYMMDD
mkdir -p $dealiaspath

dpqcpath=$outdir/dpqc/$YYYYMMDD
mkdir -p $dpqcpath

bkgpath=$outdir/bkg/$YYYYMMDD
mkdir -p $bkgpath

logpath=$outdir/log/$YYYYMMDD
mkdir -p $logpath

rm $rundir/sub_l2rwbufr_nssl*
rm $rundir/*.log
rm $rundir/stdout_test

srun $rundir/encoderw_dealiasing.exe > $rundir/stdout_test 2>&1

#  run time error check
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

#cat sub_l2rwbufr_nssl_dealiasing_??? > $dealiaspath/l2rwbufr_nssl_h${HH}
#cat sub_l2rwbufr_nssl_bkrw_??? > $bkgpath/l2rwbufr_nssl_bkrw_h${HH}
#cat sub_l2rwbufr_nssl_??? > $dpqcpath/l2rwbufr_nssl_dpqc_h${HH}
#mv stdout_test $logpath/rw_output_h${HH}
#mv log.dealias $logpath/log.dealias.h${HH}
#mv err.dealias $logpath/err.dealias.h${HH}

grep 'processing finished successfully' sub_l2rwbufr_nssl_???.log

echo "finishing data dealiasing!"
########################## run data dealiasing ##########################

ctime2=$( date +"%Y-%m-%d %H:%M:%S" ) # current time

echo $ctime2

echo "job started at: "
echo $ctime1

echo "job finished at: "
echo $ctime2

exit 0
