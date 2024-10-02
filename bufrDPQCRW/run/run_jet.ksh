#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################

# Jet
#SBATCH --account=nrtrr
#SBATCH --qos=batch
##SBATCH --partition=service
#SBATCH --partition=kjet,xjet,sjet,ujet,vjet
##SBATCH --nodes=1
#SBATCH --ntasks=80
#SBATCH --exclusive
#SBATCH --time=02:00:00
#SBATCH --job-name=dealias
#SBATCH -o log.dealias

module load intel/2022.1.2
module load netcdf
module load impi/2022.1.2

ulimit -a 
set -x

#
cd /lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrDPQCRW/run
cp /lfs4/BMC/wrfruc/jjhu/vr/rw_process/bufrDPQCRW/build/bin/encoderw_dealiasing.exe .

#------------------------------------------------
#  run 
###################################################

 rm sub_l2rwbufr_nssl*
 rm log.*
 rm *.log
 rm stdout_test

 srun ./encoderw_dealiasing.exe > stdout_test 2>&1

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

# cat sub_l2rwbufr_nssl_??? > l2rwbufr_nssl
grep 'processing finished successfully' sub_l2rwbufr_nssl_???.log
rm *.log
# rm sub_l2rwbufr_nssl_???
# rm sub_l2rwbufr_nssl_???.log
#
exit 0


