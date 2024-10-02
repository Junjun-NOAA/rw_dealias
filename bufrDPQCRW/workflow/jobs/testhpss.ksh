#!/bin/ksh --login 
#SBATCH --time=00:30:00
#SBATCH --qos=batch
#SBATCH --partition=service
#SBATCH --ntasks=1
#SBATCH --account=nrtrr
#SBATCH --job-name=testhpss
#SBATCH --output=./J-%x.%j.log
set -x

module load hpss
hsi mkdir -p /ESRL/BMC/wrfruc/5year/jjhu/DealiasedVelocityDPQC/20240805

exit 0
