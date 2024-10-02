#!/bin/ksh 
set -x

rwfilepath="/public/data/radar/nssl/AliasedVelocityDPQC"
rwfilepath="/gpfs/fs1/scratch/chunhua/HRRR/2018/data/obs_062018/DPQC/201806260000"
rwfilepath="/lfs4/BMC/wrfruc/jjhu/vr/data/AliasedVelocityDPQC/2024050900_01"
#rwfilepath="/lfs4/BMC/wrfruc/jjhu/vr/data/AliasedVelocityDPQC/2024042803"
rwfilepath="/lfs4/BMC/wrfruc/jjhu/vr/data/AliasedVelocityDPQC/2024042618"
#rwfilepath="/lfs4/BMC/wrfruc/jjhu/vr/data/AliasedVelocityDPQC/2024040212"
#radarname="KRLX"
radarname="????"

YYYYMMDD=20240426
HH=20
MIN=5

radarlist=${YYYYMMDD}-${HH}${MIN}???.${radarname}.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/${radarname}/${radarlist} > rwfilelist1

HH=21
MIN=0
radarlist=${YYYYMMDD}-${HH}${MIN}???.${radarname}.AliasedVelocityDPQC_??.??.netcdf
ls ${rwfilepath}/${radarname}/${radarlist} > rwfilelist2

cat rwfilelist1 rwfilelist2 > rwfilelist

cp rwfilelist1 rwfilelist

rm rwfilelist1 rwfilelist2 
