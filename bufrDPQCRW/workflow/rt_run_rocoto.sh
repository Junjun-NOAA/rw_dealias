#!/bin/bash
source /etc/profile

module load rocoto

rocotorun -w RT_RWDEALIAS_wflow.xml -d RT_RWDEALIAS_wflow.db
