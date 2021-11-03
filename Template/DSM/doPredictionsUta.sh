#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/lw-rowra/work/3_Land_suitability/0_Working/Uta/Roper/Scripts/doPredictionsUta.R $SLURM_ARRAY_TASK_ID sal2_2w sal2.mod.2w.rds 20 F