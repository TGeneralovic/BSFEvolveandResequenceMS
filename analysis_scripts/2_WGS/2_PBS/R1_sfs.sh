#!/bin/bash


# Provide general information about job

printf "\nWorking Directory: $(pwd)\n"
printf "\nWorking on Node: $(hostname)\n"
printf "\nDate Started: $(date)\n\n"


source activate angsd


for r in $(cat regions_R1_rest.txt)
do
realSFS \
/rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/Job_$SLURM_ARRAY_TASK_ID/saf_$SLURM_ARRAY_TASK_ID.saf.idx \
-r ${r} \
-P 512 \
> /rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/SFS_change/R1/$SLURM_ARRAY_TASK_ID.${r}.ml
done

# Information on time finished
printf "\nDate Finished: $(date)\n\n"

