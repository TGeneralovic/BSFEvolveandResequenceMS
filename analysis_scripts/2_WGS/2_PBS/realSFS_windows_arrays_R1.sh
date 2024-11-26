#!/bin/bash

# Provide general information about job

printf "\nWorking Directory: $(pwd)\n"
printf "\nWorking on Node: $(hostname)\n"
printf "\nDate Started: $(date)\n\n"


#module load angsd-0.921-gcc-5.4-ijh2jgu
source activate angsd


realSFS fst stats2 \
pbs_R1_S$SLURM_ARRAY_TASK_ID.fst.idx \
-win 20000 -step 20000 -type 0 \
> pbs_R1_S$SLURM_ARRAY_TASK_ID.20kb.20kb.idx


# Information on time finished
printf "\nDate Finished: $(date)\n\n"

