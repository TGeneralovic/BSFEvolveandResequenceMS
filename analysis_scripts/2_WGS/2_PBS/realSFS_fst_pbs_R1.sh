#!/bin/bash

# Provide general information about job

printf "\nWorking Directory: $(pwd)\n"
printf "\nWorking on Node: $(hostname)\n"
printf "\nDate Started: $(date)\n\n"


#module load angsd-0.921-gcc-5.4-ijh2jgu
source activate angsd


realSFS fst index \
/rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/Job_0/saf_0.saf.idx \
/rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/Job_3/saf_3.saf.idx \
/rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/Job_6/saf_6.saf.idx \
-sfs 0-3_S$SLURM_ARRAY_TASK_ID.ml \
-sfs 0-6_S$SLURM_ARRAY_TASK_ID.ml \
-sfs 3-6_S$SLURM_ARRAY_TASK_ID.ml \
-P 512 \
-fstout pbs_S$SLURM_ARRAY_TASK_ID


# Information on time finished
printf "\nDate Finished: $(date)\n\n"

