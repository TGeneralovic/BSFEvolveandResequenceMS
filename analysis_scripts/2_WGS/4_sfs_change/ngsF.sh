#!/bin/bash

# Provide general information about job

printf "\nWorking Directory: $(pwd)\n"
printf "\nWorking on Node: $(hostname)\n"
printf "\nDate Started: $(date)\n\n"

gunzip ngsF_0.glf.gz

/home/wz345/software/ngsF/ngsF \
--n_ind 32 \
--n_sites 52669913 \
--n_threads 10 \
--glf ngsF_0.glf \
--out ngsF_out_0

# Information on time finished
printf "\nDate Finished: $(date)\n\n"
