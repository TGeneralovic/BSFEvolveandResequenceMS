#!/bin/bash

# 1. Open sfs files for all chromosomes and combine them into one single file.
cd /rds/project/cj107/rds-cj107-jiggins-rds/projects/project_HerIll/wz345/output/PCAngsd/SFS_change/R1/
cat 0.*.ml > 0_raw.ml
cat 3.*.ml > 3_raw.ml
cat 6.*.ml > 6_raw.ml

# 2. Sum up values in every row and keep six decimals (as the original output format by realSFS).
for i in `head -n 1 0_raw.ml | awk '{print NF}' | xargs seq`; 
do 
awk -v a=$i 'BEGIN{sum = 0} {sum += $a} END{printf ("%.6f\n",sum)}' 0_raw.ml >> 0_sum_vertical.ml; 
done

for i in `head -n 1 3_raw.ml | awk '{print NF}' | xargs seq`;
do
awk -v a=$i 'BEGIN{sum = 0} {sum += $a} END{printf ("%.6f\n",sum)}' 3_raw.ml >> 3_sum_vertical.ml;
done

for i in `head -n 1 6_raw.ml | awk '{print NF}' | xargs seq`;
do
awk -v a=$i 'BEGIN{sum = 0} {sum += $a} END{printf ("%.6f\n",sum)}' 6_raw.ml >> 6_sum_vertical.ml;
done

# 3. Print out the summing result in horizontal version.
paste -d " " -s 0_sum_vertical.ml > 0.ml
paste -d " " -s 3_sum_vertical.ml > 3.ml
paste -d " " -s 6_sum_vertical.ml > 6.ml

rm *_sum_vertical.ml
