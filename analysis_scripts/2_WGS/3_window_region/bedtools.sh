module load bedtools2-2.26.0-gcc-5.4.0-t3rbc3z


# select shared regions
bedtools intersect -a R1.bed -b R2.bed > R1R2.bed
bedtools intersect -a R1R2.bed -b R3.bed > R1R2R3.bed

# extract genes that locate within target regions
bedtools window -a R1R2R3.bed -b gene_S1-S7.bed -w 0 > R1R2R3_genes.txt
