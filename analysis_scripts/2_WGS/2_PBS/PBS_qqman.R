# PBS
setwd("288_ANGSD\\FST_PBS")
library(qqman)

# Preparing figure alignment frame
par(mfrow=c(3,1))
par("mar") # Checking plot margin setting
par(mar=c(4,4,1,1)) # Margin order: c(bottom, left, top, right)


genes <- c("CysRS & eIF3m","eIF4A","eIF4A-like & LOC119651082","InR","LOC119659542 & Mps1", "Nab2", "Pkc53E", "Trnat-agu-6 & NiPp1")

# Making Manhattan plots and fit them into the frame
data1 <- read.csv("pbs_R1_S1-S7_CMplot_12genesHL.csv", header=T)
p1 <- manhattan(data1, chr="chr", bp="midPos", p= "PBS0", snp="SNP", logp=F, highlight = genes, 
                annotatePval = 0, annotateTop = FALSE, 
          ylim=c(0,0.57), ylab="R1 PBS", xlab="", genomewideline=4*sd(data1$PBS0))
p1

data2 <- read.csv("pbs_R2_S1-S7_CMplot_12genesHL.csv", header=T)
p2 <- manhattan(data2, chr="chr", bp="midPos", p= "PBS0", snp="SNP", logp=F, highlight = genes, 
                annotatePval = 0, annotateTop = FALSE, 
          ylim=c(0,0.65), ylab="R2 PBS", xlab="", genomewideline=4*sd(data2$PBS0))
p2

data3 <- read.csv("pbs_R3_S1-S7_CMplot_12genesHL.csv", header=T)
p3 <- manhattan(data3, chr="chr", bp="midPos", p= "PBS0", snp="SNP", logp=F, highlight = genes, 
                annotatePval = 0, annotateTop = FALSE, 
          ylim=c(0,0.55), ylab="R3 PBS", genomewideline=4*sd(data3$PBS0))
p3



# FST - selection over drift
setwd("288_ANGSD\\FST_PBS")
library(qqman)


# R1
par(mfrow=c(2,1))
par("mar")
par(mar=c(4,4,1,1))

data1 <- read.csv("fst02_R1_S1-S7_CMplot.csv", header=T)
p1 <- manhattan(data1, chr="chr", bp="midPos", p= "Fst02", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R1 Fst LARGE-BASE", xlab="Chromosome")
p1

data2 <- read.csv("fst12_R1_S1-S7_CMplot.csv", header=T)
p2 <- manhattan(data2, chr="chr", bp="midPos", p= "Fst12", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R1 Fst CTRL-BASE", xlab="Chromosome")
p2


# R2
par(mfrow=c(2,1))
par("mar")
par(mar=c(4,4,1,1))

data1 <- read.csv("fst02_R2_S1-S7_CMplot.csv", header=T)
p1 <- manhattan(data1, chr="chr", bp="midPos", p= "Fst02", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R2 Fst LARGE-BASE", xlab="Chromosome")
p1

data2 <- read.csv("fst12_R2_S1-S7_CMplot.csv", header=T)
p2 <- manhattan(data2, chr="chr", bp="midPos", p= "Fst12", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R2 Fst CTRL-BASE", xlab="Chromosome")
p2


# R3
par(mfrow=c(2,1))
par("mar")
par(mar=c(4,4,1,1))

data1 <- read.csv("fst02_R3_S1-S7_CMplot.csv", header=T)
p1 <- manhattan(data1, chr="chr", bp="midPos", p= "Fst02", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R3 Fst LARGE-BASE", xlab="Chromosome")
p1

data2 <- read.csv("fst12_R3_S1-S7_CMplot.csv", header=T)
p2 <- manhattan(data2, chr="chr", bp="midPos", p= "Fst12", snp="SNP", logp=F, 
                ylim=c(0,0.57), ylab="R3 Fst CTRL-BASE", xlab="Chromosome")
p2

