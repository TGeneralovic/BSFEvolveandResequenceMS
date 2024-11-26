setwd("288_ANGSD\\FST_PBS")
dt1 <- read.csv("pbs_R1_S1-S7.20kb.20kb.csv", header=T)
dt2 <- read.csv("pbs_R2_S1-S7.20kb.20kb.csv", header=T)
dt3 <- read.csv("pbs_R3_S1-S7.20kb.20kb.csv", header=T)

# selecting candidate windows
sub1 <- subset(dt1,Fst12>=4*sd(dt1$Fst12)) 
sub2 <- subset(dt2,Fst12>=4*sd(dt2$Fst12)) 
sub3 <- subset(dt3,Fst12>=4*sd(dt3$Fst12)) 

write.csv(sub1, file="regions_R1_Fst12.csv")
write.csv(sub2, file="regions_R2_Fst12.csv")
write.csv(sub3, file="regions_R3_Fst12.csv")
