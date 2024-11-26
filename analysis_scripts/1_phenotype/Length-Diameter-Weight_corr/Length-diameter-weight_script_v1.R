### LM: pupal length - diameter and weight correlations
library(ggplot2) # plotting
library(ggpubr)
library(forcats) # re-leveling datasets

setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/")
# Get mean size, weights and efficiency from GDrive copy to local and make a new sheet:
RawData <- read.csv("./raw_data/Data_R1R2R3_PupalDevRate_OvaryWeight_v1.csv", header = T, sep = ",")

# Order data:
RawData <- RawData %>%    
  mutate(Experiment = fct_relevel(Experiment, 
                                 "R1", "R2", "R3"))         # Order data by Replicate
RawData <- RawData %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                            "Control", "Large"))         # Order data by Line

# Single line:
#A <- ggplot(RawData, aes(x = PupalShellLength, y = PupalShellWidth)) + 
#  geom_smooth(aes(), method = lm)+
#  geom_point(aes()) +
#  ggpubr::stat_cor(aes(), label.x = 12) + theme_bw() +#
#  ggtitle("length-diameter"); A
# R = 0.64 p = < 2.2e-16
#
#B <-ggplot(RawData, aes(x = weight_mg, y = PupalShellLength)) + 
#  geom_smooth(aes(), method = lm)+
#  geom_point(aes()) +
#  ggpubr::stat_cor(aes(), label.x = 75) + theme_bw() +
#  ggtitle("length-weight")+ xlim(60,150); B
# R = 0.79 p = < 2.2e-16

#C <- ggplot(RawData, aes(x = weight_mg, y = PupalShellWidth)) + 
#  geom_smooth(aes(), method = lm)+
#  geom_point(aes()) +
#  ggpubr::stat_cor(aes(), label.x = 75) + theme_bw() +
#  ggtitle("weight-diameter") + xlim(60,150); C
# R = 0.68 p = < 2.2e-16
#ggarrange(A, B, C, nrow=2, ncol=2)

Control <- subset(RawData, Treatment == "Control")
  
# Single line:
A <- ggplot(Control, aes(x = PupalShellLength, y = PupalShellWidth)) + 
  geom_smooth(aes(), method = lm)+
  geom_point(aes()) +
  ggpubr::stat_cor(aes(), label.x = 12) + theme_bw() +
  ggtitle("length-diameter"); A
# R = 0.68 p = 5e-13

B <-ggplot(Control, aes(x = weight_mg, y = PupalShellLength)) + 
  geom_smooth(aes(), method = lm)+
  geom_point(aes()) +
  ggpubr::stat_cor(aes(), label.x = 75) + theme_bw() +
  ggtitle("length-weight")+ xlim(60,150); B
# R = 0.86 p = < 2.2e-16

C <- ggplot(Control, aes(x = weight_mg, y = PupalShellWidth)) + 
  geom_smooth(aes(), method = lm)+
  geom_point(aes()) +
  ggpubr::stat_cor(aes(), label.x = 75) + theme_bw() +
  ggtitle("weight-diameter") + xlim(60,150); C
# R = 0.73 p = < 1.1e-15

ggarrange(A, B, C, nrow=2, ncol=2)






# 3D:
library(scatterplot3d)
library(tidyverse) 
d <- RawData %>% 
  select(weight_mg, PupalShellWidth, PupalShellLength)
lm1a <- lm(d$weight_mg ~ d$PupalShellLength + d$PupalShellWidth)
p3 <- scatterplot3d(d)

