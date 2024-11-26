### Life stage variation in weights:
library(tidyverse)
library(ggdist)
library(ggplot2)
library(Rmisc) # for summary SE table making

setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/lifestage_variation_in_weights")
RawData <- read.csv("./raw_lifestage_weight_data.csv", header = T, sep = ",")

# Order data:
RawData <- RawData %>%    
  mutate(Replicate = fct_relevel(Lifestage, 
                                 "larval", "prepupa", "pupal"))

Control <- subset(RawData, Treatment == "Control")
Control <- Control %>%    
  mutate(Replicate = fct_relevel(Lifestage, 
                                 "pupal", "prepupa", "larval"))
Control %>%
  ggplot(aes(x=Weight_mg, y=Lifestage, fill=Lifestage))+
  stat_slab(aes(thickness = stat(pdf*n)), 
            scale = 0.7) +
  stat_dotsinterval(side = "bottom",
                    scale = 0.7,
                    slab_size = NA) + 
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  theme_bw() +
  ylab("Life stage") + xlab("Weight (mg)") #+ 

summary(Control)
LineSummary <- summarySE(Control, measurevar="Weight_mg", groupvars=c("Lifestage"), na.rm = TRUE)
LineSummary

#scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99")) + 
#scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
# Use control as its the fairest comp.

