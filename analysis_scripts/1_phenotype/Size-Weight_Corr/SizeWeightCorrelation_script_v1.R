### LM: Size - Weight correlation
library(ggplot2)
library(ggpubr)

setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/lm_weight-size_corr")
# Get mean size, weights and efficiency from GDrive copy to local and make a new sheet:
RawData <- read.csv("./raw_data_manual.csv", header = T, sep = ",")

# Order data:
RawData <- RawData %>%    
  mutate(Replicate = fct_relevel(Replicate, 
                                 "R1", "R2", "R3"))         # Order data by Replicate
RawData <- RawData %>%    
  mutate(Line = fct_relevel(Line, 
                            "Base", "Control", "Large_responce", "Large_selected"))         # Order data by Line
RawData <- RawData %>%    
  mutate(Generation = fct_relevel(Generation, 
                                  "G0", "G1", "G2","G3", "G4", "G5", "G6", "G7"))         # Order data by Generation

# Remove BASE as n=3 only 
RawData_fmt1 <-RawData[!(RawData$Line=="Base" | RawData$Line=="Base"),]
RawData_fmt1

# By replicate:
ggplot(RawData_fmt1, aes(x = m.area, y = m.weight)) + 
  geom_smooth(aes(color = Line), method = lm, 
              se = F, fullrange = TRUE)+
  geom_point(aes(color = Line, size = GroupNo), alpha = 0.5) +
  scale_color_manual(values = c("#56B4E9", "#E69F00", "#D55E00")) +
  scale_size(range = c(0.5, 12)) + # Adjust the range of points size
  ggpubr::stat_cor(aes(color = Line), label.x = 0.75) + theme_bw()

# Single line:
ggplot(RawData_fmt1, aes(x = m.area, y = m.weight)) + 
  geom_smooth(aes(), method = lm)+
  geom_point(aes()) +
  ggpubr::stat_cor(aes(), label.x = 0.75) + theme_bw()
# R = 0.94 