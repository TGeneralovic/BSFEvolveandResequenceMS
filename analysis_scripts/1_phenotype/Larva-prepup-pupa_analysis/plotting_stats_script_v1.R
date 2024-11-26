# Lifestage-Weights fixed times & body mass loss per life stage 
library(ggplot2) # plotting
library(dplyr) # data re ordering
library(forcats) # releveling
library(writexl) # write excel file
library(ggpubr) # ggdendisty plots
library(Rmisc) # for summary SE table making
library(ggside) # plot side histos
library(lmerTest) # lmer mixed models 
library(emmeans) # posthoc analysis
library(ggdist) # stats halfeye plots

options(scipen=999) # turn scientific notation off
options(scipen=0) # turn scientific notation on

###----Make transformations----
# Percentage data:
logitTransform <- function(p) {log(p/(1-p))}
ArcsinTransform <-  function(p) {asin(sqrt(p))}

# Continuous data:
posSQRTTransform <- function(p) {sqrt(p)} # moderate Pos skew
posLogTransoform <- function(p) {log10(p)} # great Pos skew
posInverseTransform <- function(p) {1/(p)} # severe Pos skew
negSQRTTransform <- function(p) {sqrt(max(p+1) - p) }# moderate Neg skew
negLogTransoform <- function(p) {log10(max(p+1) - p)}# great Neg skew
negInverseTransform <- function(p) {1/(max(p+1) - p) }# severe Neg skew

###----Make transformations----
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/Larva-prepup-pupa_analysis/")
RawData <- read.csv("./L_PP_P_raw_data_v1.csv", header = T, sep = ",")

# Order data:
RawData <- RawData %>%    
  mutate(Lifestage = fct_relevel(Lifestage, 
                                 "larva", "prepupa", "pupal"))
RawData <- RawData %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control", "Large"))
RawData <- RawData %>%    
  mutate(Replicate = fct_relevel(Experiment, 
                                 "R1", "R2", "R3"))
head(RawData)
summary(RawData)
###----Plot----
ggplot(RawData, aes(x=Treatment, y=Weight_mg, fill = Treatment)) + geom_boxplot(notch=F) +  #geom_jitter(alpha=0.2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) + theme_classic() +  theme(legend.position="right") + 
  ylab("Weight (mg)") + 
  facet_wrap(~Lifestage) +
  #stat_compare_means() +
  #stat_summary(fun.y="mean", color = "red") +
  stat_compare_means(label =  "p.signif" , label.x = 1.5)

###----Model-testing----
hist(RawData$Weight_mg)
model1 <- lm(Weight_mg ~ Treatment*Lifestage + Replicate, data=RawData)
summary(model1)
AOVresd <- aov(model1)

shapiro.test(AOVresd$residuals) #W = 0.98414, p-value = 1.487e-10
#leveneTest(Weight_mg ~ Treatment*Lifestage, data = RawData) # P = 0.3211

RawData$Weight_mgTrans <- negInverseTransform(RawData$Weight_mg); hist(RawData$Weight_mgTrans)
model1 <- lm(Weight_mgTrans ~ Treatment*Lifestage + Replicate, data=RawData)
summary(model1)
AOVresd <- aov(model1)
shapiro.test(AOVresd$residuals) # No transformations work on full dataset 

# Subset data by lifestage:
#Larval:
larval <- subset(RawData, Lifestage =="pupal")
hist(larval$Weight_mg)
model1 <- lm(Weight_mg ~ Treatment + Replicate, data=larval)
summary(model1)
AOVresd <- aov(model1)
shapiro.test(AOVresd$residuals) # W = 0.98414, p-value = 1.487e-10

larval$Weight_mgTrans <- negSQRTTransform(larval$Weight_mg); hist(larval$Weight_mgTrans)
model1 <- lm(Weight_mgTrans ~ Treatment + Replicate, data=larval)
summary(model1)
AOVresd <- aov(model1)
shapiro.test(AOVresd$residuals) # No transformations work on full dataset.

###----Model----
# Larval:
larval <- subset(RawData, Lifestage =="larva")
wilcox.test(Weight_mg ~ Treatment, data = larval)
# W = 19358, p-value = 1.818e-13





# Prepupal:
prepupal <- subset(RawData, Lifestage =="prepupa")
wilcox.test(Weight_mg ~ Treatment, data = prepupal)
# W = 14812, p-value < 2.2e-16

# Pupal:
pupal <- subset(RawData, Lifestage =="pupal")
wilcox.test(Weight_mg ~ Treatment, data = pupal)
# W = 2288.5, p-value < 2.2e-16

###----Weight-loss-%----
# Make summary table:
LineSummary <- summarySE(RawData, measurevar="Weight_mg", groupvars=c("Treatment", "Lifestage", "DOL"), na.rm = TRUE)
LineSummary

# Control: Larva - Prepupa:
100-(123.43044/155.72640*100) # 20.73891
# Control: Prepupa - Pupa:
100-(94.00643/123.43044*100) # 23.83854
# Control: Larva - Pupa:
100-(94.00643/155.72640*100) # 39.63359

# Large: Larva - Prepupa:
100-(150.82264/182.97448*100) # 17.57176
# Large: Prepupa - Pupa:
100-(116.26342/150.82264*100) # 22.91381
# Large: Larva - Pupa:
100-(116.26342/182.97448*100) # 36.45922

# Across both lines:
LineSummary <- summarySE(RawData, measurevar="Weight_mg", groupvars=c("Lifestage"), na.rm = TRUE)
LineSummary
# Larva - Prepupa:
100-(137.1540/169.3504*100) # 19.01171
# Prepupa - Pupa:
100-(106.7836/137.1540*100) # 22.14328

# Larva - Pupa:
100-(106.7836/169.3504*100) # 36.94517

###----How much bigger-%----
LineSummary <- summarySE(RawData, measurevar="Weight_mg", groupvars=c("Treatment", "Lifestage"), na.rm = TRUE)
LineSummary
# Larval:
100-(155.72640/182.97448*100) # 14.89174
# Prepupal:
100-(123.43044/150.82264*100) # 18.16186
# Pupal:
100-(94.00643/116.26342*100) # 19.14359
