### 
library(ggplot2) # plotting
library(dplyr) # data re ordering
library(forcats) # releveling
library(writexl) # write excel file
library(ggpubr) # ggdendisty plots
library(Rmisc) # for summary SE table making
library(ggside) # plot side histos
library(lmerTest) # lmer mixed models 
library(emmeans) # posthoc analysis
library(jtools) # For summ function to show model results
library(car) # homoscedasticity tests 

setwd("E:\\OneDrive - University of Cambridge\\1. Papers!!!\\202307 BSF Selection Line Paper\\2nd\\replot\\analysis\\SizeData_LineRepGen")
ChapV_mergedData_Filt_v3 <- read.csv("ChapterV_raw_data_merged_filtered_FILT_v2.csv", header = T, sep = ",") # performance, nutition and conversion raw data
str(ChapV_mergedData_Filt_v3)

# Plot:
REP1 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R1")
REP1p <- ggplot(REP1, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)") + ylim(0.25,1.25); REP1p

REP2 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R2")
REP2p <- ggplot(REP2, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)")+ ylim(0.25,1.25); REP2p

REP3 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R3")
REP3p <- ggplot(REP3, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)")+ ylim(0.25,1.25); REP3p

ggarrange(REP1p, REP2p, REP3p, ncol=1, nrow=3)

# Replicate 01:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/SizeData_LineRepGen")
Rep01_fmt <- read.csv("./Rep1_Cont-Resp_v1.csv", header = T, sep = ",")

model1 <- lm(Area ~ Line + Generation + Generation:Line, 
             data = Rep01_fmt)
summary(model1)
AOVresd <- aov(model1); AOVresd
par(mfrow = c(2, 2))
plot(model1)

# Fixed effects:
AOV1 <- anova(model1); AOV1 # Get P-vals for variables
emmeans(model1, list(pairwise ~ Generation*Line), adjust = "tukey")


REP1 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R1")
REP1p <- ggplot(REP1, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)") + ylim(0.25,1.25) + 
  stat_compare_means(label = "p.signif", method = "t.test"); REP1p





# Replicate 02:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/SizeData_LineRepGen")
Rep02_fmt <- read.csv("./Rep2_Cont-Resp_v1.csv", header = T, sep = ",")

model1 <- lm(Area ~ Line + Generation + Generation:Line, 
             data = Rep02_fmt)
summary(model1)
AOVresd <- aov(model1); AOVresd
par(mfrow = c(2, 2))
plot(model1)

# Fixed effects:
AOV1 <- anova(model1); AOV1 # Get P-values for variables
emmeans(model1, list(pairwise ~ Generation*Line), adjust = "tukey")

REP2 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R2")
REP2p <- ggplot(REP2, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)")+ ylim(0.25,1.25) +
  stat_compare_means(label = "p.signif", method = "t.test"); REP2p





# Replicate 03:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/SizeData_LineRepGen")
Rep03_fmt <- read.csv("./Rep3_Cont-Resp_v1.csv", header = T, sep = ",")

model1 <- lm(Area ~ Line + Generation + Generation:Line, 
             data = Rep03_fmt)
summary(model1)
AOVresd <- aov(model1); AOVresd
par(mfrow = c(2, 2))
plot(model1)

# Fixed effects:
AOV1 <- anova(model1); AOV1 # Get P-values for variables
emmeans(model1, list(pairwise ~ Generation*Line), adjust = "tukey")

# Fit a model with the full fixed and random effects:
model1 <- lmer(Area ~ Line + Generation + Generation:Line + (1|Label:Line), data = REP1)
summ(model1)
AOV1 <- anova(model1); AOV1 # Get P-values for variables

# Random effects:
summary(model1); plot(model1)
0.001642/(0.001642+ 0.009949) * 100 # Brood explains 14.16616 % of unexplained variance

REP3 <- subset(ChapV_mergedData_Filt_v3, Replicate == "R3")

REP3p <- ggplot(REP3, aes(x=Generation, y=Area, fill = Line)) + geom_boxplot(notch=F) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00", "#D55E00")) +
  theme_classic() +  theme(legend.position="right") + 
  ylab("Pupal size (cm2)")+ ylim(0.25,1.25) +  
  stat_compare_means(label = "p.signif", method = "t.test"); REP3p

ggarrange(REP1p, REP2p, REP3p, ncol=1, nrow=3)



# Continuous data:
posSQRTTransform <- function(p) {sqrt(p)} # moderate Pos skew
posLogTransoform <- function(p) {log10(p)} # great Pos skew
posInverseTransform <- function(p) {1/(p)} # severe Pos skew
negSQRTTransform <- function(p) {sqrt(max(p+1) - p) }# moderate Neg skew
negLogTransoform <- function(p) {log10(max(p+1) - p)}# great Neg skew
negInverseTransform <- function(p) {1/(max(p+1) - p) }# severe Neg skew

# Statistics:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/Size_CTRL-LRGE_only_model/")
Rep123_Area_Control_Large_only <- read.csv("./R1R2R3_ChapterV_Area_TRL-LRG_only_v1.csv", header = T, sep = ",")
summary(Rep123_Area_Control_Large_only)
40816+36418

# Fit the mixed-design ANOVA model/ Linear mixed-effect model:
model = lmer(Area ~ Line * Generation + (1|Replicate), data=Rep123_Area_Control_Large_only)
summary(model)
Anova(model, type="III")
AOV1 <- anova(model); AOV1 # Get P-vals for variables

#                   SumSq     MeanSq    NumDF DenDF   F value   Pr(>F)    
#  Line             153.347   153.347   1     77219   11569.25  < 2.2e-16 ***
#  Generation       96.664    16.111    6     77219   1215.47   < 2.2e-16 ***
#  Line:Generation  11.209    1.868     6     77219   140.95    < 2.2e-16 ***

summary(model); plot(model)
0.0005095/(0.0005095 + 0.0132547)*100 # Experiment accounts for 3.701632% variation
emmeans(model, list(pairwise ~ Line*Generation), adjust = "tukey")





G7_only <- subset(Rep123_Area_Control_Large_only, Generation == "G7")
LineSummary <- summarySE(G7_only, measurevar="Area", groupvars=c("Line")); LineSummary
LineSummary
100-(0.6410133/0.7551210*100)

