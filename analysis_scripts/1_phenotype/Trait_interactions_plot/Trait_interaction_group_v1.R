# Figure 2: Trait interactions & trade-offs
library(ggplot2) # plotting
library(dplyr) # data re ordering
library(forcats) # releveling
library(writexl) # write excel file
library(ggpubr) # ggdendisty plots
library(Rmisc) # for summary SE table making
library(ggside) # plot side histos
library(lmerTest) # lmer mixed models 
library(emmeans) # post-hoc analysis

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

# Step 1: Make box plot with t-test comps on each trait:
# Trait 01: Larval growth rate-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/DailyGrowthRate")
GRdata = read.csv("./Data_R1R2R3_LarvalDailyGrowthRates_v1.csv")
head(GRdata)

LineSummary <- summarySE(GRdata, measurevar="Weight_mg", groupvars=c("Experiment", "Treatment", "DOL"), na.rm = TRUE)
LineSummary

LineSummary <- summarySE(GRdata, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE)
LineSummary
100-(121.8595/144.5407*100)

# Generate growth rates (mg/day):
GrowthRates_calc_v1 <- (GRdata$Weight_mg/GRdata$DOL)
GrowthRates_calc_v2 <- data.frame(GRdata$Experiment, GRdata$Treatment, GRdata$DOL, GrowthRates_calc_v1)
head(GrowthRates_calc_v2)
str(GrowthRates_calc_v2); summary(GrowthRates_calc_v2)

# Two-sample t-test: by Treatment
control <- subset(GrowthRates_calc_v2, GRdata.Treatment == "Control"); summary(control) # Replace the dots with your control group data
large <- subset(GrowthRates_calc_v2, GRdata.Treatment == "Large"); summary(large) # Replace the dots with your large group data

result1 <- shapiro.test(control$GrowthRates_calc_v1); hist(control$GrowthRates_calc_v1); result1
result2 <- shapiro.test(large$GrowthRates_calc_v1); hist(large$GrowthRates_calc_v1); result2

res <- wilcox.test(GrowthRates_calc_v1 ~ GRdata.Treatment, data = GrowthRates_calc_v2,
                   exact = FALSE)
res
# W = 5296691, p-value < 2.2e-16


# Plot
GrowthRate_pp <- ggplot(GrowthRates_calc_v2, aes(x=GRdata.Treatment, y=GrowthRates_calc_v1, fill = GRdata.Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Growth rate (mg/day)") +  xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                      label.x = 1.5); GrowthRate_pp

# How much change is the large line?
LineSummary <- summarySE(GRdata, measurevar="Weight_mg", groupvars=c("Experiment", "Treatment"), na.rm = TRUE); LineSummary
100-(128.6548/167.0122*100) # R1: +22.96683%
100-(115.7036/137.5460*100) # R1: +15.88007%
100-(120.9156/132.1084*100) # R1: +8.472436%

# Across all replicates:
LineSummary <- summarySE(GRdata, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(121.8595/144.5407*100) # R1: +15.69191%

# Trait 02: Larval weight (mg)-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/lifestage_variation_in_weights")
RawData <- read.csv("./raw_lifestage_weight_data.csv", header = T, sep = ",")

# Order data:
RawData <- RawData %>%    
  mutate(Lifestage = fct_relevel(Lifestage, 
                                 "larval", "prepupa", "pupal"))
summary(RawData)
Larva_ony <- subset(RawData, Lifestage == "larval")

# Two-sample t-test:
library(stats)
control <- subset(Larva_ony, Treatment == "Control")  # Replace the dots with your control group data
large <- subset(Larva_ony, Treatment == "Large") # Replace the dots with your large group data

result1 <- shapiro.test(control$Weight_mg); hist(control$Weight_mg); result1
result2 <- shapiro.test(large$Weight_mg); hist(large$Weight_mg); result2

control$Weight_mgTrans <- posLogTransoform(control$Weight_mg)
large$Weight_mgTrans <- posLogTransoform(large$Weight_mg)
result1 <- shapiro.test(control$Weight_mgTrans); hist(control$Weight_mgTrans); result1
result2 <- shapiro.test(large$Weight_mgTrans); hist(large$Weight_mgTrans); result2


res <- wilcox.test(Weight_mg ~ Treatment, data = Larva_ony,
                   exact = FALSE)
res
# W = 19358, p-value = 1.818e-13

# Plot
Larvalweight_pp <- ggplot(Larva_ony, aes(x=Treatment, y=Weight_mg, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Larval weight (mg)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Larvalweight_pp

# How much change is the large line?
LineSummary <- summarySE(Larva_ony, measurevar="Weight_mg", groupvars=c("Replicate", "Treatment"), na.rm = TRUE); LineSummary
100-(169.1920 /202.7933 *100) # R1: +16.56924%
100-(132.3187 /143.0443 *100) # R1: +7.498097%
100-(163.1830 /198.0580 *100) # R1: +17.60848%

# Across all replicates:
LineSummary <- summarySE(Larva_ony, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(155.7264/182.9745 *100) # R1: +14.89175%


# Trait 03: Prepupal weight (mg)-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/lifestage_variation_in_weights")
RawData <- read.csv("./raw_lifestage_weight_data.csv", header = T, sep = ",")
library(tidyverse)
library(ggdist)

# Order data:
RawData <- RawData %>%    
  mutate(Lifestage = fct_relevel(Lifestage, 
                                 "larval", "prepupa", "pupal"))
summary(RawData)
Prepupa_only <- subset(RawData, Lifestage == "prepupa")

# Two-sample t-test:
library(stats)
control <- subset(Prepupa_only, Treatment == "Control")  # Replace the dots with your control group data
large <- subset(Prepupa_only, Treatment == "Large") # Replace the dots with your large group data

result1 <- shapiro.test(control$Weight_mg); hist(control$Weight_mg); result1
result2 <- shapiro.test(large$Weight_mg); hist(large$Weight_mg); result2

res <- wilcox.test(Weight_mg ~ Treatment, data = Prepupa_only,
                   exact = FALSE)
res
# W = 14812, p-value < 2.2e-16

# Plot:
Prepupalweight_pp <- ggplot(Prepupa_only, aes(x=Treatment, y=Weight_mg, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Prepupal weight (mg)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Prepupalweight_pp

# How much change is the large line?
LineSummary <- summarySE(Prepupa_only, measurevar="Weight_mg", groupvars=c("Replicate", "Treatment"), na.rm = TRUE); LineSummary
100-(142.7613  /177.7627  *100) # R1: +19.68996%
100-(106.3646  /128.9928  *100) # R1: +17.54222%
100-(121.5610  /146.9900  *100) # R1: +17.29982%

# Across all replicates:
LineSummary <- summarySE(Prepupa_only, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(123.4304  /150.8226   *100) # R1: +18.16187%


# Trait 04: Pupal weight (mg)-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/lifestage_variation_in_weights")
RawData <- read.csv("./raw_lifestage_weight_data.csv", header = T, sep = ",")
library(tidyverse)
library(ggdist)

# Order data:
RawData <- RawData %>%    
  mutate(Lifestage = fct_relevel(Lifestage, 
                                 "larval", "prepupa", "pupal"))
summary(RawData)
Pupal_only <- subset(RawData, Lifestage == "pupal")

# Two-sample t-test:
library(stats)
control <- subset(Pupal_only, Treatment == "Control")  # Replace the dots with your control group data
large <- subset(Pupal_only, Treatment == "Large") # Replace the dots with your large group data

result1 <- shapiro.test(control$Weight_mg); hist(control$Weight_mg); result1
result2 <- shapiro.test(large$Weight_mg); hist(large$Weight_mg); result2

res <- wilcox.test(Weight_mg ~ Treatment, data = Pupal_only,
                   exact = FALSE)
res
# W = 2288.5, p-value < 2.2e-16

# Plot:
Pupalweight_pp <- ggplot(Pupal_only, aes(x=Treatment, y=Weight_mg, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Pupal weight (mg)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Pupalweight_pp

# How much change is the large line?
LineSummary <- summarySE(Pupal_only, measurevar="Weight_mg", groupvars=c("Replicate", "Treatment"), na.rm = TRUE); LineSummary
100-(91.59216  /131.77907   *100) # R1: +30.49567%
100-(83.64250   /112.46917    *100) # R1: +25.63073%
100-(97.68571   /110.04700   *100) # R1: +11.23274%

# Across all replicates:
LineSummary <- summarySE(Pupal_only, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(94.00643   /116.26342    *100) # R1: +19.14359%

# Trait 05: Pupal size---p < 2.2e-16 ***--------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/collating_merged_raw_data_file")
ChapV_mergedData_Filt_v3 <- read.csv("./ChapterV_raw_data_merged_filtered_v2.csv", header = T, sep = ",") # performance, nutition and conversion raw data
head(ChapV_mergedData_Filt_v3)
Size_F7 <- subset(ChapV_mergedData_Filt_v3, Generation == "G7") # Replace the dots with your large group data
summary(Size_F7)

LineSummary <- summarySE(Size_F7, measurevar="Area", groupvars=c("Replicate", "Line"))
LineSummary

# Two-sample t-test:
control <- subset(Size_F7, Line == "Control") # Replace the dots with your control group data
large <- subset(Size_F7, Line == "Large_responce") # Replace the dots with your large group data

result1 <- shapiro.test(control$Area); result1
hist(control$Area)
result2 <- shapiro.test(large$Area); result2 
hist(large$Area)

t.test(control$Area, large$Area)
# t = -60.987, df = 11699, p-value < 2.2e-16
# (T-test, t11699=-60.987,p < 2.2e-16) *

# Plot:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/Pupal_F7_size")
Size_F7_CTRL_LRGE <- read.csv("./RawData_F7_size_CTRL-LRG_v1.csv", header = T, sep = ",") # performance, nutition and conversion raw data

Pupalsize_pp <- ggplot(Size_F7_CTRL_LRGE, aes(x=Line, y=Area, fill = Line)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Pupal body size (F7; cm2)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Pupalsize_pp

# How much change is the large line?
LineSummary <- summarySE(Size_F7_CTRL_LRGE, measurevar="Area", groupvars=c("Replicate", "Line"), na.rm = TRUE); LineSummary
100-(0.6520921   /0.7428206    *100) # R1: +12.21405%
100-(0.6198561    /0.7436430     *100) # R1: +16.64601%
100-(0.6552064    /0.7848096    *100) # R1: +16.51397%

# Across all replicates:
LineSummary <- summarySE(Size_F7_CTRL_LRGE, measurevar="Area", groupvars=c("Line"), na.rm = TRUE); LineSummary
100-(0.6412067    /0.7568712     *100) # R1: +15.28193%

# Trait 06: Protein content (mg/L)---p = 0.01182 *--------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/raw_data/")
Protein_raw <- read.csv("./Data_R1R2R3_LarvalProteinContent_v1.csv", header = T, sep = ",")
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/protein/")

# Order data:
Protein_raw <- Protein_raw %>%    
  mutate(Replicate = fct_relevel(Replicate, 
                                 "R1", "R2", "R3")) # Order data by Replicate
Protein_raw <- Protein_raw %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control","Large")) # Order data by Line

LineSummary <- summarySE(Protein_raw, measurevar="Protein_content_mg.L", groupvars=c("Replicate", "Treatment"))
LineSummary

# Two-sample t-test:
library(stats)
control <- subset(Protein_raw, Treatment == "Control")  # Replace the dots with your control group data
large <- subset(Protein_raw, Treatment == "Large") # Replace the dots with your large group data

result1 <- shapiro.test(control$Protein_content_mg.L); hist(control$Protein_content_mg.L); result1
result2 <- shapiro.test(large$Protein_content_mg.L); hist(large$Protein_content_mg.L); result2

control$Protein_content_mgTrans <- posSQRTTransform(control$Protein_content_mg)
large$Protein_content_mgTrans <- posSQRTTransform(large$Protein_content_mg)

result1 <- shapiro.test(control$Protein_content_mgTrans); hist(control$Protein_content_mgTrans); result1
result2 <- shapiro.test(large$Protein_content_mgTrans); hist(large$Protein_content_mgTrans); result2

t.test(control$Protein_content_mgTrans, large$Protein_content_mgTrans)
# t = -2.5996, df = 57.946, p-value = 0.01182
# (T-test, t58=-2.6,p<0.05) *

# Plot:
Protein_pp <- ggplot(Protein_raw, aes(x=Treatment, y=Protein_content_mg.L, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Protein content (mg/L)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Protein_pp

# How much change is the large line?
LineSummary <- summarySE(Protein_raw, measurevar="Protein_yield_perL_mg", groupvars=c("Replicate", "Treatment"), na.rm = TRUE); LineSummary
100-(0.05585503    /0.07220017     *100) # R1: +22.63864%
100-(0.04167349     /0.05341097      *100) # R1: +21.97579%
100-(0.06378975     /0.06245742     *100) # R1: -2.133181%

# Across all replicates:
LineSummary <- summarySE(Protein_raw, measurevar="Protein_yield_perL_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(0.05528517     /0.06404819      *100) # R1: +13.68192%

# Trait 07: Larval development (day 13)---p = 0.00024 ***--------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/raw_data/")
DevRate_raw <- read.csv("./Data_R1R2R3_LSDevelopmentRate_v1.csv", header = T, sep = ",")
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/LifeStageDevRates/")

# Order data:
DevRate_raw <- DevRate_raw %>%    
  mutate(Experiment = fct_relevel(Experiment, 
                                  "R1", "R2", "R3")) # Order data by Replicate
DevRate_raw <- DevRate_raw %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control","Large")) # Order data by Line
DevRate_raw <- DevRate_raw %>%    
  mutate(DOL = fct_relevel(DOL, 
                           "5DOL","6DOL","7DOL","8DOL","9DOL","10DOL","11DOL","12DOL","13DOL","14DOL","15DOL","16DOL","17DOL","18DOL","19DOL","20DOL"))  # Order data by DOL
summary(DevRate_raw)

LarvalSummary <- summarySE(DevRate_raw, measurevar="larval_prop", groupvars=c("Treatment", "DOL"), na.rm = TRUE)
LarvalSummary

# Extract proportions at day 13:
Data_13DOL <- subset(DevRate_raw, DOL == "13DOL")
summary(Data_13DOL)
Data_13DOL$PROP_L <- (Data_13DOL$larval_prop/100)

LarvalSummary <- summarySE(Data_13DOL, measurevar="PROP_L", groupvars=c("Treatment"), na.rm = TRUE)
LarvalSummary
100 - (0.8907979 /0.9592092 *100) 

res <- wilcox.test(PROP_L ~ Treatment, data = Data_13DOL,
                   exact = FALSE)
res
# W = 325, p-value = 0.0002541

# Rep 01: 12-DOL
# CON: L + PP = TOTAL
(79+134+96+164+88+112+120+102+86) 
981 + 0 
# SEL: L + PP = TOTAL
(67+76+89+56+82+111+101+96+77) + (6+7+5+1+4+4+10+4+4) 
# two-proportions-z-test: 
res <- prop.test(x = c(0, 45), n = c(981, 800))
res # X2 = 13.45, p < 0.001




LarvalSummary <- summarySE(Data_13DOL, measurevar="PROP_L", groupvars=c("Treatment","Experiment", "DOL"), na.rm = TRUE)
LarvalSummary

# Plot:
Proportion_pp <- ggplot(Data_13DOL, aes(x=Treatment, y=PROP_L, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Larval proportion (day 13)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Proportion_pp

# How much change is the large line?
LineSummary <- summarySE(Data_13DOL, measurevar="PROP_L", groupvars=c("Replicate", "Treatment"), na.rm = TRUE); LineSummary
100-(0.8587879      /0.9632431        *100) # R1: +10.84412%
100-(0.8988150       /0.9481471       *100) # R1: +5.203001%
100-(0.8976026      /0.9645087      *100) # R1: 6.936806%

# Across all replicates:
LineSummary <- summarySE(Data_13DOL, measurevar="PROP_L", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(0.8907979      /0.9592092       *100) # R1: +7.132052%

#LarvalSummary <- summarySE(DevRate_raw, measurevar="larval_prop", groupvars=c("Treatment","Experiment", "DOL"), na.rm = TRUE)
#LarvalSummary
#
#Control_R1 <- c(1,1,1,1,1,1,1,1,1,1,1,1,
#             0.98,0.97,0.83,0.56,0.44,0.20,0.07,0.07,0.03,0.02,0.01,0.02,0.01)
#Control_R2 <- c(1,1,1,1,1,1,1,1,1,1,1,1,
#             0.98,0.93,0.82,0.71,0.71,0.38,0.25,0.24,0.1)
#Control_R3 <- c(1,1,1,1,1,1,1,1,1,1,1,0.99,
#             0.94,0.91,0.77,0.55,0.36,0.19,0.03,0.04,0.03)###
#
#Control_ALL <- c(1,1,1,1,1,1,1,1,1,1,1,1,
#                0.98,0.97,0.83,0.56,0.44,0.20,0.07,0.07,0.03,0.02,0.01,0.02,0.01,#
#                1,1,1,1,1,1,1,1,1,1,1,1#,
#                0.98,0.93,0.82,0.71,0.71,0.38,0.25,0.24,0.1,
#                1,1,1,1,1,1,1,1,1,1,1,0.99,
#                0.94,0.91,0.77,0.55,0.36,0.19,0.03,0.04,0.03)#
#
#Large_R1 <- c(1,1,1,1,1,1,1,1,1,1,1,0.95,
#             0.85,0.72,0.64,0.57,0.43,0.27,0.14,0.12,0.07,0.03,0.02,0.02,0.01)
#Large_R2 <- c(1,1,1,1,1,1,1,1,1,1,1,0.99,
#             0.94,0.90,0.78,0.77,0.68,0.38,0.47,0.25,0.17)
#Large_R3 <- c(1,1,1,1,1,1,1,1,1,1,1,1,
#              0.95,0.94,0.87,0.58,0.48,0.15,0.09,0.05,0.03)#
#
#Large_ALL <- c(1,1,1,1,1,1,1,1,1,1,1,0.95,
#              0.85,0.72,0.64,0.57,0.43,0.27,0.14,0.12,0.07,0.03,0.02,0.02,0.01,
#              1,1,1,1,1,1,1,1,1,1,1,0.99,
#              0.94,0.90,0.78,0.77,0.68,0.38,0.47,0.25,0.17,
#              1,1,1,1,1,1,1,1,1,1,1,1,
#              0.95,0.94,0.87,0.58,0.48,0.15,0.09,0.05,0.03)#

#TEST <- data.frame(Control_ALL, Large_ALL)
#data_long <- gather(TEST, Treatment, factor_key=TRUE)
#data_long
#res <- wilcox.test(value ~ Treatment, data = data_long,
#                   exact = FALSE)
#res
## W = 325, p-value = 0.0002541
#
## Plot:
#Proportion_pp <- ggplot(data_long, aes(x=Treatment, y=value, fill = Treatment)) + 
#  geom_boxplot(notch=F) + 
#  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
#  ylab("Larval proportion (day 13)") + 
#  theme_bw() + theme(legend.position="none") +
#  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
#                     label.x = 1.5); Proportion_pp
#
# Trait 08: Pupal development (hours)-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/raw_data/")
PupalDevRate_raw <- read.csv("./Data_R1R2R3_PupalDevRate_OvaryWeight_v1.csv", header = T, sep = ",")
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/LifeStageDevRates/")

# Order data:
PupalDevRate_raw <- PupalDevRate_raw %>%    
  mutate(Experiment = fct_relevel(Experiment, 
                                  "R1", "R2", "R3")) # Order data by Replicate
PupalDevRate_raw <- PupalDevRate_raw %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control","Large")) # Order data by Line
PupalDevRate_raw <- PupalDevRate_raw %>%    
  mutate(DOL = fct_relevel(DOL, 
                           "17DOL","18DOL","19DOL","20DOL", "21DOL"))  # Order data by DOL
head(PupalDevRate_raw)

control <- subset(PupalDevRate_raw, Treatment == "Control"); hist(control$TotalPupalDevTime) # Replace the dots with your control group data
large <- subset(PupalDevRate_raw, Treatment == "Large"); hist(large$TotalPupalDevTime) # Replace the dots with your large group data

t.test(control$TotalPupalDevTime, large$TotalPupalDevTime)
# t = -60.987, df = 11699, p-value < 2.2e-16
# (T-test, t11699=-60.987,p < 2.2e-16) *


res <- wilcox.test(TotalPupalDevTime ~ Treatment, data = PupalDevRate_raw,
                   exact = FALSE)
res
# W = 38499, p-value = 0.2761

# Plot:
setwd("E:\\OneDrive - University of Cambridge\\1. Papers!!!\\202307 BSF Selection Line Paper\\2nd\\replot\\analysis\\PupalDev")
PupalDevRate_clean<- read.csv("PupalDevelopment_v1.csv", header = T, sep = ",")
PupalDevRate_pp <- ggplot(PupalDevRate_clean, aes(x=Treatment, y=TotalPupalDevTime, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Pupal development time (hrs)") + xlab("") +
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5) + facet_wrap(~Sex); PupalDevRate_pp

# How much change is the large line?
LineSummary <- summarySE(PupalDevRate_raw, measurevar="TotalPupalDevTime", groupvars=c("Experiment", "Treatment"), na.rm = TRUE); LineSummary
100-(178.3087        /174.9597          *100) # R1: -1.914155%
100-(175.2183         /169.1667         *100) # R1: -3.5773%
100-(169.1975        /172.3034        *100) # R1: +1.802576%

# Across all replicates:
LineSummary <- summarySE(PupalDevRate_raw, measurevar="TotalPupalDevTime", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(175.1279       /172.8611         *100) # R1: -1.311342%

# What about by sex?
LineSummary <- summarySE(PupalDevRate_raw, measurevar="TotalPupalDevTime", groupvars=c("Experiment", "Treatment", "Sex"), na.rm = TRUE); LineSummary
# Male
100-(169.8353         /173.9412           *100) # R1: 2.36051%
100-(168.4600          /174.8879          *100) # R1: 3.67544%
100-(171.7895         /168.4789        *100) # R1: -1.96499%

# Female
100-(189.5625          /179.6818            *100) # R1: -5.498999%
100-(176.6923           /174.2143           *100) # R1: -1.422386%
100-(174.3000          /173.2188         *100) # R1: -0.6241817%


# Across all replicates:
LineSummary <- summarySE(PupalDevRate_raw, measurevar="TotalPupalDevTime", groupvars=c("Treatment", "Sex"), na.rm = TRUE); LineSummary
# Male
100-(170.7547        /172.0431          *100) # R1: 0.7488821%
# Female
100-(185.8851        /175.6639           *100) # R1: -5.818612%


control <- subset(PupalDevRate_raw, Treatment == "Control"); hist(control$TotalPupalDevTime) # Replace the dots with your control group data
large <- subset(PupalDevRate_raw, Treatment == "Large"); hist(large$TotalPupalDevTime) # Replace the dots with your large group data

control_Male <- subset(control, Sex == "M"); hist(control_Male$TotalPupalDevTime)
control_Female <- subset(control, Sex == "F"); hist(control_Female$TotalPupalDevTime)

large_Male <- subset(large, Sex == "M"); hist(large_Male$TotalPupalDevTime)
large_Female <- subset(large, Sex == "F"); hist(large_Female$TotalPupalDevTime)

result1 <- shapiro.test(control_Male$TotalPupalDevTime); hist(control_Male$TotalPupalDevTime); result1
result2 <- shapiro.test(control_Female$TotalPupalDevTime); hist(control_Female$TotalPupalDevTime); result2
result3 <- shapiro.test(large_Male$TotalPupalDevTime); hist(large_Male$TotalPupalDevTime); result3
result4 <- shapiro.test(large_Female$TotalPupalDevTime); hist(large_Female$TotalPupalDevTime); result4
# All significant & no tranformations work

# Male
Male <- subset(PupalDevRate_raw, Sex == "M"); hist(Male$TotalPupalDevTime)
res <- wilcox.test(TotalPupalDevTime ~ Treatment, data = Male,
                   exact = FALSE); res
# W = 23356, p-value = 0.4282

# Female
Female <- subset(PupalDevRate_raw, Sex == "F"); hist(Female$TotalPupalDevTime)
res <- wilcox.test(TotalPupalDevTime ~ Treatment, data = Female,
                   exact = FALSE); res
# W = 1844, p-value = 0.001501



PupalDevRate_raw <- read.csv("PupalDevelopment_v1.csv", header = T, sep = ",")

Female_pupa <-subset(PupalDevRate_raw, Sex == "F")
Male_pupa <-subset(PupalDevRate_raw, Sex == "M")

Female_pupa_sum <- summarySE(Female_pupa, measurevar="weight_mg", groupvars=c("Treatment", "Sex"), na.rm = TRUE)
Female_pupa_sum

Male_pupa_sum <- summarySE(Male_pupa, measurevar="weight_mg", groupvars=c("Treatment", "Sex"), na.rm = TRUE)
Male_pupa_sum

100-(92.21383/104.9659*100) # Control: 12.14877
100-(115.38762/143.1199*100) # Large: 19.37696

Female_pupa_sum <- summarySE(Female_pupa, measurevar="weight_mg", groupvars=c("Sex"), na.rm = TRUE)
Female_pupa_sum

Male_pupa_sum <- summarySE(Male_pupa, measurevar="weight_mg", groupvars=c("Sex"), na.rm = TRUE)
Male_pupa_sum
100-(103.9377/127.3943*100) # Across all: 18.4126


# Trait 09: Ovary weight (mg)-----------------------
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/raw_data/")
Ovary_raw <- read.csv("./Data_R1R2R3_PupalDevRate_OvaryWeight_v1.csv", header = T, sep = ",")
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/analysis/draft_analysis/ovary/")

# Order data:
Ovary_raw <- Ovary_raw %>%    
  mutate(Experiment = fct_relevel(Experiment, 
                                  "R1", "R2", "R3")) # Order data by Replicate
Ovary_raw <- Ovary_raw %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control","Large")) # Order data by Line
Ovary_raw <- Ovary_raw %>%    
  mutate(DOL = fct_relevel(DOL, 
                           "17DOL","18DOL","19DOL","20DOL", "21DOL"))  # Order data by DOL

LineSummary <- summarySE(Ovary_raw, measurevar="Ovary_weight_mg", groupvars=c("Experiment", "Treatment"), na.rm = TRUE)
LineSummary

control <- subset(Ovary_raw, Treatment == "Control")  # Replace the dots with your control group data
large <- subset(Ovary_raw, Treatment == "Large") # Replace the dots with your large group data

result1 <- shapiro.test(control$Ovary_weight_mg); hist(control$Ovary_weight_mg); result1
result2 <- shapiro.test(large$Ovary_weight_mg); hist(large$Ovary_weight_mg); result2

control$Ovary_weight_mgTrans <- posSQRTTransform(control$Ovary_weight_mg)
large$Ovary_weight_mgTrans <- posSQRTTransform(large$Ovary_weight_mg)
result1 <- shapiro.test(control$Ovary_weight_mgTrans); hist(control$Ovary_weight_mgTrans); result1
result2 <- shapiro.test(large$Ovary_weight_mgTrans); hist(large$Ovary_weight_mgTrans); result2

t.test(control$Ovary_weight_mgTrans, large$Ovary_weight_mgTrans)
# t = -1.4759, df = 102.94, p-value = 0.143
# (T-test, t103=-1.4759,p = 0.143) ns

# Plot:
Ovary_weight_pp <- ggplot(Ovary_raw, aes(x=Treatment, y=Ovary_weight_mg, fill = Treatment)) + 
  geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Ovary weight (mg)") + 
  theme_bw() + theme(legend.position="none") +
  stat_compare_means(method = "t.test", aes(label = ..p.signif..), 
                     label.x = 1.5); Ovary_weight_pp

# How much change is the large line?
LineSummary <- summarySE(Ovary_raw, measurevar="Ovary_weight_mg", groupvars=c("Experiment", "Treatment"), na.rm = TRUE); LineSummary
100-(1.935000      /4.105000         *100) # R1: +52.86236%
100-(2.088333        /2.786154        *100) # R1: +25.04603%
100-(5.290323       /5.322222       *100) # R1: +0.5993549%

LineSummary <- summarySE(Ovary_raw, measurevar="Ovary_weight_mg", groupvars=c("Treatment"), na.rm = TRUE); LineSummary
100-(3.775965      /4.198431          *100) # R1: +10.06247%

# Make figure with raw values with by trait-----------------------
ggarrange(GrowthRate_pp, Larvalweight_pp, Prepupalweight_pp, 
          Pupalweight_pp, Pupalsize_pp, Protein_pp,
          Proportion_pp, PupalDevRate_pp, Ovary_weight_pp)
# Manually edit to add p-vals

# Make figure with all 10 traits and % change-----------------------
setwd("E:\\OneDrive - University of Cambridge\\1. Papers!!!\\202307 BSF Selection Line Paper\\2nd\\replot\\analysis\\Trait_interactions_plot")
PercChangeData1 <- read.csv("development_rate.csv", header = T, sep = ",") # performance, nutition and conversion raw data

PercChangeData1 <- PercChangeData1 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Larval growth rate (mg/day)", "Larval development rate (5% PP; Days)", 
                             "Male pupal development rate (hours)", "Female pupal development rate (hours)"))

LineSummary1 <- summarySE(PercChangeData1, measurevar="PerChange", groupvars=c("Trait", "Line"), na.rm = TRUE)
LineSummary1

pd1 <- position_dodge(0.5) # move them .05 to the left and right
ggplot(LineSummary1, aes(x=Trait, y=PerChange, col = Trait)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_line(position=pd, size=1.25, alpha=0.5) +
  xlab("Life History Trait") +
  ylab("Percentage difference (Control/Selected; %)") +
  geom_errorbar(aes(ymin=PerChange-ci, ymax=PerChange+ci), colour="black", width=.2, position=pd1) +
  scale_colour_manual(values=c( "#009E73", "#009E73", "#009E73", "#D55E00")) +
  geom_boxplot(notch=F) + # 21 is filled circle
  ggtitle("Percentage change in development rate") +
  theme_bw() + theme(legend.position="none") + 
  ylim(-50,50) +
  theme(axis.text.x = element_text(angle = 30))

ggplot(LineSummary1, aes(x=Trait, y=PerChange, fill = Trait)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_boxplot(notch=F) + 
  #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Percentage difference (Control/Selected; %)") + 
  theme_bw() + theme(legend.position="none") +
  ylim(-55,55) +
  scale_fill_manual(values=c("#009E73", "#009E73", "#D55E00", "#009E73")) +
  theme(axis.text.x = element_text(angle = 45))


PercChangeData2 <- read.csv("size_weight.csv", header = T, sep = ",") # performance, nutition and conversion raw data

PercChangeData2 <- PercChangeData2 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Larval weight (12 DOL; mg)", "Prepupal weight (21 DOL; mg)",  
                             "Pupal weight (mg)", "F7 Pupal size (cm^2)"))

LineSummary2 <- summarySE(PercChangeData2, measurevar="PerChange", groupvars=c("Trait", "Line"), na.rm = TRUE)
LineSummary2

pd2 <- position_dodge(0.5) # move them .05 to the left and right
ggplot(LineSummary2, aes(x=Trait, y=PerChange, col = Trait)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_line(position=pd, size=1.25, alpha=0.5) +
  xlab("Life History Trait") +
  ylab("Percentage difference (Control/Selected; %)") +
  geom_errorbar(aes(ymin=PerChange-ci, ymax=PerChange+ci), colour="black", width=.2, position=pd) +
  scale_colour_manual(values=c("#009E73", "#009E73", "#009E73", "#009E73")) +
  geom_point(position=pd, size=7, shape=20) + # 21 is filled circle
  ggtitle("Percentage change in size and weight") +
  theme_bw() + theme(legend.position="none") + 
  ylim(-10,50) +
  theme(axis.text.x = element_text(angle = 30))


PercChangeData3 <- read.csv("nut_fecundity.csv", header = T, sep = ",") # performance, nutition and conversion raw data

PercChangeData3 <- PercChangeData3 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Protein content (mg/L)", "Ovary weight (mg)"))

LineSummary3 <- summarySE(PercChangeData3, measurevar="PerChange", groupvars=c("Trait", "Line"), na.rm = TRUE)
LineSummary3

pd3 <- position_dodge(0.5) # move them .05 to the left and right
ggplot(LineSummary3, aes(x=Trait, y=PerChange, col = Trait)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_line(position=pd, size=1.25, alpha=0.5) +
  xlab("Life History Trait") +
  ylab("Percentage difference (Control/Selected; %)") +
  geom_errorbar(aes(ymin=PerChange-ci, ymax=PerChange+ci), colour="black", width=.2, position=pd) +
  scale_colour_manual(values=c("#009E73", "#009E73")) +
  geom_point(position=pd, size=7, shape=20) + # 21 is filled circle
  ggtitle("Percentage change in nutrition and fecundity") +
  theme_bw() + theme(legend.position="none") + 
  ylim(-50,100) +
  theme(axis.text.x = element_text(angle = 30))

# Plot boxplot version:
library(dplyr)
library(stringi)
library(stringr)
setwd("E:\\OneDrive - University of Cambridge\\1. Papers!!!\\202307 BSF Selection Line Paper\\2nd\\replot\\analysis\\Trait_interactions_plot")

PercChangeData1 <- read.csv("development_rate.csv", header = T, sep = ",")

# Order data:
PercChangeData1 <- PercChangeData1 %>%    
  mutate(Replicate = fct_relevel(Rep, 
                                 "R1", "R2", "R3"))         # Order data by Replicate
PercChangeData1 <- PercChangeData1 %>%    
  mutate(Line = fct_relevel(Line, 
                            "Control", "Large"))         # Order data by Line

PercChangeData1 <- PercChangeData1 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Larval growth rate (mg/day)", "Larval development time (5% PP; Days)", 
                             "Male pupal development time (hours)", "Female pupal development time (hours)"))

ggplot(PercChangeData1, aes(x=Trait, y=PerChange, fill = Trait)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_boxplot(notch=F) + 
  #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Percentage difference (Control/Selected; %)") + 
  theme_bw() + theme(legend.position="none") +
  ylim(-10,30) +
  scale_fill_manual(values=c("#A64170", "#A64170", "#A64170", "#F0BD0D")) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_x_discrete(labels=function(Trait) str_wrap(Trait, width=15, indent = 0)) +
  theme(axis.text.y = element_text(lineheight = 0.7, 
                                   size = 12)) + 
  annotate("text", x="Larval growth rate (mg/day)", y=29.5, label="***", size=4) + 
  annotate("text", x="Larval development time (5% PP; Days)", y=29.5, label="***", size=4) + 
  annotate("text", x="Male pupal development time (hours)", y=30, label="ns", size=4) + 
  annotate("text", x="Female pupal development time (hours)", y=29.5, label="**", size=4)




PercChangeData2 <- read.csv("size_weight.csv", header = T, sep = ",")

# Order data:
PercChangeData2 <- PercChangeData2 %>%    
  mutate(Replicate = fct_relevel(Rep, 
                                 "R1", "R2", "R3"))         # Order data by Replicate
PercChangeData2 <- PercChangeData2 %>%    
  mutate(Line = fct_relevel(Line, 
                            "Control", "Large"))         # Order data by Line

PercChangeData2 <- PercChangeData2 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Larval weight (12 DOL; mg)", "Prepupal weight (21 DOL; mg)",  
                             "Pupal weight (mg)", "F7 Pupal size (cm^2)"))

ggplot(PercChangeData2, aes(x=Trait, y=PerChange, fill = Trait)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_boxplot(notch=F) + 
  #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Percentage difference (Control/Selected; %)") + 
  theme_bw() + theme(legend.position="none") +
  ylim(-5,30) +
  scale_fill_manual(values=c("#A64170", "#A64170", "#A64170", "#A64170")) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_x_discrete(labels=function(Trait) str_wrap(Trait, width=15, indent = 0)) +
  theme(axis.text.y = element_text(lineheight = 0.7, 
                                   size = 12)) + 
  annotate("text", x="Larval weight (12 DOL; mg)", y=29.5, label="***", size=4) + 
  annotate("text", x="Prepupal weight (21 DOL; mg)", y=29.5, label="***", size=4) + 
  annotate("text", x="Pupal weight (mg)", y=29.5, label="***", size=4) + 
  annotate("text", x="F7 Pupal size (cm^2)", y=29.5, label="**", size=4)



PercChangeData3 <- read.csv("nut_fecundity.csv", header = T, sep = ",")

# Order data:
PercChangeData3 <- PercChangeData3 %>%    
  mutate(Replicate = fct_relevel(Rep, 
                                 "R1", "R2", "R3"))         # Order data by Replicate
PercChangeData3 <- PercChangeData3 %>%    
  mutate(Line = fct_relevel(Line, 
                            "Control", "Large"))         # Order data by Line

PercChangeData3 <- PercChangeData3 %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Protein content (mg/L)", "Ovary weight (mg)"))

ggplot(PercChangeData3, aes(x=Trait, y=PerChange, fill = Trait)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_boxplot(notch=F) + 
  #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Percentage difference (Control/Selected; %)") + 
  theme_bw() + theme(legend.position="none") +
  ylim(-5,30) +
  scale_fill_manual(values=c("#A64170", "#A64170")) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_x_discrete(labels=function(Trait) str_wrap(Trait, width=15, indent = 0)) +
  theme(axis.text.y = element_text(lineheight = 0.7, 
                                   size = 12)) + 
  annotate("text", x="Protein content (mg/L)", y=29.5, label="*", size=4) + 
  annotate("text", x="Ovary weight (mg)", y=30, label="ns", size=4)


















PercChangeData <- PercChangeData %>%    
  mutate(Trait = fct_relevel(Trait, 
                             "Larval growth rate (mg/day)", "Larval weight (12 DOL; mg)", "Prepupal weight (21 DOL; mg)",  
                             "Pupal weight (mg)", "F7 Pupal size (cm^2)",
                             "Protein content (mg/L)", "Larval development rate (5% PP; Days)", 
                             "Male pupal development rate (hours)", "Female pupal development rate (hours)",
                             "Ovary weight (mg)"))

ggplot(PercChangeData, aes(x=Trait, y=PerChange, fill = Trait)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1.25, alpha =0.4) +
  geom_boxplot(notch=F) + 
  #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Percentage difference (Control/Selected; %)") + 
  theme_bw() + theme(legend.position="none") +
  ylim(-55,55) +
  scale_fill_manual(values=c("#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#009E73", "#D55E00", "#009E73")) +
  theme(axis.text.x = element_text(angle = 45))

PupalDevRate_R2_M <- subset(PupalDevRate_raw, Experiment == "R2", 
                            Sex == "M")
PupalDevRate_R2_F <- subset(PupalDevRate_raw, Experiment == "R2", 
                               Sex == "F")


PupalDevRate_F <- subset(PupalDevRate_raw, Sex == "F", na.rm=T)
library(plyr)
cdat <- ddply(PupalDevRate_F, "Treatment", summarise, weight_mg.mean=mean(weight_mg), na.rm=T)
cdat
ggplot(PupalDevRate_F, aes(x=weight_mg, fill=Treatment)) + 
  geom_histogram(alpha=0.5, position="identity") + 
  geom_vline(cdat, aes(xintercept=weight_mg.mean, colour=Treatment, na.rm=T),   # Ignore NA values for mean
            linetype="dashed", size=1)
