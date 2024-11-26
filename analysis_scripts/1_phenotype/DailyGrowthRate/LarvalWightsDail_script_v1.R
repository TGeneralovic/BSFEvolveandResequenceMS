## Daily larval weights:
library(ggplot2) # plotting
library(dplyr) # data re ordering
library(forcats) # releveling
library(writexl) # write excel file
library(ggpubr) # ggdendisty plots
library(Rmisc) # for summary SE table making
library(ggside) # plot side histos
library(lmerTest) # lmer mixed models 
library(emmeans) # post-hoc analysis
library(tidyverse) # reformat data
library(lme4) # mixed effect models
library(car) # Anova function 

options('max.print' = 100000)   # or whatever value you want
getOption('max.print')

# Larval data:
setwd("D:/2019-09_Thesis/ChapterV-GenomicImprovement-EvolReseq/Chapter_drafts/version_2/analysis/DailyGrowthRate")
data = read.csv("./Data_R1R2R3_LarvalDailyGrowthRates_v1.csv")
summary(data)

# Order data:
data <- data %>%    
  mutate(Experiment = fct_relevel(Experiment, 
                                  "R1", "R2", "R3")) # Order data by Replicate
data <- data %>%    
  mutate(Treatment = fct_relevel(Treatment, 
                                 "Control","Large")) # Order data by Line
data <- data %>%    
  mutate(DOL2 = fct_relevel(DOL2, 
                            "5DOL","6DOL","7DOL","8DOL","9DOL","10DOL","11DOL","12DOL","13DOL","14DOL","15DOL","16DOL","17DOL","18DOL","19DOL","20DOL"))  # Order data by DOL

# Make summary table:
LineSummary <- summarySE(data, measurevar="Weight_mg", groupvars=c("Experiment", "Treatment", "DOL2"), na.rm = TRUE)
LineSummary

# Fit the mixed-design ANOVA model/ Linear mixed-effect model:
model = lmer(Weight_mg ~ Treatment * DOL2 + (1|Experiment) + (1|Tray:Experiment), data=data)
summary(model)
Anova(model, type="III")
AOV1 <- anova(model); AOV1 # Get P-vals for variables

#                   SumSq     MeanSq  NumDF DenDF   F value     Pr(>F)    
#   Treatment       56642     56642   1     9.0     46.1065     7.997e-05 ***
#   DOL2            29390483  1959366 15    7558.4  1594.9144   < 2.2e-16 ***
#   Treatment:DOL2  112172    7478    15    7558.7  6.0872      6.917e-13 ***
  
summary(model); plot(model)
269.92/(269.92 + 49.23 + 1228.51)*100 # Experiment accounts for 17.44% variation
49.23/(269.92 + 49.23 + 1228.51)*100 # Experiment accounts for 3.18% variation

# Calculate Tukey's HSD
emm_options(pbkrtest.limit = 7601)
# emm = emmeans(model, ~ Treatment | DOL2) # Takes an age to run due to so many combos
# Perform Tukey's HSD post-hoc test
pairs(emm, adjust="tukey")
emmeans(model, list(pairwise ~ Treatment*DOL2), adjust = "tukey")
#       CTRL - LARGE
# Day5-  0.9892
# Day6-  0.4868
# Day7-  0.0018
# Day8-  0.1756
# Day9-  0.0003
# Day10- <0.0001
# Day11- 0.0439
# Day12- 0.0016
# Day13- 0.0004
# Day14- 0.0012
# Day15- 0.005
# Day16- 0.0034
# Day17- 0.0061
# Day18- 0.0003
# Day19- 0.001
# Day20- 0.0034

# Plot normal quantile-quantile (Q-Q) plot
qqnorm(residuals(model))
qqline(residuals(model))

# Test for normality of residuals
shapiro.test(residuals(model))
# Aggregate the data by taking the mean of each combination of Treatment, day, and experiment
aggregated_data <- aggregate(data$Weight_mg, by = list(data$Treatment, data$DOL2, data$Experiment), FUN = mean)
# Rename the columns of the aggregated data for clarity
colnames(aggregated_data) <- c("Treatment", "day", "experiment", "mean_weight")
# Fit the model to the aggregated data
model_fit_aggregated = lmer(mean_weight ~ Treatment * day + (1|experiment), data=aggregated_data)
# Check the assumptions of the model fit to the aggregated data
shapiro.test(residuals(model_fit_aggregated))
# W = 0.99232, p-value = 0.8845 

# Test for equal variances of residuals
plot(fitted(model), residuals(model))
abline(h=0) # non aggregated plot looks ok
leveneTest(residuals(model_fit_aggregated) ~ fitted(model_fit_aggregated))
# Create a residuals vs. fitted values plot
plot(fitted(model_fit_aggregated), residuals(model_fit_aggregated))
abline(h = 0) # non aggregated plot looks good! 
# Residuals are homoscedastic (random scatter of points around zero that does not depend on the fitted values)

# On average, how much bigger are the large larva than the controls across the 15 days of feeding?
LineSummary <- summarySE(data, measurevar="Weight_mg", groupvars=c("Treatment"), na.rm = TRUE)
LineSummary

100 - (121.8595 /144.5407 *100) 
#15.69191% bigger

144.5407/121.8595
# 1.19 times bigger

# Plot:
ggplot(data, aes(x=DOL2, y=Weight_mg, fill = Treatment)) + geom_boxplot(notch=F) + 
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  
  ylab("Body size (cm2)") + 
  theme_bw() +
  stat_compare_means(aes(group = Treatment), label = "p.signif") # change to post-hoc results manually


# GR: 
data$GR <- data$Weight_mg/data$DOL
LineSummary <- summarySE(data, measurevar="GR", groupvars=c("Treatment"), na.rm = TRUE)
LineSummary
100-(8.967817/11.046395*100) # 18.8168%
