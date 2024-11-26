
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

View(DevRate_raw)

LarvalSummary <- summarySE(DevRate_raw, measurevar="larval_prop", groupvars=c("Treatment", "DOL"), na.rm = TRUE)
LarvalSummary

# Two-proportions-z-tests:

# Day 11:
DOL <- subset(DevRate_raw, DOL == "11DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(25, 25), n = c((25+0), (25+0))); res
# X-squared = NaN, df = 1, p-value = NA

# Day 12:
DOL12 <- subset(DevRate_raw, DOL == "12DOL")
C <- subset(DOL12, Treatment == "Control"); summary(C)
L <- subset(DOL12, Treatment == "Large"); summary(L)
res <- prop.test(x = c(243, 174), n = c((243+3), (174+10))); res
# X-squared = 5.0228, df = 1, p-value = 0.02501

# Day 13:
DOL13 <- subset(DevRate_raw, DOL == "13DOL")
DOL13_C <- subset(DOL13, Treatment == "Control"); summary(DOL13_C)
DOL13_L <- subset(DOL13, Treatment == "Large"); summary(DOL13_L)
res <- prop.test(x = c(265, 203), n = c((265+22), (203+34))); res
# X-squared = 5.3896, df = 1, p-value = 0.02026

# Day 14:
DOL <- subset(DevRate_raw, DOL == "14DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(366, 301), n = c((366+19), (301+73))); res
# X-squared = 36.522, df = 1, p-value = 1.51e-09

# Day 15:
DOL <- subset(DevRate_raw, DOL == "15DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(178, 209), n = c((178+37), (209+74))); res
# X-squared = 5.1322, df = 1, p-value = 0.02349

# Day 16:
DOL <- subset(DevRate_raw, DOL == "16DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(138, 140), n = c((138+79), (140+70))); res
# X-squared = 0.31846, df = 1, p-value = 0.5725

# Day 17:
DOL <- subset(DevRate_raw, DOL == "17DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(114, 178), n = c((114+125+2), (178+159+13))); res
# X-squared = 0.58611, df = 1, p-value = 0.4439

# Day 18:
DOL <- subset(DevRate_raw, DOL == "18DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(95, 43), n = c((95+305+4), (43+109+23))); res
# X-squared = 0.028166, df = 1, p-value = 0.8667

# Day 19:
DOL <- subset(DevRate_raw, DOL == "19DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(44, 104), n = c((44+240+9), (104+136+35))); res
# X-squared = 37.104, df = 1, p-value = 1.12e-09

# Day 20:
DOL <- subset(DevRate_raw, DOL == "20DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(56, 64), n = c((56+315+20), (64+109+54))); res
# X-squared = 16.786, df = 1, p-value = 4.183e-05

# Day 21:
DOL <- subset(DevRate_raw, DOL == "21DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(27, 16), n = c((27+193+45), (16+94+45))); res
# X-squared = 8.9572e-32, df = 1, p-value = 1

# Day 22:
DOL <- subset(DevRate_raw, DOL == "22DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(10, 10), n = c((10+184+85), (10+81+83))); res
# X-squared = 0.73073, df = 1, p-value = 0.3926

# Day 23:
DOL <- subset(DevRate_raw, DOL == "23DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(7, 7), n = c((7+52+143), (7+45+119))); res
# X-squared = 0.0019987, df = 1, p-value = 0.9643

# Day 24:
DOL <- subset(DevRate_raw, DOL == "24DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(12, 9), n = c((12+37+164), (9+62+627))); res
# X-squared = 11.816, df = 1, p-value = 0.0005871

# Day 25:
DOL <- subset(DevRate_raw, DOL == "25DOL")
C <- subset(DOL, Treatment == "Control"); summary(C)
L <- subset(DOL, Treatment == "Large"); summary(L)
res <- prop.test(x = c(4, 4), n = c((4+43+152), (4+26+99))); res
# X-squared = 0.067159, df = 1, p-value = 0.7955




# Larval- all Replicates
LarvalSummary <- summarySE(DevRate_raw, measurevar="larval_prop", groupvars=c("Treatment", "DOL"), na.rm = TRUE)
LarvalSummary
pd <- position_dodge(0) # move them .05 to the left and right
ggplot(LarvalSummary, aes(x=DOL, y=larval_prop, colour=Treatment, group=Treatment)) + 
  geom_line(position=pd, size=1.25, alpha=0.5) +
  geom_vline(xintercept=9, linetype="dashed", color = "#E69F00", size=1.25, alpha =0.4) + # 13 days larval @ 95%
  geom_vline(xintercept=10, linetype="dashed", color = "#56B4E9", size=1.25, alpha =0.4) + # 14 days larval @ 95%
  #geom_vline(xintercept=9, linetype="dashed", color = "#E69F00", size=1.25, alpha =0.4) + # 17 days larval @ 50%
  #geom_vline(xintercept=10, linetype="dashed", color = "#56B4E9", size=1.25, alpha =0.4) + # 17 days larval @ 50%
  xlab("Day Old Larva (DOL)") +
  ylab("Development (%)") +
  geom_errorbar(aes(ymin=larval_prop-ci, ymax=larval_prop+ci), colour="black", width=.2, position=pd) +
  scale_colour_manual(values=c("#56B4E9", "#E69F00")) +
  geom_point(position=pd, size=3, shape=20) + # 21 is filled circle
  theme_bw() +
  theme(legend.justification=c(1,-5),
        legend.position=c(1,0)) # Position legend in bottom right



