#random sampling and stats for bgluc #thrall
rm(list = ls())
library(dplyr)
library(beanplot)
library(ggplot2)
library(broom)
library(nlme)
library(lme4)
library(multcomp)
library(lmerTest)
library(emmeans)
setwd("C:/Users/ayushg7.AUTH/Dropbox/Ayush_TAMU/enzyme study/jacob/R/input_csv")

#importing inrow and between row data separately 
#have to manually put the plot number before importing

# poxc_ir<- read.csv("poxc_inrow.csv")
# poxc_br <- read.csv ("poxc_betweenrow.csv")

data <- read.csv ("EnzymeCompiledData_june242021.csv")

colnames(data) <- c("sampleid",
                    "location",
                    "plotno",
                    "Tillage",
                    "Crop.Type",
                    "rowposition",
                    "rep",
                    "bgluc",
                    "nag",
                    "phos")

data <- data[which(data$location == "Thrall"),]
data1 <- data[,-c(2,7,9,10)]

#data$Plot.no. <- as.character(data$Plot.no.)
#data$bgluc <- as.numeric(data$bgluc)
#data$nag <- as.numeric (data$nag)
data1$bgluc<- as.numeric(as.character(data1$bgluc))
data1 <- droplevels(data1)
data1 <- na.omit(data1)
data_inrow <- data1[which(data1$rowposition == "In Row"),]
data_betrow <- data1[which(data1$rowposition == "Out Row"),]
#data$plotno <- as.factor(data$plotno)
#seperating into specific treatments
#inrow
nt_f_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Fallow"),]
nt_s_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Sorghum"),]
nt_m_ir <- data_inrow[which(data_inrow$Tillage == "NT" & data_inrow$Crop.Type == "Cover Crop"),]
ct_f_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Fallow"),]
ct_s_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Sorghum"),]
ct_m_ir <- data_inrow[which(data_inrow$Tillage == "CT" & data_inrow$Crop.Type == "Cover Crop"),]

#betweenrow
nt_f_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Fallow"),]
nt_s_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Sorghum"),]
nt_m_br <- data_betrow[which(data_betrow$Tillage == "NT" & data_betrow$Crop.Type == "Cover Crop"),]
ct_f_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Fallow"),]
ct_s_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Sorghum"),]
ct_m_br <- data_betrow[which(data_betrow$Tillage == "CT" & data_betrow$Crop.Type == "Cover Crop"),]

#separating into each plot
#inrow
nt_f_ir_1 <- nt_f_ir[which(nt_f_ir$plotno == "14"),]
nt_f_ir_2 <- nt_f_ir[which(nt_f_ir$plotno == "26"),]
nt_f_ir_3 <- nt_f_ir[which(nt_f_ir$plotno == "46"),]

nt_s_ir_1 <- nt_s_ir[which(nt_s_ir$plotno == "13"),]
nt_s_ir_2 <- nt_s_ir[which(nt_s_ir$plotno == "25"),]
nt_s_ir_3 <- nt_s_ir[which(nt_s_ir$plotno == "44"),]

nt_m_ir_1 <- nt_m_ir[which(nt_m_ir$plotno == "18"),]
nt_m_ir_2 <- nt_m_ir[which(nt_m_ir$plotno == "27"),]
nt_m_ir_3 <- nt_m_ir[which(nt_m_ir$plotno == "47"),]

ct_f_ir_1 <- ct_f_ir[which(ct_f_ir$plotno == "1"),]
ct_f_ir_2 <- ct_f_ir[which(ct_f_ir$plotno == "24"),]
ct_f_ir_3 <- ct_f_ir[which(ct_f_ir$plotno == "54"),]

ct_s_ir_1 <- ct_s_ir[which(ct_s_ir$plotno == "2"),]
ct_s_ir_2 <- ct_s_ir[which(ct_s_ir$plotno == "23"),]
ct_s_ir_3 <- ct_s_ir[which(ct_s_ir$plotno == "50"),]

ct_m_ir_1 <- ct_m_ir[which(ct_m_ir$plotno == "3"),]
ct_m_ir_2 <- ct_m_ir[which(ct_m_ir$plotno == "21"),]
ct_m_ir_3 <- ct_m_ir[which(ct_m_ir$plotno == "49"),]


#betweenrow
nt_f_br_1 <- nt_f_br[which(nt_f_br$plotno == "14"),]
nt_f_br_2 <- nt_f_br[which(nt_f_br$plotno == "26"),]
nt_f_br_3 <- nt_f_br[which(nt_f_br$plotno == "46"),]

nt_s_br_1 <- nt_s_br[which(nt_s_br$plotno == "13"),]
nt_s_br_2 <- nt_s_br[which(nt_s_br$plotno == "25"),]
nt_s_br_3 <- nt_s_br[which(nt_s_br$plotno == "44"),]

nt_m_br_1 <- nt_m_br[which(nt_m_br$plotno == "18"),]
nt_m_br_2 <- nt_m_br[which(nt_m_br$plotno == "27"),]
nt_m_br_3 <- nt_m_br[which(nt_m_br$plotno == "47"),]

ct_f_br_1 <- ct_f_br[which(ct_f_br$plotno == "1"),]
ct_f_br_2 <- ct_f_br[which(ct_f_br$plotno == "24"),]
ct_f_br_3 <- ct_f_br[which(ct_f_br$plotno == "54"),]

ct_s_br_1 <- ct_s_br[which(ct_s_br$plotno == "2"),]
ct_s_br_2 <- ct_s_br[which(ct_s_br$plotno == "23"),]
ct_s_br_3 <- ct_s_br[which(ct_s_br$plotno == "50"),]

ct_m_br_1 <- ct_m_br[which(ct_m_br$plotno == "3"),]
ct_m_br_2 <- ct_m_br[which(ct_m_br$plotno == "21"),]
ct_m_br_3 <- ct_m_br[which(ct_m_br$plotno == "49"),]


defaultW <- getOption("warn")
options(warn = -1)

#inrow###########################################################
################################################################

#sample 1#########################################################################################

Tillage_1 <- c (rep ("nt",9), rep ("ct", 9))
Tillage_1 <- as.factor(Tillage_1)
cover_type_1 <- c(rep("fallow",3), rep("sorghum",3), rep("mix",3),
                  
                  rep("fallow",3), rep("sorghum",3), rep("mix",3))
cover_type_1 <- as.factor(cover_type_1)
Rep <- rep((1:3), 6)
Rep <- as.factor(Rep)

# sample1_trt <- data.frame (tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep )
# sample1_trt$Rep <- as.factor(sample1_trt$Rep)


sample1 <- matrix(ncol = 10000, nrow = 18)
sample1_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample1[1, i] = sample (nt_f_ir_1$bgluc, 1, replace = T)
  sample1[2, i] = sample (nt_f_ir_2$bgluc, 1, replace = T)
  sample1[3, i] = sample (nt_f_ir_3$bgluc, 1, replace = T)
  sample1[4, i] = sample (nt_s_ir_1$bgluc, 1, replace = T)
  sample1[5, i] = sample (nt_s_ir_2$bgluc, 1, replace = T)
  sample1[6, i] = sample (nt_s_ir_3$bgluc, 1, replace = T)
  sample1[7, i] = sample (nt_m_ir_1$bgluc, 1, replace = T)
  sample1[8, i] = sample (nt_m_ir_2$bgluc, 1, replace = T)
  sample1[9, i] = sample (nt_m_ir_3$bgluc, 1, replace = T)
  
  sample1[10, i] = sample (ct_f_ir_1$bgluc, 1, replace = T)
  sample1[11, i] = sample (ct_f_ir_2$bgluc, 1, replace = T)
  sample1[12, i] = sample (ct_f_ir_3$bgluc, 1, replace = T)
  sample1[13, i] = sample (ct_s_ir_1$bgluc, 1, replace = T)
  sample1[14, i] = sample (ct_s_ir_2$bgluc, 1, replace = T)
  sample1[15, i] = sample (ct_s_ir_3$bgluc, 1, replace = T)
  sample1[16, i] = sample (ct_m_ir_1$bgluc, 1, replace = T)
  sample1[17, i] = sample (ct_m_ir_2$bgluc, 1, replace = T)
  sample1[18, i] = sample (ct_m_ir_3$bgluc, 1, replace = T)
  
  sample_1 <- data.frame (bgluc = sample1[,i], tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_1,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample1_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample1_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample1_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample1_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample1_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample1_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample1_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample1_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample1_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample1_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample1_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample1_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample1_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample1_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample1_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample1_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample1_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample1_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample1_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample1_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample1_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}

#sample2########################################################################################
Tillage_2 <- c (rep ("nt",18), rep ("ct", 18))
Tillage_2 <- as.factor(Tillage_2)
cover_type_2 <- c(rep("fallow",6), rep("sorghum",6), rep("mix",6),
                  
                  rep("fallow",6), rep("sorghum",6), rep("mix",6))
cover_type_2 <- as.factor(cover_type_2)
Rep <- rep(c(1,1,2,2,3,3), 6)
Rep <- as.factor(Rep)

# sample2_trt <- data.frame (tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep )
# sample2_trt$Rep <- as.factor(sample2_trt$Rep)


sample2 <- matrix(ncol = 10000, nrow = 36)
sample2_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample2[1:2, i] = sample (nt_f_ir_1$bgluc, 2, replace = T)
  sample2[3:4, i] = sample (nt_f_ir_2$bgluc, 2, replace = T)
  sample2[5:6, i] = sample (nt_f_ir_3$bgluc, 2, replace = T)
  sample2[7:8, i] = sample (nt_s_ir_1$bgluc, 2, replace = T)
  sample2[9:10, i] = sample (nt_s_ir_2$bgluc, 2, replace = T)
  sample2[11:12, i] = sample (nt_s_ir_3$bgluc, 2, replace = T)
  sample2[13:14, i] = sample (nt_m_ir_1$bgluc, 2, replace = T)
  sample2[15:16, i] = sample (nt_m_ir_2$bgluc, 2, replace = T)
  sample2[17:18, i] = sample (nt_m_ir_3$bgluc, 2, replace = T)
  
  sample2[19:20, i] = sample (ct_f_ir_1$bgluc, 2, replace = T)
  sample2[21:22, i] = sample (ct_f_ir_2$bgluc, 2, replace = T)
  sample2[23:24, i] = sample (ct_f_ir_3$bgluc, 2, replace = T)
  sample2[25:26, i] = sample (ct_s_ir_1$bgluc, 2, replace = T)
  sample2[27:28, i] = sample (ct_s_ir_2$bgluc, 2, replace = T)
  sample2[29:30, i] = sample (ct_s_ir_3$bgluc, 2, replace = T)
  sample2[31:32, i] = sample (ct_m_ir_1$bgluc, 2, replace = T)
  sample2[33:34, i] = sample (ct_m_ir_2$bgluc, 2, replace = T)
  sample2[35:36, i] = sample (ct_m_ir_3$bgluc, 2, replace = T)
  
  sample_2 <- data.frame (bgluc = sample2[,i], tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_2,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample2_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample2_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample2_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample2_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample2_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample2_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample2_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample2_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample2_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample2_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample2_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample2_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample2_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample2_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample2_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample2_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample2_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample2_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample2_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample2_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample2_pvalue [21, i] <- posthoc_interraction$p.value[15]
}


#sample3########################################################################################
Tillage_3 <- c (rep ("nt",27), rep ("ct", 27))
cover_type_3 <- c(rep("fallow",9), rep("sorghum",9), rep("mix",9),
                  
                  rep("fallow",9), rep("sorghum",9), rep("mix",9))
Rep <- rep(c(1,1,1,2,2,2,3,3,3), 6)
Rep <- as.factor(Rep)

sample3_trt <- data.frame (tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep )
sample3_trt$Rep <- as.factor(sample3_trt$Rep)


sample3 <- matrix(ncol = 10000, nrow = 54)
sample3_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample3[1:3, i] = sample (nt_f_ir_1$bgluc, 3, replace = T)
  sample3[4:6, i] = sample (nt_f_ir_2$bgluc, 3, replace = T)
  sample3[7:9, i] = sample (nt_f_ir_3$bgluc, 3, replace = T)
  sample3[10:12, i] = sample (nt_s_ir_1$bgluc, 3, replace = T)
  sample3[13:15, i] = sample (nt_s_ir_2$bgluc, 3, replace = T)
  sample3[16:18, i] = sample (nt_s_ir_3$bgluc, 3, replace = T)
  sample3[19:21, i] = sample (nt_m_ir_1$bgluc, 3, replace = T)
  sample3[22:24, i] = sample (nt_m_ir_2$bgluc, 3, replace = T)
  sample3[25:27, i] = sample (nt_m_ir_3$bgluc, 3, replace = T)
  
  sample3[28:30, i] = sample (ct_f_ir_1$bgluc, 3, replace = T)
  sample3[31:33, i] = sample (ct_f_ir_2$bgluc, 3, replace = T)
  sample3[34:36, i] = sample (ct_f_ir_3$bgluc, 3, replace = T)
  sample3[37:39, i] = sample (ct_s_ir_1$bgluc, 3, replace = T)
  sample3[40:42, i] = sample (ct_s_ir_2$bgluc, 3, replace = T)
  sample3[43:45, i] = sample (ct_s_ir_3$bgluc, 3, replace = T)
  sample3[46:48, i] = sample (ct_m_ir_1$bgluc, 3, replace = T)
  sample3[49:51, i] = sample (ct_m_ir_2$bgluc, 3, replace = T)
  sample3[52:54, i] = sample (ct_m_ir_3$bgluc, 3, replace = T)
  
  sample_3 <- data.frame (bgluc = sample3[,i], tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_3,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample3_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample3_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample3_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample3_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample3_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample3_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample3_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample3_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample3_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample3_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample3_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample3_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample3_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample3_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample3_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample3_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample3_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample3_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample3_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample3_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample3_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample4########################################################################################
Tillage_4 <- c (rep ("nt",36), rep ("ct", 36))
cover_type_4 <- c(rep("fallow",12), rep("sorghum",12), rep("mix",12),
                  
                  rep("fallow",12), rep("sorghum",12), rep("mix",12))
Rep <- rep(c(1,1,1,1,2,2,2,2,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample4_trt <- data.frame (tillage = Tillage_4, cover_type = cover_type_4, Rep = Rep )
sample4_trt$Rep <- as.factor(sample4_trt$Rep)


sample4 <- matrix(ncol = 10000, nrow = 72)
sample4_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample4[1:4, i] = sample (nt_f_ir_1$bgluc, 4, replace = T)
  sample4[5:8, i] = sample (nt_f_ir_2$bgluc, 4, replace = T)
  sample4[9:12, i] = sample (nt_f_ir_3$bgluc, 4, replace = T)
  sample4[13:16, i] = sample (nt_s_ir_1$bgluc, 4, replace = T)
  sample4[17:20, i] = sample (nt_s_ir_2$bgluc, 4, replace = T)
  sample4[21:24, i] = sample (nt_s_ir_3$bgluc, 4, replace = T)
  sample4[25:28, i] = sample (nt_m_ir_1$bgluc, 4, replace = T)
  sample4[29:32, i] = sample (nt_m_ir_2$bgluc, 4, replace = T)
  sample4[33:36, i] = sample (nt_m_ir_3$bgluc, 4, replace = T)
  
  sample4[37:40, i] = sample (ct_f_ir_1$bgluc, 4, replace = T)
  sample4[41:44, i] = sample (ct_f_ir_2$bgluc, 4, replace = T)
  sample4[45:48, i] = sample (ct_f_ir_3$bgluc, 4, replace = T)
  sample4[49:52, i] = sample (ct_s_ir_1$bgluc, 4, replace = T)
  sample4[53:56, i] = sample (ct_s_ir_2$bgluc, 4, replace = T)
  sample4[57:60, i] = sample (ct_s_ir_3$bgluc, 4, replace = T)
  sample4[61:64, i] = sample (ct_m_ir_1$bgluc, 4, replace = T)
  sample4[65:68, i] = sample (ct_m_ir_2$bgluc, 4, replace = T)
  sample4[69:72, i] = sample (ct_m_ir_3$bgluc, 4, replace = T)
  
  sample_4 <- data.frame (bgluc = sample4[,i], tillage = Tillage_4, cover_type = cover_type_4, 
                          Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_4,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample4_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample4_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample4_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample4_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample4_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample4_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample4_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample4_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample4_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample4_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample4_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample4_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample4_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample4_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample4_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample4_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample4_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample4_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample4_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample4_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample4_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}
#sample5########################################################################################
Tillage_5 <- c (rep ("nt",45), rep ("ct", 45))
cover_type_5 <- c(rep("fallow",15), rep("sorghum",15), rep("mix",15),
                  
                  rep("fallow",15), rep("sorghum",15), rep("mix",15))
Rep <- rep(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample5_trt <- data.frame (tillage = Tillage_5, cover_type = cover_type_5, Rep = Rep )
sample5_trt$Rep <- as.factor(sample5_trt$Rep)


sample5 <- matrix(ncol = 10000, nrow = 90)
sample5_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample5[1:5, i] = sample (nt_f_ir_1$bgluc, 5, replace = T)
  sample5[6:10, i] = sample (nt_f_ir_2$bgluc, 5, replace = T)
  sample5[11:15, i] = sample (nt_f_ir_3$bgluc, 5, replace = T)
  sample5[16:20, i] = sample (nt_s_ir_1$bgluc, 5, replace = T)
  sample5[21:25, i] = sample (nt_s_ir_2$bgluc, 5, replace = T)
  sample5[26:30, i] = sample (nt_s_ir_3$bgluc, 5, replace = T)
  sample5[31:35, i] = sample (nt_m_ir_1$bgluc, 5, replace = T)
  sample5[36:40, i] = sample (nt_m_ir_2$bgluc, 5, replace = T)
  sample5[41:45, i] = sample (nt_m_ir_3$bgluc, 5, replace = T)
  
  sample5[46:50, i] = sample (ct_f_ir_1$bgluc, 5, replace = T)
  sample5[51:55, i] = sample (ct_f_ir_2$bgluc, 5, replace = T)
  sample5[56:60, i] = sample (ct_f_ir_3$bgluc, 5, replace = T)
  sample5[61:65, i] = sample (ct_s_ir_1$bgluc, 5, replace = T)
  sample5[66:70, i] = sample (ct_s_ir_2$bgluc, 5, replace = T)
  sample5[71:75, i] = sample (ct_s_ir_3$bgluc, 5, replace = T)
  sample5[76:80, i] = sample (ct_m_ir_1$bgluc, 5, replace = T)
  sample5[81:85, i] = sample (ct_m_ir_2$bgluc, 5, replace = T)
  sample5[86:90, i] = sample (ct_m_ir_3$bgluc, 5, replace = T)
  
  sample_5 <- data.frame (bgluc = sample5[,i], tillage = Tillage_5, cover_type = cover_type_5, Rep= Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_5,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample5_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample5_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample5_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample5_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample5_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample5_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample5_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample5_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample5_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample5_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample5_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample5_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample5_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample5_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample5_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample5_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample5_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample5_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample5_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample5_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample5_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}

#sample6########################################################################################
Tillage_6 <- c (rep ("nt",54), rep ("ct", 54))
cover_type_6 <- c(rep("fallow",18), rep("sorghum",18), rep("mix",18),
                  
                  rep("fallow",18), rep("sorghum",18), rep("mix",18))
Rep <- rep(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample6_trt <- data.frame (tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep )
sample6_trt$Rep <- as.factor(sample6_trt$Rep)


sample6 <- matrix(ncol = 10000, nrow = 108)
sample6_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample6[1:6, i] = sample (nt_f_ir_1$bgluc, 6, replace = T)
  sample6[7:12, i] = sample (nt_f_ir_2$bgluc, 6, replace = T)
  sample6[13:18, i] = sample (nt_f_ir_3$bgluc, 6, replace = T)
  sample6[19:24, i] = sample (nt_s_ir_1$bgluc, 6, replace = T)
  sample6[25:30, i] = sample (nt_s_ir_2$bgluc, 6, replace = T)
  sample6[31:36, i] = sample (nt_s_ir_3$bgluc, 6, replace = T)
  sample6[37:42, i] = sample (nt_m_ir_1$bgluc, 6, replace = T)
  sample6[43:48, i] = sample (nt_m_ir_2$bgluc, 6, replace = T)
  sample6[49:54, i] = sample (nt_m_ir_3$bgluc, 6, replace = T)
  
  sample6[55:60, i] = sample (ct_f_ir_1$bgluc, 6, replace = T)
  sample6[61:66, i] = sample (ct_f_ir_2$bgluc, 6, replace = T)
  sample6[67:72, i] = sample (ct_f_ir_3$bgluc, 6, replace = T)
  sample6[73:78, i] = sample (ct_s_ir_1$bgluc, 6, replace = T)
  sample6[79:84, i] = sample (ct_s_ir_2$bgluc, 6, replace = T)
  sample6[85:90, i] = sample (ct_s_ir_3$bgluc, 6, replace = T)
  sample6[91:96, i] = sample (ct_m_ir_1$bgluc, 6, replace = T)
  sample6[97:102, i] = sample (ct_m_ir_2$bgluc, 6, replace = T)
  sample6[103:108, i] = sample (ct_m_ir_3$bgluc, 6, replace = T)
  
  sample_6 <- data.frame (bgluc = sample6[,i], tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_6,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample6_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample6_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample6_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample6_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample6_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample6_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample6_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample6_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample6_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample6_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample6_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample6_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample6_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample6_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample6_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample6_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample6_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample6_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample6_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample6_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample6_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample7########################################################################################
Tillage_7 <- c (rep ("nt",63), rep ("ct", 63))
cover_type_7 <- c(rep("fallow",21), rep("sorghum",21), rep("mix",21),
                  
                  rep("fallow",21), rep("sorghum",21), rep("mix",21))
Rep <- rep(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample7_trt <- data.frame (tillage = Tillage_7, cover_type = cover_type_7, Rep = Rep )
sample7_trt$Rep <- as.factor(sample7_trt$Rep)


sample7 <- matrix(ncol = 10000, nrow = 126)
sample7_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample7[1:7, i] = sample (nt_f_ir_1$bgluc, 7, replace = T)
  sample7[8:14, i] = sample (nt_f_ir_2$bgluc, 7, replace = T)
  sample7[15:21, i] = sample (nt_f_ir_3$bgluc, 7, replace = T)
  sample7[22:28, i] = sample (nt_s_ir_1$bgluc, 7, replace = T)
  sample7[29:35, i] = sample (nt_s_ir_2$bgluc, 7, replace = T)
  sample7[36:42, i] = sample (nt_s_ir_3$bgluc, 7, replace = T)
  sample7[43:49, i] = sample (nt_m_ir_1$bgluc, 7, replace = T)
  sample7[50:56, i] = sample (nt_m_ir_2$bgluc, 7, replace = T)
  sample7[57:63, i] = sample (nt_m_ir_3$bgluc, 7, replace = T)
  
  sample7[64:70, i] = sample (ct_f_ir_1$bgluc, 7, replace = T)
  sample7[71:77, i] = sample (ct_f_ir_2$bgluc, 7, replace = T)
  sample7[78:84, i] = sample (ct_f_ir_3$bgluc, 7, replace = T)
  sample7[85:91, i] = sample (ct_s_ir_1$bgluc, 7, replace = T)
  sample7[92:98, i] = sample (ct_s_ir_2$bgluc, 7, replace = T)
  sample7[99:105, i] = sample (ct_s_ir_3$bgluc, 7, replace = T)
  sample7[106:112, i] = sample (ct_m_ir_1$bgluc, 7, replace = T)
  sample7[113:119, i] = sample (ct_m_ir_2$bgluc, 7, replace = T)
  sample7[120:126, i] = sample (ct_m_ir_3$bgluc, 7, replace = T)
  
  sample_7 <- data.frame (bgluc = sample7[,i], tillage = Tillage_7, cover_type = cover_type_7, Rep=Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_7,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample7_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample7_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample7_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample7_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample7_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample7_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample7_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample7_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample7_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample7_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample7_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample7_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample7_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample7_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample7_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample7_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample7_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample7_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample7_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample7_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample7_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}

#sample8########################################################################################
Tillage_8 <- c (rep ("nt",72), rep ("ct", 72))
cover_type_8 <- c(rep("fallow",24), rep("sorghum",24), rep("mix",24),
                  
                  rep("fallow",24), rep("sorghum",24), rep("mix",24))
Rep <- rep(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample8_trt <- data.frame (tillage = Tillage_8, cover_type = cover_type_8, Rep = Rep )
sample8_trt$Rep <- as.factor(sample8_trt$Rep)


sample8 <- matrix(ncol = 10000, nrow = 144)
sample8_pvalue <- matrix(ncol = 10000, nrow = 6)

for(i in 1:10000) {
  
  
  sample8[1:8, i] = sample (nt_f_ir_1$bgluc, 8, replace = T)
  sample8[9:16, i] = sample (nt_f_ir_2$bgluc, 8, replace = T)
  sample8[17:24, i] = sample (nt_f_ir_3$bgluc, 8, replace = T)
  sample8[25:32, i] = sample (nt_s_ir_1$bgluc, 8, replace = T)
  sample8[33:40, i] = sample (nt_s_ir_2$bgluc, 8, replace = T)
  sample8[41:48, i] = sample (nt_s_ir_3$bgluc, 8, replace = T)
  sample8[49:56, i] = sample (nt_m_ir_1$bgluc, 8, replace = T)
  sample8[57:64, i] = sample (nt_m_ir_2$bgluc, 8, replace = T)
  sample8[65:72, i] = sample (nt_m_ir_3$bgluc, 8, replace = T)
  
  sample8[73:80, i] = sample (ct_f_ir_1$bgluc, 8, replace = T)
  sample8[81:88, i] = sample (ct_f_ir_2$bgluc, 8, replace = T)
  sample8[89:96, i] = sample (ct_f_ir_3$bgluc, 8, replace = T)
  sample8[97:104, i] = sample (ct_s_ir_1$bgluc, 8, replace = T)
  sample8[105:112, i] = sample (ct_s_ir_2$bgluc, 8, replace = T)
  sample8[113:120, i] = sample (ct_s_ir_3$bgluc, 8, replace = T)
  sample8[121:128, i] = sample (ct_m_ir_1$bgluc, 8, replace = T)
  sample8[129:136, i] = sample (ct_m_ir_2$bgluc, 8, replace = T)
  sample8[137:144, i] = sample (ct_m_ir_3$bgluc, 8, replace = T)
  
  sample_8 <- data.frame (bgluc = sample8[,i], tillage = Tillage_8, cover_type = cover_type_8, 
                          Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_8,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample8_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample8_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample8_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample8_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample8_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample8_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample8_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample8_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample8_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample8_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample8_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample8_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample8_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample8_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample8_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample8_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample8_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample8_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample8_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample8_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample8_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}
#sample9########################################################################################
Tillage_9 <- c (rep ("nt",81), rep ("ct", 81))
cover_type_9 <- c(rep("fallow",27), rep("sorghum",27), rep("mix",27),
                  
                  rep("fallow",27), rep("sorghum",27), rep("mix",27))
Rep <- rep(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample9_trt <- data.frame (tillage = Tillage_9, cover_type = cover_type_9, Rep = Rep )
sample9_trt$Rep <- as.factor(sample9_trt$Rep)


sample9 <- matrix(ncol = 10000, nrow = 162)
sample9_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample9[1:9, i] = sample (nt_f_ir_1$bgluc, 9, replace = T)
  sample9[10:18, i] = sample (nt_f_ir_2$bgluc, 9, replace = T)
  sample9[19:27, i] = sample (nt_f_ir_3$bgluc, 9, replace = T)
  sample9[28:36, i] = sample (nt_s_ir_1$bgluc, 9, replace = T)
  sample9[37:45, i] = sample (nt_s_ir_2$bgluc, 9, replace = T)
  sample9[46:54, i] = sample (nt_s_ir_3$bgluc, 9, replace = T)
  sample9[55:63, i] = sample (nt_m_ir_1$bgluc, 9, replace = T)
  sample9[64:72, i] = sample (nt_m_ir_2$bgluc, 9, replace = T)
  sample9[73:81, i] = sample (nt_m_ir_3$bgluc, 9, replace = T)
  
  sample9[82:90, i] = sample (ct_f_ir_1$bgluc, 9, replace = T)
  sample9[91:99, i] = sample (ct_f_ir_2$bgluc, 9, replace = T)
  sample9[100:108, i] = sample (ct_f_ir_3$bgluc, 9, replace = T)
  sample9[109:117, i] = sample (ct_s_ir_1$bgluc, 9, replace = T)
  sample9[118:126, i] = sample (ct_s_ir_2$bgluc, 9, replace = T)
  sample9[127:135, i] = sample (ct_s_ir_3$bgluc, 9, replace = T)
  sample9[136:144, i] = sample (ct_m_ir_1$bgluc, 9, replace = T)
  sample9[145:153, i] = sample (ct_m_ir_2$bgluc, 9, replace = T)
  sample9[154:162, i] = sample (ct_m_ir_3$bgluc, 9, replace = T)
  
  sample_9 <- data.frame (bgluc = sample9[,i], tillage = Tillage_9, cover_type = cover_type_9, Rep=Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_9,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample9_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample9_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample9_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample9_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample9_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample9_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample9_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample9_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample9_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample9_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample9_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample9_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample9_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample9_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample9_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample9_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample9_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample9_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample9_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample9_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample9_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}

#repear again for between row samples############################################
###############################################################################


#sample 1#########################################################################################

Tillage_1 <- c (rep ("nt",9), rep ("ct", 9))
cover_type_1 <- c(rep("fallow",3), rep("sorghum",3), rep("mix",3),
                  
                  rep("fallow",3), rep("sorghum",3), rep("mix",3))
Rep <- rep((1:3), 6)
Rep <- as.factor(Rep)

sample1_trt <- data.frame (tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep )
sample1_trt$Rep <- as.factor(sample1_trt$Rep)


sample1 <- matrix(ncol = 10000, nrow = 18)
sample1_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample1[1, i] = sample (nt_f_br_1$bgluc, 1, replace = T)
  sample1[2, i] = sample (nt_f_br_2$bgluc, 1, replace = T)
  sample1[3, i] = sample (nt_f_br_3$bgluc, 1, replace = T)
  sample1[4, i] = sample (nt_s_br_1$bgluc, 1, replace = T)
  sample1[5, i] = sample (nt_s_br_2$bgluc, 1, replace = T)
  sample1[6, i] = sample (nt_s_br_3$bgluc, 1, replace = T)
  sample1[7, i] = sample (nt_m_br_1$bgluc, 1, replace = T)
  sample1[8, i] = sample (nt_m_br_2$bgluc, 1, replace = T)
  sample1[9, i] = sample (nt_m_br_3$bgluc, 1, replace = T)
  
  sample1[10, i] = sample (ct_f_br_1$bgluc, 1, replace = T)
  sample1[11, i] = sample (ct_f_br_2$bgluc, 1, replace = T)
  sample1[12, i] = sample (ct_f_br_3$bgluc, 1, replace = T)
  sample1[13, i] = sample (ct_s_br_1$bgluc, 1, replace = T)
  sample1[14, i] = sample (ct_s_br_2$bgluc, 1, replace = T)
  sample1[15, i] = sample (ct_s_br_3$bgluc, 1, replace = T)
  sample1[16, i] = sample (ct_m_br_1$bgluc, 1, replace = T)
  sample1[17, i] = sample (ct_m_br_2$bgluc, 1, replace = T)
  sample1[18, i] = sample (ct_m_br_3$bgluc, 1, replace = T)
  
  sample_1 <- data.frame (bgluc = sample1[,i], tillage = Tillage_1, cover_type = cover_type_1, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_1,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample1_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample1_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample1_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample1_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample1_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample1_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample1_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample1_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample1_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample1_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample1_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample1_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample1_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample1_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample1_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample1_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample1_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample1_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample1_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample1_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample1_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}


#sample2########################################################################################
Tillage_2 <- c (rep ("nt",18), rep ("ct", 18))
cover_type_2 <- c(rep("fallow",6), rep("sorghum",6), rep("mix",6),
                  
                  rep("fallow",6), rep("sorghum",6), rep("mix",6))
Rep <- rep(c(1,1,2,2,3,3), 6)
Rep <- as.factor(Rep)

sample2_trt <- data.frame (tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep )
sample2_trt$Rep <- as.factor(sample2_trt$Rep)


sample2 <- matrix(ncol = 10000, nrow = 36)
sample2_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample2[1:2, i] = sample (nt_f_br_1$bgluc, 2, replace = T)
  sample2[3:4, i] = sample (nt_f_br_2$bgluc, 2, replace = T)
  sample2[5:6, i] = sample (nt_f_br_3$bgluc, 2, replace = T)
  sample2[7:8, i] = sample (nt_s_br_1$bgluc, 2, replace = T)
  sample2[9:10, i] = sample (nt_s_br_2$bgluc, 2, replace = T)
  sample2[11:12, i] = sample (nt_s_br_3$bgluc, 2, replace = T)
  sample2[13:14, i] = sample (nt_m_br_1$bgluc, 2, replace = T)
  sample2[15:16, i] = sample (nt_m_br_2$bgluc, 2, replace = T)
  sample2[17:18, i] = sample (nt_m_br_3$bgluc, 2, replace = T)
  
  sample2[19:20, i] = sample (ct_f_br_1$bgluc, 2, replace = T)
  sample2[21:22, i] = sample (ct_f_br_2$bgluc, 2, replace = T)
  sample2[23:24, i] = sample (ct_f_br_3$bgluc, 2, replace = T)
  sample2[25:26, i] = sample (ct_s_br_1$bgluc, 2, replace = T)
  sample2[27:28, i] = sample (ct_s_br_2$bgluc, 2, replace = T)
  sample2[29:30, i] = sample (ct_s_br_3$bgluc, 2, replace = T)
  sample2[31:32, i] = sample (ct_m_br_1$bgluc, 2, replace = T)
  sample2[33:34, i] = sample (ct_m_br_2$bgluc, 2, replace = T)
  sample2[35:36, i] = sample (ct_m_br_3$bgluc, 2, replace = T)
  
  sample_2 <- data.frame (bgluc = sample2[,i], tillage = Tillage_2, cover_type = cover_type_2, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_2,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample2_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample2_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample2_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample2_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample2_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample2_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample2_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample2_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample2_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample2_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample2_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample2_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample2_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample2_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample2_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample2_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample2_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample2_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample2_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample2_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample2_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
  
}


#sample3########################################################################################
Tillage_3 <- c (rep ("nt",27), rep ("ct", 27))
cover_type_3 <- c(rep("fallow",9), rep("sorghum",9), rep("mix",9),
                  
                  rep("fallow",9), rep("sorghum",9), rep("mix",9))
Rep <- rep(c(1,1,1,2,2,2,3,3,3), 6)
Rep <- as.factor(Rep)

sample3_trt <- data.frame (tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep )
sample3_trt$Rep <- as.factor(sample3_trt$Rep)


sample3 <- matrix(ncol = 10000, nrow = 54)
sample3_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample3[1:3, i] = sample (nt_f_br_1$bgluc, 3, replace = T)
  sample3[4:6, i] = sample (nt_f_br_2$bgluc, 3, replace = T)
  sample3[7:9, i] = sample (nt_f_br_3$bgluc, 3, replace = T)
  sample3[10:12, i] = sample (nt_s_br_1$bgluc, 3, replace = T)
  sample3[13:15, i] = sample (nt_s_br_2$bgluc, 3, replace = T)
  sample3[16:18, i] = sample (nt_s_br_3$bgluc, 3, replace = T)
  sample3[19:21, i] = sample (nt_m_br_1$bgluc, 3, replace = T)
  sample3[22:24, i] = sample (nt_m_br_2$bgluc, 3, replace = T)
  sample3[25:27, i] = sample (nt_m_br_3$bgluc, 3, replace = T)
  
  sample3[28:30, i] = sample (ct_f_br_1$bgluc, 3, replace = T)
  sample3[31:33, i] = sample (ct_f_br_2$bgluc, 3, replace = T)
  sample3[34:36, i] = sample (ct_f_br_3$bgluc, 3, replace = T)
  sample3[37:39, i] = sample (ct_s_br_1$bgluc, 3, replace = T)
  sample3[40:42, i] = sample (ct_s_br_2$bgluc, 3, replace = T)
  sample3[43:45, i] = sample (ct_s_br_3$bgluc, 3, replace = T)
  sample3[46:48, i] = sample (ct_m_br_1$bgluc, 3, replace = T)
  sample3[49:51, i] = sample (ct_m_br_2$bgluc, 3, replace = T)
  sample3[52:54, i] = sample (ct_m_br_3$bgluc, 3, replace = T)
  
  sample_3 <- data.frame (bgluc = sample3[,i], tillage = Tillage_3, cover_type = cover_type_3, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_3,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample3_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample3_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample3_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample3_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample3_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample3_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample3_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample3_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample3_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample3_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample3_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample3_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample3_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample3_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample3_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample3_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample3_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample3_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample3_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample3_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample3_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample4########################################################################################
Tillage_4 <- c (rep ("nt",36), rep ("ct", 36))
cover_type_4 <- c(rep("fallow",12), rep("sorghum",12), rep("mix",12),
                  
                  rep("fallow",12), rep("sorghum",12), rep("mix",12))
Rep <- rep(c(1,1,1,1,2,2,2,2,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample4_trt <- data.frame (tillage = Tillage_4, cover_type = cover_type_4, Rep = Rep )
sample4_trt$Rep <- as.factor(sample4_trt$Rep)


sample4 <- matrix(ncol = 10000, nrow = 72)
sample4_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample4[1:4, i] = sample (nt_f_br_1$bgluc, 4, replace = T)
  sample4[5:8, i] = sample (nt_f_br_2$bgluc, 4, replace = T)
  sample4[9:12, i] = sample (nt_f_br_3$bgluc, 4, replace = T)
  sample4[13:16, i] = sample (nt_s_br_1$bgluc, 4, replace = T)
  sample4[17:20, i] = sample (nt_s_br_2$bgluc, 4, replace = T)
  sample4[21:24, i] = sample (nt_s_br_3$bgluc, 4, replace = T)
  sample4[25:28, i] = sample (nt_m_br_1$bgluc, 4, replace = T)
  sample4[29:32, i] = sample (nt_m_br_2$bgluc, 4, replace = T)
  sample4[33:36, i] = sample (nt_m_br_3$bgluc, 4, replace = T)
  
  sample4[37:40, i] = sample (ct_f_br_1$bgluc, 4, replace = T)
  sample4[41:44, i] = sample (ct_f_br_2$bgluc, 4, replace = T)
  sample4[45:48, i] = sample (ct_f_br_3$bgluc, 4, replace = T)
  sample4[49:52, i] = sample (ct_s_br_1$bgluc, 4, replace = T)
  sample4[53:56, i] = sample (ct_s_br_2$bgluc, 4, replace = T)
  sample4[57:60, i] = sample (ct_s_br_3$bgluc, 4, replace = T)
  sample4[61:64, i] = sample (ct_m_br_1$bgluc, 4, replace = T)
  sample4[65:68, i] = sample (ct_m_br_2$bgluc, 4, replace = T)
  sample4[69:72, i] = sample (ct_m_br_3$bgluc, 4, replace = T)
  
  sample_4 <- data.frame (bgluc = sample4[,i], tillage = Tillage_4, cover_type = cover_type_4, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_4,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample4_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample4_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample4_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample4_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample4_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample4_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample4_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample4_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample4_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample4_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample4_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample4_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample4_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample4_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample4_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample4_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample4_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample4_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample4_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample4_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample4_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}
#sample5########################################################################################
Tillage_5 <- c (rep ("nt",45), rep ("ct", 45))
cover_type_5 <- c(rep("fallow",15), rep("sorghum",15), rep("mix",15),
                  
                  rep("fallow",15), rep("sorghum",15), rep("mix",15))
Rep <- rep(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample5_trt <- data.frame (tillage = Tillage_5, cover_type = cover_type_5, Rep = Rep )
sample5_trt$Rep <- as.factor(sample5_trt$Rep)


sample5 <- matrix(ncol = 10000, nrow = 90)
sample5_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample5[1:5, i] = sample (nt_f_br_1$bgluc, 5, replace = T)
  sample5[6:10, i] = sample (nt_f_br_2$bgluc, 5, replace = T)
  sample5[11:15, i] = sample (nt_f_br_3$bgluc, 5, replace = T)
  sample5[16:20, i] = sample (nt_s_br_1$bgluc, 5, replace = T)
  sample5[21:25, i] = sample (nt_s_br_2$bgluc, 5, replace = T)
  sample5[26:30, i] = sample (nt_s_br_3$bgluc, 5, replace = T)
  sample5[31:35, i] = sample (nt_m_br_1$bgluc, 5, replace = T)
  sample5[36:40, i] = sample (nt_m_br_2$bgluc, 5, replace = T)
  sample5[41:45, i] = sample (nt_m_br_3$bgluc, 5, replace = T)
  
  sample5[46:50, i] = sample (ct_f_br_1$bgluc, 5, replace = T)
  sample5[51:55, i] = sample (ct_f_br_2$bgluc, 5, replace = T)
  sample5[56:60, i] = sample (ct_f_br_3$bgluc, 5, replace = T)
  sample5[61:65, i] = sample (ct_s_br_1$bgluc, 5, replace = T)
  sample5[66:70, i] = sample (ct_s_br_2$bgluc, 5, replace = T)
  sample5[71:75, i] = sample (ct_s_br_3$bgluc, 5, replace = T)
  sample5[76:80, i] = sample (ct_m_br_1$bgluc, 5, replace = T)
  sample5[81:85, i] = sample (ct_m_br_2$bgluc, 5, replace = T)
  sample5[86:90, i] = sample (ct_m_br_3$bgluc, 5, replace = T)
  
  sample_5 <- data.frame (bgluc = sample5[,i], tillage = Tillage_5, cover_type = cover_type_5, Rep = Rep)
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_5,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample5_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample5_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample5_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample5_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample5_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample5_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample5_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample5_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample5_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample5_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample5_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample5_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample5_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample5_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample5_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample5_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample5_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample5_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample5_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample5_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample5_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample6########################################################################################
Tillage_6 <- c (rep ("nt",54), rep ("ct", 54))
cover_type_6 <- c(rep("fallow",18), rep("sorghum",18), rep("mix",18),
                  
                  rep("fallow",18), rep("sorghum",18), rep("mix",18))
Rep <- rep(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample6_trt <- data.frame (tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep )
sample6_trt$Rep <- as.factor(sample6_trt$Rep)


sample6 <- matrix(ncol = 10000, nrow = 108)
sample6_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample6[1:6, i] = sample (nt_f_br_1$bgluc, 6, replace = T)
  sample6[7:12, i] = sample (nt_f_br_2$bgluc, 6, replace = T)
  sample6[13:18, i] = sample (nt_f_br_3$bgluc, 6, replace = T)
  sample6[19:24, i] = sample (nt_s_br_1$bgluc, 6, replace = T)
  sample6[25:30, i] = sample (nt_s_br_2$bgluc, 6, replace = T)
  sample6[31:36, i] = sample (nt_s_br_3$bgluc, 6, replace = T)
  sample6[37:42, i] = sample (nt_m_br_1$bgluc, 6, replace = T)
  sample6[43:48, i] = sample (nt_m_br_2$bgluc, 6, replace = T)
  sample6[49:54, i] = sample (nt_m_br_3$bgluc, 6, replace = T)
  
  sample6[55:60, i] = sample (ct_f_br_1$bgluc, 6, replace = T)
  sample6[61:66, i] = sample (ct_f_br_2$bgluc, 6, replace = T)
  sample6[67:72, i] = sample (ct_f_br_3$bgluc, 6, replace = T)
  sample6[73:78, i] = sample (ct_s_br_1$bgluc, 6, replace = T)
  sample6[79:84, i] = sample (ct_s_br_2$bgluc, 6, replace = T)
  sample6[85:90, i] = sample (ct_s_br_3$bgluc, 6, replace = T)
  sample6[91:96, i] = sample (ct_m_br_1$bgluc, 6, replace = T)
  sample6[97:102, i] = sample (ct_m_br_2$bgluc, 6, replace = T)
  sample6[103:108, i] = sample (ct_m_br_3$bgluc, 6, replace = T)
  
  sample_6 <- data.frame (bgluc = sample6[,i], tillage = Tillage_6, cover_type = cover_type_6, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_6,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample6_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample6_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample6_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample6_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample6_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample6_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample6_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample6_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample6_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample6_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample6_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample6_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample6_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample6_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample6_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample6_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample6_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample6_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample6_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample6_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample6_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample7########################################################################################
Tillage_7 <- c (rep ("nt",63), rep ("ct", 63))
cover_type_7 <- c(rep("fallow",21), rep("sorghum",21), rep("mix",21),
                  
                  rep("fallow",21), rep("sorghum",21), rep("mix",21))
Rep <- rep(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample7_trt <- data.frame (tillage = Tillage_7, cover_type = cover_type_7, Rep = Rep )
sample7_trt$Rep <- as.factor(sample7_trt$Rep)


sample7 <- matrix(ncol = 10000, nrow = 126)
sample7_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample7[1:7, i] = sample (nt_f_br_1$bgluc, 7, replace = T)
  sample7[8:14, i] = sample (nt_f_br_2$bgluc, 7, replace = T)
  sample7[15:21, i] = sample (nt_f_br_3$bgluc, 7, replace = T)
  sample7[22:28, i] = sample (nt_s_br_1$bgluc, 7, replace = T)
  sample7[29:35, i] = sample (nt_s_br_2$bgluc, 7, replace = T)
  sample7[36:42, i] = sample (nt_s_br_3$bgluc, 7, replace = T)
  sample7[43:49, i] = sample (nt_m_br_1$bgluc, 7, replace = T)
  sample7[50:56, i] = sample (nt_m_br_2$bgluc, 7, replace = T)
  sample7[57:63, i] = sample (nt_m_br_3$bgluc, 7, replace = T)
  
  sample7[64:70, i] = sample (ct_f_br_1$bgluc, 7, replace = T)
  sample7[71:77, i] = sample (ct_f_br_2$bgluc, 7, replace = T)
  sample7[78:84, i] = sample (ct_f_br_3$bgluc, 7, replace = T)
  sample7[85:91, i] = sample (ct_s_br_1$bgluc, 7, replace = T)
  sample7[92:98, i] = sample (ct_s_br_2$bgluc, 7, replace = T)
  sample7[99:105, i] = sample (ct_s_br_3$bgluc, 7, replace = T)
  sample7[106:112, i] = sample (ct_m_br_1$bgluc, 7, replace = T)
  sample7[113:119, i] = sample (ct_m_br_2$bgluc, 7, replace = T)
  sample7[120:126, i] = sample (ct_m_br_3$bgluc, 7, replace = T)
  
  sample_7 <- data.frame (bgluc = sample7[,i], tillage = Tillage_7, cover_type = cover_type_7, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_7,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample7_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample7_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample7_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample7_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample7_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample7_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample7_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample7_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample7_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample7_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample7_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample7_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample7_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample7_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample7_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample7_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample7_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample7_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample7_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample7_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample7_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample8########################################################################################
Tillage_8 <- c (rep ("nt",72), rep ("ct", 72))
cover_type_8 <- c(rep("fallow",24), rep("sorghum",24), rep("mix",24),
                  
                  rep("fallow",24), rep("sorghum",24), rep("mix",24))
Rep <- rep(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample8_trt <- data.frame (tillage = Tillage_8, cover_type = cover_type_8, Rep = Rep )
sample8_trt$Rep <- as.factor(sample8_trt$Rep)


sample8 <- matrix(ncol = 10000, nrow = 144)
sample8_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample8[1:8, i] = sample (nt_f_br_1$bgluc, 8, replace = T)
  sample8[9:16, i] = sample (nt_f_br_2$bgluc, 8, replace = T)
  sample8[17:24, i] = sample (nt_f_br_3$bgluc, 8, replace = T)
  sample8[25:32, i] = sample (nt_s_br_1$bgluc, 8, replace = T)
  sample8[33:40, i] = sample (nt_s_br_2$bgluc, 8, replace = T)
  sample8[41:48, i] = sample (nt_s_br_3$bgluc, 8, replace = T)
  sample8[49:56, i] = sample (nt_m_br_1$bgluc, 8, replace = T)
  sample8[57:64, i] = sample (nt_m_br_2$bgluc, 8, replace = T)
  sample8[65:72, i] = sample (nt_m_br_3$bgluc, 8, replace = T)
  
  sample8[73:80, i] = sample (ct_f_br_1$bgluc, 8, replace = T)
  sample8[81:88, i] = sample (ct_f_br_2$bgluc, 8, replace = T)
  sample8[89:96, i] = sample (ct_f_br_3$bgluc, 8, replace = T)
  sample8[97:104, i] = sample (ct_s_br_1$bgluc, 8, replace = T)
  sample8[105:112, i] = sample (ct_s_br_2$bgluc, 8, replace = T)
  sample8[113:120, i] = sample (ct_s_br_3$bgluc, 8, replace = T)
  sample8[121:128, i] = sample (ct_m_br_1$bgluc, 8, replace = T)
  sample8[129:136, i] = sample (ct_m_br_2$bgluc, 8, replace = T)
  sample8[137:144, i] = sample (ct_m_br_3$bgluc, 8, replace = T)
  
  sample_8 <- data.frame (bgluc = sample8[,i], tillage = Tillage_8, cover_type = cover_type_8, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_8,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample8_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample8_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample8_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample8_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample8_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample8_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample8_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample8_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample8_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample8_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample8_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample8_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample8_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample8_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample8_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample8_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample8_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample8_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample8_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample8_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample8_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

#sample9########################################################################################
Tillage_9 <- c (rep ("nt",81), rep ("ct", 81))
cover_type_9 <- c(rep("fallow",27), rep("sorghum",27), rep("mix",27),
                  
                  rep("fallow",27), rep("sorghum",27), rep("mix",27))
Rep <- rep(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3), 6)
Rep <- as.factor(Rep)

sample9_trt <- data.frame (tillage = Tillage_9, cover_type = cover_type_9, Rep = Rep )
sample9_trt$Rep <- as.factor(sample9_trt$Rep)


sample9 <- matrix(ncol = 10000, nrow = 162)
sample9_br_pvalue <- matrix(ncol = 10000, nrow = 21)

for(i in 1:10000) {
  
  
  sample9[1:9, i] = sample (nt_f_br_1$bgluc, 9, replace = T)
  sample9[10:18, i] = sample (nt_f_br_2$bgluc, 9, replace = T)
  sample9[19:27, i] = sample (nt_f_br_3$bgluc, 9, replace = T)
  sample9[28:36, i] = sample (nt_s_br_1$bgluc, 9, replace = T)
  sample9[37:45, i] = sample (nt_s_br_2$bgluc, 9, replace = T)
  sample9[46:54, i] = sample (nt_s_br_3$bgluc, 9, replace = T)
  sample9[55:63, i] = sample (nt_m_br_1$bgluc, 9, replace = T)
  sample9[64:72, i] = sample (nt_m_br_2$bgluc, 9, replace = T)
  sample9[73:81, i] = sample (nt_m_br_3$bgluc, 9, replace = T)
  
  sample9[82:90, i] = sample (ct_f_br_1$bgluc, 9, replace = T)
  sample9[91:99, i] = sample (ct_f_br_2$bgluc, 9, replace = T)
  sample9[100:108, i] = sample (ct_f_br_3$bgluc, 9, replace = T)
  sample9[109:117, i] = sample (ct_s_br_1$bgluc, 9, replace = T)
  sample9[118:126, i] = sample (ct_s_br_2$bgluc, 9, replace = T)
  sample9[127:135, i] = sample (ct_s_br_3$bgluc, 9, replace = T)
  sample9[136:144, i] = sample (ct_m_br_1$bgluc, 9, replace = T)
  sample9[145:153, i] = sample (ct_m_br_2$bgluc, 9, replace = T)
  sample9[154:162, i] = sample (ct_m_br_3$bgluc, 9, replace = T)
  
  sample_9 <- data.frame (bgluc = sample9[,i], tillage = Tillage_9, cover_type = cover_type_9, Rep = Rep)
  
  fit <- lmer(bgluc ~ tillage * cover_type + (1|Rep:tillage), data = sample_9,
              control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
              #method = "REML", control = lmeControl(opt = "optim", method = "BFGS")
  )
  
  fit_all <- anova(fit)
  
  
  suppressMessages (posthoc_cover <- emmeans(fit, list(pairwise ~ cover_type), adjust = "tukey"))
  posthoc_cover <- posthoc_cover$`pairwise differences of cover_type`
  posthoc_cover <- as.data.frame(posthoc_cover)
  
  suppressMessages (posthoc_interraction <- emmeans(fit, list(pairwise ~ tillage * cover_type), adjust = "tukey"))
  posthoc_interraction <- posthoc_interraction$`pairwise differences of tillage, cover_type`
  posthoc_interraction <- as.data.frame(posthoc_interraction)
  
  
  sample9_br_pvalue [1,i] <-fit_all$`Pr(>F)`[1]
  sample9_br_pvalue [6,i] <- fit_all$`Pr(>F)`[3]
  sample9_br_pvalue [2,i] <- fit_all$`Pr(>F)`[2]
  sample9_br_pvalue [3,i] <- posthoc_cover$p.value[1]#check
  sample9_br_pvalue [4,i] <-  posthoc_cover$p.value[2]
  sample9_br_pvalue [5,i] <-  posthoc_cover$p.value[3]
  
  sample9_br_pvalue [7, i] <- posthoc_interraction$p.value[1]
  sample9_br_pvalue [8, i] <- posthoc_interraction$p.value[2]
  sample9_br_pvalue [9, i] <- posthoc_interraction$p.value[3]
  sample9_br_pvalue [10, i] <- posthoc_interraction$p.value[4]
  sample9_br_pvalue [11, i] <- posthoc_interraction$p.value[5]
  sample9_br_pvalue [12, i] <- posthoc_interraction$p.value[6]
  sample9_br_pvalue [13, i] <- posthoc_interraction$p.value[7]
  sample9_br_pvalue [14, i] <- posthoc_interraction$p.value[8]
  sample9_br_pvalue [15, i] <- posthoc_interraction$p.value[9]
  sample9_br_pvalue [16, i] <- posthoc_interraction$p.value[10]
  sample9_br_pvalue [17, i] <- posthoc_interraction$p.value[11]
  sample9_br_pvalue [18, i] <- posthoc_interraction$p.value[12]
  sample9_br_pvalue [19, i] <- posthoc_interraction$p.value[13]
  sample9_br_pvalue [20, i] <- posthoc_interraction$p.value[14]
  sample9_br_pvalue [21, i] <- posthoc_interraction$p.value[15]
}

options(warn = defaultW)
###############################################################################
#compiling the data and grabglucing
#ir
ir_1_tillage <- sample1_pvalue[1,]
ir_1_interraction <- sample1_pvalue[6,]
ir_1_cover_fallow_covercrop <- sample1_pvalue[3,]
ir_1_cover_sorghum_covercrop <- sample1_pvalue[5,]
ir_1_cover_fallow_sorghum <- sample1_pvalue[4,]
ir_1_cover <- sample1_pvalue[2,]
ir_1_ctf_ntf <- sample1_pvalue [7,]
ir_1_ctf_ctm <- sample1_pvalue [8,]
ir_1_ctf_ntm <- sample1_pvalue [8,]
ir_1_ctf_cts <- sample1_pvalue [10,]
ir_1_ctf_nts <- sample1_pvalue [11,]
ir_1_ntf_ctm <- sample1_pvalue [12,]
ir_1_ntf_ntm <- sample1_pvalue [13,]
ir_1_ntf_cts <- sample1_pvalue [14,]
ir_1_ntf_nts <- sample1_pvalue [15,]
ir_1_ctm_ntm <- sample1_pvalue [16,]
ir_1_ctm_cts <- sample1_pvalue [17,]
ir_1_ctm_nts <- sample1_pvalue [18,]
ir_1_ntm_cts <- sample1_pvalue [19,]
ir_1_ntm_nts <- sample1_pvalue [20,]
ir_1_cts_nts <- sample1_pvalue [21,]


ir_2_tillage <- sample2_pvalue[1,]
ir_2_interraction <- sample2_pvalue[6,]
ir_2_cover_fallow_covercrop <- sample2_pvalue[3,]
ir_2_cover_sorghum_covercrop <- sample2_pvalue[5,]
ir_2_cover_fallow_sorghum <- sample2_pvalue[4,]
ir_2_cover <- sample2_pvalue[2,]
ir_2_ctf_ntf <- sample2_pvalue [7,]
ir_2_ctf_ctm <- sample2_pvalue [8,]
ir_2_ctf_ntm <- sample2_pvalue [8,]
ir_2_ctf_cts <- sample2_pvalue [10,]
ir_2_ctf_nts <- sample2_pvalue [11,]
ir_2_ntf_ctm <- sample2_pvalue [12,]
ir_2_ntf_ntm <- sample2_pvalue [13,]
ir_2_ntf_cts <- sample2_pvalue [14,]
ir_2_ntf_nts <- sample2_pvalue [15,]
ir_2_ctm_ntm <- sample2_pvalue [16,]
ir_2_ctm_cts <- sample2_pvalue [17,]
ir_2_ctm_nts <- sample2_pvalue [18,]
ir_2_ntm_cts <- sample2_pvalue [19,]
ir_2_ntm_nts <- sample2_pvalue [20,]
ir_2_cts_nts <- sample2_pvalue [21,]

ir_3_tillage <- sample3_pvalue[1,]
ir_3_interraction <- sample3_pvalue[6,]
ir_3_cover_fallow_covercrop <- sample3_pvalue[3,]
ir_3_cover_sorghum_covercrop <- sample3_pvalue[5,]
ir_3_cover_fallow_sorghum <- sample3_pvalue[4,]
ir_3_cover <- sample3_pvalue[2,]
ir_3_ctf_ntf <- sample3_pvalue [7,]
ir_3_ctf_ctm <- sample3_pvalue [8,]
ir_3_ctf_ntm <- sample3_pvalue [8,]
ir_3_ctf_cts <- sample3_pvalue [10,]
ir_3_ctf_nts <- sample3_pvalue [11,]
ir_3_ntf_ctm <- sample3_pvalue [12,]
ir_3_ntf_ntm <- sample3_pvalue [13,]
ir_3_ntf_cts <- sample3_pvalue [14,]
ir_3_ntf_nts <- sample3_pvalue [15,]
ir_3_ctm_ntm <- sample3_pvalue [16,]
ir_3_ctm_cts <- sample3_pvalue [17,]
ir_3_ctm_nts <- sample3_pvalue [18,]
ir_3_ntm_cts <- sample3_pvalue [19,]
ir_3_ntm_nts <- sample3_pvalue [20,]
ir_3_cts_nts <- sample3_pvalue [21,]

ir_4_tillage <- sample4_pvalue[1,]
ir_4_interraction <- sample4_pvalue[6,]
ir_4_cover_fallow_covercrop <- sample4_pvalue[3,]
ir_4_cover_sorghum_covercrop <- sample4_pvalue[5,]
ir_4_cover_fallow_sorghum <- sample4_pvalue[4,]
ir_4_cover <- sample4_pvalue[2,]
ir_4_ctf_ntf <- sample4_pvalue [7,]
ir_4_ctf_ctm <- sample4_pvalue [8,]
ir_4_ctf_ntm <- sample4_pvalue [8,]
ir_4_ctf_cts <- sample4_pvalue [10,]
ir_4_ctf_nts <- sample4_pvalue [11,]
ir_4_ntf_ctm <- sample4_pvalue [12,]
ir_4_ntf_ntm <- sample4_pvalue [13,]
ir_4_ntf_cts <- sample4_pvalue [14,]
ir_4_ntf_nts <- sample4_pvalue [15,]
ir_4_ctm_ntm <- sample4_pvalue [16,]
ir_4_ctm_cts <- sample4_pvalue [17,]
ir_4_ctm_nts <- sample4_pvalue [18,]
ir_4_ntm_cts <- sample4_pvalue [19,]
ir_4_ntm_nts <- sample4_pvalue [20,]
ir_4_cts_nts <- sample4_pvalue [21,]

ir_5_tillage <- sample5_pvalue[1,]
ir_5_interraction <- sample5_pvalue[6,]
ir_5_cover_fallow_covercrop <- sample5_pvalue[3,]
ir_5_cover_sorghum_covercrop <- sample5_pvalue[5,]
ir_5_cover_fallow_sorghum <- sample5_pvalue[4,]
ir_5_cover <- sample5_pvalue[2,]
ir_5_ctf_ntf <- sample5_pvalue [7,]
ir_5_ctf_ctm <- sample5_pvalue [8,]
ir_5_ctf_ntm <- sample5_pvalue [8,]
ir_5_ctf_cts <- sample5_pvalue [10,]
ir_5_ctf_nts <- sample5_pvalue [11,]
ir_5_ntf_ctm <- sample5_pvalue [12,]
ir_5_ntf_ntm <- sample5_pvalue [13,]
ir_5_ntf_cts <- sample5_pvalue [14,]
ir_5_ntf_nts <- sample5_pvalue [15,]
ir_5_ctm_ntm <- sample5_pvalue [16,]
ir_5_ctm_cts <- sample5_pvalue [17,]
ir_5_ctm_nts <- sample5_pvalue [18,]
ir_5_ntm_cts <- sample5_pvalue [19,]
ir_5_ntm_nts <- sample5_pvalue [20,]
ir_5_cts_nts <- sample5_pvalue [21,]

ir_6_tillage <- sample6_pvalue[1,]
ir_6_interraction <- sample6_pvalue[6,]
ir_6_cover_fallow_covercrop <- sample6_pvalue[3,]
ir_6_cover_sorghum_covercrop <- sample6_pvalue[5,]
ir_6_cover_fallow_sorghum <- sample6_pvalue[4,]
ir_6_cover <- sample6_pvalue[2,]
ir_6_ctf_ntf <- sample6_pvalue [7,]
ir_6_ctf_ctm <- sample6_pvalue [8,]
ir_6_ctf_ntm <- sample6_pvalue [8,]
ir_6_ctf_cts <- sample6_pvalue [10,]
ir_6_ctf_nts <- sample6_pvalue [11,]
ir_6_ntf_ctm <- sample6_pvalue [12,]
ir_6_ntf_ntm <- sample6_pvalue [13,]
ir_6_ntf_cts <- sample6_pvalue [14,]
ir_6_ntf_nts <- sample6_pvalue [15,]
ir_6_ctm_ntm <- sample6_pvalue [16,]
ir_6_ctm_cts <- sample6_pvalue [17,]
ir_6_ctm_nts <- sample6_pvalue [18,]
ir_6_ntm_cts <- sample6_pvalue [19,]
ir_6_ntm_nts <- sample6_pvalue [20,]
ir_6_cts_nts <- sample6_pvalue [21,]

ir_7_tillage <- sample7_pvalue[1,]
ir_7_interraction <- sample7_pvalue[6,]
ir_7_cover_fallow_covercrop <- sample7_pvalue[3,]
ir_7_cover_sorghum_covercrop <- sample7_pvalue[5,]
ir_7_cover_fallow_sorghum <- sample7_pvalue[4,]
ir_7_cover <- sample7_pvalue[2,]
ir_7_ctf_ntf <- sample7_pvalue [7,]
ir_7_ctf_ctm <- sample7_pvalue [8,]
ir_7_ctf_ntm <- sample7_pvalue [8,]
ir_7_ctf_cts <- sample7_pvalue [10,]
ir_7_ctf_nts <- sample7_pvalue [11,]
ir_7_ntf_ctm <- sample7_pvalue [12,]
ir_7_ntf_ntm <- sample7_pvalue [13,]
ir_7_ntf_cts <- sample7_pvalue [14,]
ir_7_ntf_nts <- sample7_pvalue [15,]
ir_7_ctm_ntm <- sample7_pvalue [16,]
ir_7_ctm_cts <- sample7_pvalue [17,]
ir_7_ctm_nts <- sample7_pvalue [18,]
ir_7_ntm_cts <- sample7_pvalue [19,]
ir_7_ntm_nts <- sample7_pvalue [20,]
ir_7_cts_nts <- sample7_pvalue [21,]

ir_8_tillage <- sample8_pvalue[1,]
ir_8_interraction <- sample8_pvalue[6,]
ir_8_cover_fallow_covercrop <- sample8_pvalue[3,]
ir_8_cover_sorghum_covercrop <- sample8_pvalue[5,]
ir_8_cover_fallow_sorghum <- sample8_pvalue[4,]
ir_8_cover <- sample8_pvalue[2,]
ir_8_ctf_ntf <- sample8_pvalue [7,]
ir_8_ctf_ctm <- sample8_pvalue [8,]
ir_8_ctf_ntm <- sample8_pvalue [8,]
ir_8_ctf_cts <- sample8_pvalue [10,]
ir_8_ctf_nts <- sample8_pvalue [11,]
ir_8_ntf_ctm <- sample8_pvalue [12,]
ir_8_ntf_ntm <- sample8_pvalue [13,]
ir_8_ntf_cts <- sample8_pvalue [14,]
ir_8_ntf_nts <- sample8_pvalue [15,]
ir_8_ctm_ntm <- sample8_pvalue [16,]
ir_8_ctm_cts <- sample8_pvalue [17,]
ir_8_ctm_nts <- sample8_pvalue [18,]
ir_8_ntm_cts <- sample8_pvalue [19,]
ir_8_ntm_nts <- sample8_pvalue [20,]
ir_8_cts_nts <- sample8_pvalue [21,]

ir_9_tillage <- sample9_pvalue[1,]
ir_9_interraction <- sample9_pvalue[6,]
ir_9_cover_fallow_covercrop <- sample9_pvalue[3,]
ir_9_cover_sorghum_covercrop <- sample9_pvalue[5,]
ir_9_cover_fallow_sorghum <- sample9_pvalue[4,]
ir_9_cover <- sample9_pvalue[2,]
ir_9_ctf_ntf <- sample9_pvalue [7,]
ir_9_ctf_ctm <- sample9_pvalue [8,]
ir_9_ctf_ntm <- sample9_pvalue [8,]
ir_9_ctf_cts <- sample9_pvalue [10,]
ir_9_ctf_nts <- sample9_pvalue [11,]
ir_9_ntf_ctm <- sample9_pvalue [12,]
ir_9_ntf_ntm <- sample9_pvalue [13,]
ir_9_ntf_cts <- sample9_pvalue [14,]
ir_9_ntf_nts <- sample9_pvalue [15,]
ir_9_ctm_ntm <- sample9_pvalue [16,]
ir_9_ctm_cts <- sample9_pvalue [17,]
ir_9_ctm_nts <- sample9_pvalue [18,]
ir_9_ntm_cts <- sample9_pvalue [19,]
ir_9_ntm_nts <- sample9_pvalue [20,]
ir_9_cts_nts <- sample9_pvalue [21,]

#br###########################################

br_1_tillage <- sample1_br_pvalue[1,]
br_1_interraction <- sample1_br_pvalue[6,]
br_1_cover_fallow_covercrop <- sample1_br_pvalue[3,]
br_1_cover_sorghum_covercrop <- sample1_br_pvalue[5,]
br_1_cover_fallow_sorghum <- sample1_br_pvalue[4,]
br_1_cover <- sample1_br_pvalue[2,]
br_1_ctf_ntf <- sample1_pvalue [7,]
br_1_ctf_ctm <- sample1_pvalue [8,]
br_1_ctf_ntm <- sample1_pvalue [8,]
br_1_ctf_cts <- sample1_pvalue [10,]
br_1_ctf_nts <- sample1_pvalue [11,]
br_1_ntf_ctm <- sample1_pvalue [12,]
br_1_ntf_ntm <- sample1_pvalue [13,]
br_1_ntf_cts <- sample1_pvalue [14,]
br_1_ntf_nts <- sample1_pvalue [15,]
br_1_ctm_ntm <- sample1_pvalue [16,]
br_1_ctm_cts <- sample1_pvalue [17,]
br_1_ctm_nts <- sample1_pvalue [18,]
br_1_ntm_cts <- sample1_pvalue [19,]
br_1_ntm_nts <- sample1_pvalue [20,]
br_1_cts_nts <- sample1_pvalue [21,]

br_2_tillage <- sample2_br_pvalue[1,]
br_2_interraction <- sample2_br_pvalue[6,]
br_2_cover_fallow_covercrop <- sample2_br_pvalue[3,]
br_2_cover_sorghum_covercrop <- sample2_br_pvalue[5,]
br_2_cover_fallow_sorghum <- sample2_br_pvalue[4,]
br_2_cover <- sample2_br_pvalue[2,]
br_2_ctf_ntf <- sample2_pvalue [7,]
br_2_ctf_ctm <- sample2_pvalue [8,]
br_2_ctf_ntm <- sample2_pvalue [8,]
br_2_ctf_cts <- sample2_pvalue [10,]
br_2_ctf_nts <- sample2_pvalue [11,]
br_2_ntf_ctm <- sample2_pvalue [12,]
br_2_ntf_ntm <- sample2_pvalue [13,]
br_2_ntf_cts <- sample2_pvalue [14,]
br_2_ntf_nts <- sample2_pvalue [15,]
br_2_ctm_ntm <- sample2_pvalue [16,]
br_2_ctm_cts <- sample2_pvalue [17,]
br_2_ctm_nts <- sample2_pvalue [18,]
br_2_ntm_cts <- sample2_pvalue [19,]
br_2_ntm_nts <- sample2_pvalue [20,]
br_2_cts_nts <- sample2_pvalue [21,]

br_3_tillage <- sample3_br_pvalue[1,]
br_3_interraction <- sample3_br_pvalue[6,]
br_3_cover_fallow_covercrop <- sample3_br_pvalue[3,]
br_3_cover_sorghum_covercrop <- sample3_br_pvalue[5,]
br_3_cover_fallow_sorghum <- sample3_br_pvalue[4,]
br_3_cover <- sample3_br_pvalue[2,]
br_3_ctf_ntf <- sample3_pvalue [7,]
br_3_ctf_ctm <- sample3_pvalue [8,]
br_3_ctf_ntm <- sample3_pvalue [8,]
br_3_ctf_cts <- sample3_pvalue [10,]
br_3_ctf_nts <- sample3_pvalue [11,]
br_3_ntf_ctm <- sample3_pvalue [12,]
br_3_ntf_ntm <- sample3_pvalue [13,]
br_3_ntf_cts <- sample3_pvalue [14,]
br_3_ntf_nts <- sample3_pvalue [15,]
br_3_ctm_ntm <- sample3_pvalue [16,]
br_3_ctm_cts <- sample3_pvalue [17,]
br_3_ctm_nts <- sample3_pvalue [18,]
br_3_ntm_cts <- sample3_pvalue [19,]
br_3_ntm_nts <- sample3_pvalue [20,]
br_3_cts_nts <- sample3_pvalue [21,]

br_4_tillage <- sample4_br_pvalue[1,]
br_4_interraction <- sample4_br_pvalue[6,]
br_4_cover_fallow_covercrop <- sample4_br_pvalue[3,]
br_4_cover_sorghum_covercrop <- sample4_br_pvalue[5,]
br_4_cover_fallow_sorghum <- sample4_br_pvalue[4,]
br_4_cover <- sample4_br_pvalue[2,]
br_4_ctf_ntf <- sample4_pvalue [7,]
br_4_ctf_ctm <- sample4_pvalue [8,]
br_4_ctf_ntm <- sample4_pvalue [8,]
br_4_ctf_cts <- sample4_pvalue [10,]
br_4_ctf_nts <- sample4_pvalue [11,]
br_4_ntf_ctm <- sample4_pvalue [12,]
br_4_ntf_ntm <- sample4_pvalue [13,]
br_4_ntf_cts <- sample4_pvalue [14,]
br_4_ntf_nts <- sample4_pvalue [15,]
br_4_ctm_ntm <- sample4_pvalue [16,]
br_4_ctm_cts <- sample4_pvalue [17,]
br_4_ctm_nts <- sample4_pvalue [18,]
br_4_ntm_cts <- sample4_pvalue [19,]
br_4_ntm_nts <- sample4_pvalue [20,]
br_4_cts_nts <- sample4_pvalue [21,]

br_5_tillage <- sample5_br_pvalue[1,]
br_5_interraction <- sample5_br_pvalue[6,]
br_5_cover_fallow_covercrop <- sample5_br_pvalue[3,]
br_5_cover_sorghum_covercrop <- sample5_br_pvalue[5,]
br_5_cover_fallow_sorghum <- sample5_br_pvalue[4,]
br_5_cover <- sample5_br_pvalue[2,]
br_5_ctf_ntf <- sample5_pvalue [7,]
br_5_ctf_ctm <- sample5_pvalue [8,]
br_5_ctf_ntm <- sample5_pvalue [8,]
br_5_ctf_cts <- sample5_pvalue [10,]
br_5_ctf_nts <- sample5_pvalue [11,]
br_5_ntf_ctm <- sample5_pvalue [12,]
br_5_ntf_ntm <- sample5_pvalue [13,]
br_5_ntf_cts <- sample5_pvalue [14,]
br_5_ntf_nts <- sample5_pvalue [15,]
br_5_ctm_ntm <- sample5_pvalue [16,]
br_5_ctm_cts <- sample5_pvalue [17,]
br_5_ctm_nts <- sample5_pvalue [18,]
br_5_ntm_cts <- sample5_pvalue [19,]
br_5_ntm_nts <- sample5_pvalue [20,]
br_5_cts_nts <- sample5_pvalue [21,]

br_6_tillage <- sample6_br_pvalue[1,]
br_6_interraction <- sample6_br_pvalue[6,]
br_6_cover_fallow_covercrop <- sample6_br_pvalue[3,]
br_6_cover_sorghum_covercrop <- sample6_br_pvalue[5,]
br_6_cover_fallow_sorghum <- sample6_br_pvalue[4,]
br_6_cover <- sample6_br_pvalue[2,]
br_6_ctf_ntf <- sample6_pvalue [7,]
br_6_ctf_ctm <- sample6_pvalue [8,]
br_6_ctf_ntm <- sample6_pvalue [8,]
br_6_ctf_cts <- sample6_pvalue [10,]
br_6_ctf_nts <- sample6_pvalue [11,]
br_6_ntf_ctm <- sample6_pvalue [12,]
br_6_ntf_ntm <- sample6_pvalue [13,]
br_6_ntf_cts <- sample6_pvalue [14,]
br_6_ntf_nts <- sample6_pvalue [15,]
br_6_ctm_ntm <- sample6_pvalue [16,]
br_6_ctm_cts <- sample6_pvalue [17,]
br_6_ctm_nts <- sample6_pvalue [18,]
br_6_ntm_cts <- sample6_pvalue [19,]
br_6_ntm_nts <- sample6_pvalue [20,]
br_6_cts_nts <- sample6_pvalue [21,]

br_7_tillage <- sample7_br_pvalue[1,]
br_7_interraction <- sample7_br_pvalue[6,]
br_7_cover_fallow_covercrop <- sample7_br_pvalue[3,]
br_7_cover_sorghum_covercrop <- sample7_br_pvalue[5,]
br_7_cover_fallow_sorghum <- sample7_br_pvalue[4,]
br_7_cover <- sample7_br_pvalue[2,]
br_7_ctf_ntf <- sample7_pvalue [7,]
br_7_ctf_ctm <- sample7_pvalue [8,]
br_7_ctf_ntm <- sample7_pvalue [8,]
br_7_ctf_cts <- sample7_pvalue [10,]
br_7_ctf_nts <- sample7_pvalue [11,]
br_7_ntf_ctm <- sample7_pvalue [12,]
br_7_ntf_ntm <- sample7_pvalue [13,]
br_7_ntf_cts <- sample7_pvalue [14,]
br_7_ntf_nts <- sample7_pvalue [15,]
br_7_ctm_ntm <- sample7_pvalue [16,]
br_7_ctm_cts <- sample7_pvalue [17,]
br_7_ctm_nts <- sample7_pvalue [18,]
br_7_ntm_cts <- sample7_pvalue [19,]
br_7_ntm_nts <- sample7_pvalue [20,]
br_7_cts_nts <- sample7_pvalue [21,]

br_8_tillage <- sample8_br_pvalue[1,]
br_8_interraction <- sample8_br_pvalue[6,]
br_8_cover_fallow_covercrop <- sample8_br_pvalue[3,]
br_8_cover_sorghum_covercrop <- sample8_br_pvalue[5,]
br_8_cover_fallow_sorghum <- sample8_br_pvalue[4,]
br_8_cover <- sample8_br_pvalue[2,]
br_8_ctf_ntf <- sample8_pvalue [7,]
br_8_ctf_ctm <- sample8_pvalue [8,]
br_8_ctf_ntm <- sample8_pvalue [8,]
br_8_ctf_cts <- sample8_pvalue [10,]
br_8_ctf_nts <- sample8_pvalue [11,]
br_8_ntf_ctm <- sample8_pvalue [12,]
br_8_ntf_ntm <- sample8_pvalue [13,]
br_8_ntf_cts <- sample8_pvalue [14,]
br_8_ntf_nts <- sample8_pvalue [15,]
br_8_ctm_ntm <- sample8_pvalue [16,]
br_8_ctm_cts <- sample8_pvalue [17,]
br_8_ctm_nts <- sample8_pvalue [18,]
br_8_ntm_cts <- sample8_pvalue [19,]
br_8_ntm_nts <- sample8_pvalue [20,]
br_8_cts_nts <- sample8_pvalue [21,]

br_9_tillage <- sample9_br_pvalue[1,]
br_9_interraction <- sample9_br_pvalue[6,]
br_9_cover_fallow_covercrop <- sample9_br_pvalue[3,]
br_9_cover_sorghum_covercrop <- sample9_br_pvalue[5,]
br_9_cover_fallow_sorghum <- sample9_br_pvalue[4,]
br_9_cover <- sample9_br_pvalue[2,]
br_9_ctf_ntf <- sample9_pvalue [7,]
br_9_ctf_ctm <- sample9_pvalue [8,]
br_9_ctf_ntm <- sample9_pvalue [8,]
br_9_ctf_cts <- sample9_pvalue [10,]
br_9_ctf_nts <- sample9_pvalue [11,]
br_9_ntf_ctm <- sample9_pvalue [12,]
br_9_ntf_ntm <- sample9_pvalue [13,]
br_9_ntf_cts <- sample9_pvalue [14,]
br_9_ntf_nts <- sample9_pvalue [15,]
br_9_ctm_ntm <- sample9_pvalue [16,]
br_9_ctm_cts <- sample9_pvalue [17,]
br_9_ctm_nts <- sample9_pvalue [18,]
br_9_ntm_cts <- sample9_pvalue [19,]
br_9_ntm_nts <- sample9_pvalue [20,]
br_9_cts_nts <- sample9_pvalue [21,]

tillage_pvalue <- cbind(
  t(ir_1_tillage),
  t(ir_2_tillage),
  t(ir_3_tillage),
  t(ir_4_tillage),
  t(ir_5_tillage),
  t(ir_6_tillage),
  t(ir_7_tillage),
  t(ir_8_tillage),
  t(ir_9_tillage),
  t(br_1_tillage),
  t(br_2_tillage),
  t(br_3_tillage),
  t(br_4_tillage),
  t(br_5_tillage),
  t(br_6_tillage),
  t(br_7_tillage),
  t(br_8_tillage),
  t(br_9_tillage)
)

tillage_pvalue <- t(tillage_pvalue)

cover_pvalue <- cbind(
  t(ir_1_cover),
  t(ir_2_cover),
  t(ir_3_cover),
  t(ir_4_cover),
  t(ir_5_cover),
  t(ir_6_cover),
  t(ir_7_cover),
  t(ir_8_cover),
  t(ir_9_cover),
  t(br_1_cover),
  t(br_2_cover),
  t(br_3_cover),
  t(br_4_cover),
  t(br_5_cover),
  t(br_6_cover),
  t(br_7_cover),
  t(br_8_cover),
  t(br_9_cover)
)

cover_pvalue <- t(cover_pvalue)
#cover crop tukey
#cover_fallow_covercrop
cover_fallow_covercrop_pvalue <- cbind(
  t(ir_1_cover_fallow_covercrop),
  t(ir_2_cover_fallow_covercrop),
  t(ir_3_cover_fallow_covercrop),
  t(ir_4_cover_fallow_covercrop),
  t(ir_5_cover_fallow_covercrop),
  t(ir_6_cover_fallow_covercrop),
  t(ir_7_cover_fallow_covercrop),
  t(ir_8_cover_fallow_covercrop),
  t(ir_9_cover_fallow_covercrop),
  t(br_1_cover_fallow_covercrop),
  t(br_2_cover_fallow_covercrop),
  t(br_3_cover_fallow_covercrop),
  t(br_4_cover_fallow_covercrop),
  t(br_5_cover_fallow_covercrop),
  t(br_6_cover_fallow_covercrop),
  t(br_7_cover_fallow_covercrop),
  t(br_8_cover_fallow_covercrop),
  t(br_9_cover_fallow_covercrop)
)

cover_fallow_covercrop_pvalue <- t(cover_fallow_covercrop_pvalue)
#cover_fallow_sorghum

cover_fallow_sorghum_pvalue <- cbind(
  t(ir_1_cover_fallow_sorghum),
  t(ir_2_cover_fallow_sorghum),
  t(ir_3_cover_fallow_sorghum),
  t(ir_4_cover_fallow_sorghum),
  t(ir_5_cover_fallow_sorghum),
  t(ir_6_cover_fallow_sorghum),
  t(ir_7_cover_fallow_sorghum),
  t(ir_8_cover_fallow_sorghum),
  t(ir_9_cover_fallow_sorghum),
  t(br_1_cover_fallow_sorghum),
  t(br_2_cover_fallow_sorghum),
  t(br_3_cover_fallow_sorghum),
  t(br_4_cover_fallow_sorghum),
  t(br_5_cover_fallow_sorghum),
  t(br_6_cover_fallow_sorghum),
  t(br_7_cover_fallow_sorghum),
  t(br_8_cover_fallow_sorghum),
  t(br_9_cover_fallow_sorghum)
)

cover_fallow_sorghum_pvalue <- t(cover_fallow_sorghum_pvalue)

#cover_sorghum_covercrop
cover_sorghum_covercrop_pvalue <- cbind(
  t(ir_1_cover_sorghum_covercrop),
  t(ir_2_cover_sorghum_covercrop),
  t(ir_3_cover_sorghum_covercrop),
  t(ir_4_cover_sorghum_covercrop),
  t(ir_5_cover_sorghum_covercrop),
  t(ir_6_cover_sorghum_covercrop),
  t(ir_7_cover_sorghum_covercrop),
  t(ir_8_cover_sorghum_covercrop),
  t(ir_9_cover_sorghum_covercrop),
  t(br_1_cover_sorghum_covercrop),
  t(br_2_cover_sorghum_covercrop),
  t(br_3_cover_sorghum_covercrop),
  t(br_4_cover_sorghum_covercrop),
  t(br_5_cover_sorghum_covercrop),
  t(br_6_cover_sorghum_covercrop),
  t(br_7_cover_sorghum_covercrop),
  t(br_8_cover_sorghum_covercrop),
  t(br_9_cover_sorghum_covercrop)
)

cover_sorghum_covercrop_pvalue <- t(cover_sorghum_covercrop_pvalue)

interraction_pvalue <- cbind(
  t(ir_1_interraction),
  t(ir_2_interraction),
  t(ir_3_interraction),
  t(ir_4_interraction),
  t(ir_5_interraction),
  t(ir_6_interraction),
  t(ir_7_interraction),
  t(ir_8_interraction),
  t(ir_9_interraction),
  t(br_1_interraction),
  t(br_2_interraction),
  t(br_3_interraction),
  t(br_4_interraction),
  t(br_5_interraction),
  t(br_6_interraction),
  t(br_7_interraction),
  t(br_8_interraction),
  t(br_9_interraction)
)

interraction_pvalue <- t(interraction_pvalue)

ctf_ntf_pvalue <- cbind(
  t(ir_1_ctf_ntf),
  t(ir_2_ctf_ntf),
  t(ir_3_ctf_ntf),
  t(ir_4_ctf_ntf),
  t(ir_5_ctf_ntf),
  t(ir_6_ctf_ntf),
  t(ir_7_ctf_ntf),
  t(ir_8_ctf_ntf),
  t(ir_9_ctf_ntf),
  t(br_1_ctf_ntf),
  t(br_2_ctf_ntf),
  t(br_3_ctf_ntf),
  t(br_4_ctf_ntf),
  t(br_5_ctf_ntf),
  t(br_6_ctf_ntf),
  t(br_7_ctf_ntf),
  t(br_8_ctf_ntf),
  t(br_9_ctf_ntf)
)

ctf_ntf_pvalue <- t(ctf_ntf_pvalue)

ctf_ctm_pvalue <- cbind(
  t(ir_1_ctf_ctm),
  t(ir_2_ctf_ctm),
  t(ir_3_ctf_ctm),
  t(ir_4_ctf_ctm),
  t(ir_5_ctf_ctm),
  t(ir_6_ctf_ctm),
  t(ir_7_ctf_ctm),
  t(ir_8_ctf_ctm),
  t(ir_9_ctf_ctm),
  t(br_1_ctf_ctm),
  t(br_2_ctf_ctm),
  t(br_3_ctf_ctm),
  t(br_4_ctf_ctm),
  t(br_5_ctf_ctm),
  t(br_6_ctf_ctm),
  t(br_7_ctf_ctm),
  t(br_8_ctf_ctm),
  t(br_9_ctf_ctm)
)

ctf_ctm_pvalue <- t(ctf_ctm_pvalue)

ctf_ntm_pvalue <- cbind(
  t(ir_1_ctf_ntm),
  t(ir_2_ctf_ntm),
  t(ir_3_ctf_ntm),
  t(ir_4_ctf_ntm),
  t(ir_5_ctf_ntm),
  t(ir_6_ctf_ntm),
  t(ir_7_ctf_ntm),
  t(ir_8_ctf_ntm),
  t(ir_9_ctf_ntm),
  t(br_1_ctf_ntm),
  t(br_2_ctf_ntm),
  t(br_3_ctf_ntm),
  t(br_4_ctf_ntm),
  t(br_5_ctf_ntm),
  t(br_6_ctf_ntm),
  t(br_7_ctf_ntm),
  t(br_8_ctf_ntm),
  t(br_9_ctf_ntm)
)

ctf_ntm_pvalue <- t(ctf_ntm_pvalue)

ctf_cts_pvalue <- cbind(
  t(ir_1_ctf_cts),
  t(ir_2_ctf_cts),
  t(ir_3_ctf_cts),
  t(ir_4_ctf_cts),
  t(ir_5_ctf_cts),
  t(ir_6_ctf_cts),
  t(ir_7_ctf_cts),
  t(ir_8_ctf_cts),
  t(ir_9_ctf_cts),
  t(br_1_ctf_cts),
  t(br_2_ctf_cts),
  t(br_3_ctf_cts),
  t(br_4_ctf_cts),
  t(br_5_ctf_cts),
  t(br_6_ctf_cts),
  t(br_7_ctf_cts),
  t(br_8_ctf_cts),
  t(br_9_ctf_cts)
)

ctf_cts_pvalue <- t(ctf_cts_pvalue)

ctf_nts_pvalue <- cbind(
  t(ir_1_ctf_nts),
  t(ir_2_ctf_nts),
  t(ir_3_ctf_nts),
  t(ir_4_ctf_nts),
  t(ir_5_ctf_nts),
  t(ir_6_ctf_nts),
  t(ir_7_ctf_nts),
  t(ir_8_ctf_nts),
  t(ir_9_ctf_nts),
  t(br_1_ctf_nts),
  t(br_2_ctf_nts),
  t(br_3_ctf_nts),
  t(br_4_ctf_nts),
  t(br_5_ctf_nts),
  t(br_6_ctf_nts),
  t(br_7_ctf_nts),
  t(br_8_ctf_nts),
  t(br_9_ctf_nts)
)

ctf_nts_pvalue <- t(ctf_nts_pvalue)

ntf_ctm_pvalue <- cbind(
  t(ir_1_ntf_ctm),
  t(ir_2_ntf_ctm),
  t(ir_3_ntf_ctm),
  t(ir_4_ntf_ctm),
  t(ir_5_ntf_ctm),
  t(ir_6_ntf_ctm),
  t(ir_7_ntf_ctm),
  t(ir_8_ntf_ctm),
  t(ir_9_ntf_ctm),
  t(br_1_ntf_ctm),
  t(br_2_ntf_ctm),
  t(br_3_ntf_ctm),
  t(br_4_ntf_ctm),
  t(br_5_ntf_ctm),
  t(br_6_ntf_ctm),
  t(br_7_ntf_ctm),
  t(br_8_ntf_ctm),
  t(br_9_ntf_ctm)
)

ntf_ctm_pvalue <- t(ntf_ctm_pvalue)

ntf_ntm_pvalue <- cbind(
  t(ir_1_ntf_ntm),
  t(ir_2_ntf_ntm),
  t(ir_3_ntf_ntm),
  t(ir_4_ntf_ntm),
  t(ir_5_ntf_ntm),
  t(ir_6_ntf_ntm),
  t(ir_7_ntf_ntm),
  t(ir_8_ntf_ntm),
  t(ir_9_ntf_ntm),
  t(br_1_ntf_ntm),
  t(br_2_ntf_ntm),
  t(br_3_ntf_ntm),
  t(br_4_ntf_ntm),
  t(br_5_ntf_ntm),
  t(br_6_ntf_ntm),
  t(br_7_ntf_ntm),
  t(br_8_ntf_ntm),
  t(br_9_ntf_ntm)
)

ntf_ntm_pvalue <- t(ntf_ntm_pvalue)

ntf_cts_pvalue <- cbind(
  t(ir_1_ntf_cts),
  t(ir_2_ntf_cts),
  t(ir_3_ntf_cts),
  t(ir_4_ntf_cts),
  t(ir_5_ntf_cts),
  t(ir_6_ntf_cts),
  t(ir_7_ntf_cts),
  t(ir_8_ntf_cts),
  t(ir_9_ntf_cts),
  t(br_1_ntf_cts),
  t(br_2_ntf_cts),
  t(br_3_ntf_cts),
  t(br_4_ntf_cts),
  t(br_5_ntf_cts),
  t(br_6_ntf_cts),
  t(br_7_ntf_cts),
  t(br_8_ntf_cts),
  t(br_9_ntf_cts)
)

ntf_cts_pvalue <- t(ntf_cts_pvalue)

ntf_nts_pvalue <- cbind(
  t(ir_1_ntf_nts),
  t(ir_2_ntf_nts),
  t(ir_3_ntf_nts),
  t(ir_4_ntf_nts),
  t(ir_5_ntf_nts),
  t(ir_6_ntf_nts),
  t(ir_7_ntf_nts),
  t(ir_8_ntf_nts),
  t(ir_9_ntf_nts),
  t(br_1_ntf_nts),
  t(br_2_ntf_nts),
  t(br_3_ntf_nts),
  t(br_4_ntf_nts),
  t(br_5_ntf_nts),
  t(br_6_ntf_nts),
  t(br_7_ntf_nts),
  t(br_8_ntf_nts),
  t(br_9_ntf_nts)
)

ntf_nts_pvalue <- t(ntf_nts_pvalue)

ctm_ntm_pvalue <- cbind(
  t(ir_1_ctm_ntm),
  t(ir_2_ctm_ntm),
  t(ir_3_ctm_ntm),
  t(ir_4_ctm_ntm),
  t(ir_5_ctm_ntm),
  t(ir_6_ctm_ntm),
  t(ir_7_ctm_ntm),
  t(ir_8_ctm_ntm),
  t(ir_9_ctm_ntm),
  t(br_1_ctm_ntm),
  t(br_2_ctm_ntm),
  t(br_3_ctm_ntm),
  t(br_4_ctm_ntm),
  t(br_5_ctm_ntm),
  t(br_6_ctm_ntm),
  t(br_7_ctm_ntm),
  t(br_8_ctm_ntm),
  t(br_9_ctm_ntm)
)

ctm_ntm_pvalue <- t(ctm_ntm_pvalue)

ctm_cts_pvalue <- cbind(
  t(ir_1_ctm_cts),
  t(ir_2_ctm_cts),
  t(ir_3_ctm_cts),
  t(ir_4_ctm_cts),
  t(ir_5_ctm_cts),
  t(ir_6_ctm_cts),
  t(ir_7_ctm_cts),
  t(ir_8_ctm_cts),
  t(ir_9_ctm_cts),
  t(br_1_ctm_cts),
  t(br_2_ctm_cts),
  t(br_3_ctm_cts),
  t(br_4_ctm_cts),
  t(br_5_ctm_cts),
  t(br_6_ctm_cts),
  t(br_7_ctm_cts),
  t(br_8_ctm_cts),
  t(br_9_ctm_cts)
)

ctm_cts_pvalue <- t(ctm_cts_pvalue)

ctm_nts_pvalue <- cbind(
  t(ir_1_ctm_nts),
  t(ir_2_ctm_nts),
  t(ir_3_ctm_nts),
  t(ir_4_ctm_nts),
  t(ir_5_ctm_nts),
  t(ir_6_ctm_nts),
  t(ir_7_ctm_nts),
  t(ir_8_ctm_nts),
  t(ir_9_ctm_nts),
  t(br_1_ctm_nts),
  t(br_2_ctm_nts),
  t(br_3_ctm_nts),
  t(br_4_ctm_nts),
  t(br_5_ctm_nts),
  t(br_6_ctm_nts),
  t(br_7_ctm_nts),
  t(br_8_ctm_nts),
  t(br_9_ctm_nts)
)

ctm_nts_pvalue <- t(ctm_nts_pvalue)

ntm_cts_pvalue <- cbind(
  t(ir_1_ntm_cts),
  t(ir_2_ntm_cts),
  t(ir_3_ntm_cts),
  t(ir_4_ntm_cts),
  t(ir_5_ntm_cts),
  t(ir_6_ntm_cts),
  t(ir_7_ntm_cts),
  t(ir_8_ntm_cts),
  t(ir_9_ntm_cts),
  t(br_1_ntm_cts),
  t(br_2_ntm_cts),
  t(br_3_ntm_cts),
  t(br_4_ntm_cts),
  t(br_5_ntm_cts),
  t(br_6_ntm_cts),
  t(br_7_ntm_cts),
  t(br_8_ntm_cts),
  t(br_9_ntm_cts)
)

ntm_cts_pvalue <- t(ntm_cts_pvalue)

ntm_nts_pvalue <- cbind(
  t(ir_1_ntm_nts),
  t(ir_2_ntm_nts),
  t(ir_3_ntm_nts),
  t(ir_4_ntm_nts),
  t(ir_5_ntm_nts),
  t(ir_6_ntm_nts),
  t(ir_7_ntm_nts),
  t(ir_8_ntm_nts),
  t(ir_9_ntm_nts),
  t(br_1_ntm_nts),
  t(br_2_ntm_nts),
  t(br_3_ntm_nts),
  t(br_4_ntm_nts),
  t(br_5_ntm_nts),
  t(br_6_ntm_nts),
  t(br_7_ntm_nts),
  t(br_8_ntm_nts),
  t(br_9_ntm_nts)
)

ntm_nts_pvalue <- t(ntm_nts_pvalue)

cts_nts_pvalue <- cbind(
  t(ir_1_cts_nts),
  t(ir_2_cts_nts),
  t(ir_3_cts_nts),
  t(ir_4_cts_nts),
  t(ir_5_cts_nts),
  t(ir_6_cts_nts),
  t(ir_7_cts_nts),
  t(ir_8_cts_nts),
  t(ir_9_cts_nts),
  t(br_1_cts_nts),
  t(br_2_cts_nts),
  t(br_3_cts_nts),
  t(br_4_cts_nts),
  t(br_5_cts_nts),
  t(br_6_cts_nts),
  t(br_7_cts_nts),
  t(br_8_cts_nts),
  t(br_9_cts_nts)
)

cts_nts_pvalue <- t(cts_nts_pvalue)

one <- c(rep(1,10000))
two <- c(rep(2,10000))
three <- c(rep(3,10000))
four <- c(rep(4,10000))
five <- c(rep(5,10000))
six <- c(rep(6,10000))
seven <- c(rep(7,10000))
eight <- c(rep(8,10000))
nine <- c(rep(9,10000))



sample_no <- cbind(t(one), t(two), t(three), t(four), t(five), t(six), t(seven), t(eight), t(nine),
                   t(one), t(two), t(three), t(four), t(five), t(six), t(seven), t(eight), t(nine))
sample_no <- t(sample_no)

row <- c (rep ("In Row",90000), rep ("Between Row", 90000))
row <- t(row)
row <- t(row)

compiled <- data.frame(
  sample_no,
  row,
  tillage_pvalue,
  cover_pvalue,
  interraction_pvalue,
  cover_fallow_covercrop_pvalue,
  cover_fallow_sorghum_pvalue,
  cover_sorghum_covercrop_pvalue,
  ctf_ntf_pvalue,
  ctf_ctm_pvalue,
  ctf_ntm_pvalue,
  ctf_cts_pvalue,
  ctf_nts_pvalue,
  ntf_ctm_pvalue,
  ntf_ntm_pvalue,
  ntf_cts_pvalue,
  ntf_nts_pvalue,
  ctm_ntm_pvalue,
  ctm_cts_pvalue,
  ctm_nts_pvalue,
  ntm_cts_pvalue,
  ntm_nts_pvalue,
  cts_nts_pvalue
  
)

compiled$sample_no <- as.factor(compiled$sample_no)