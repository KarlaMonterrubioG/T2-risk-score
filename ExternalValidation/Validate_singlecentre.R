#Author: Caelan Taggart and Karla Monterubio-Gómez
#Date 24/09/2021
#Validation for Swedish data 


library(gtsummary)
library(tidyverse)
library(readxl)
library(kableExtra)
library(rms)
library(Hmisc)
library(fastmap)
library(pROC)
library(ggplot2)
library(knitr)
library(qwraps2)
library(tidyr)
library(survival)
library(Rcpp)
library(survminer)

update.packages("fastmap")

# Load and merge 2 dfs , dates corrected on upload

karolinska_t2_cleaned1 = read_excel("~/PhD/Demand Score/karolinska t2 cleaned1.xlsx", 
                                    col_types = c("numeric", "numeric", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "numeric", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "date", "numeric", 
                                                  "numeric", "numeric", "date", "numeric", 
                                                  "numeric", "text", "numeric", "text", 
                                                  "date", "numeric", "numeric", "numeric"))
library(readxl)
GRACE_AR_220110 = read_excel("PhD/Demand Score/GRACE_AR_220110.xlsx", 
                              col_types = c("numeric", "date", "date", 
                                            "numeric", "date", "numeric", "date", 
                                            "numeric", "numeric", "numeric"))

fullswede = merge(GRACE_AR_220110, karolinska_t2_cleaned1,by="lpnr",all.y = TRUE)
View(fullswede)

#test<- anti_join(karolinska_t2_cleaned1, GRACE_data_AR_210924,by="lpnr")
View(test)


# Exclude the three st-deviation missing patients and one with no outcome variables

fullswede = fullswede[-c(which(fullswede$lpnr==24185),
                         which(fullswede$lpnr==29162),which(fullswede$lpnr==2716)),]

#First sort out factors

fullswede$PrevCD = as.factor(fullswede$PrevCD)
fullswede$`Prior MI` = as.factor(fullswede$`Prior MI`)
fullswede$PrevHFHospitalisation = as.factor(fullswede$PrevHFHospitalisation)
fullswede$`Prior CABG or PCI` = as.factor(fullswede$`Prior CABG or PCI`)
fullswede$PrevDiabetes = as.factor(fullswede$PrevDiabetes)
fullswede$`STT-deviation on ECG`= as.factor(fullswede$`STT-deviation on ECG`)
fullswede$dead = as.factor(fullswede$dead)
fullswede$CV_death = as.factor(fullswede$CV_death)
fullswede$AMI = as.factor(fullswede$AMI)
fullswede$Sex = as.factor(fullswede$Sex)
fullswede$`Systolic BP`= as.numeric(fullswede$`Systolic BP`)
fullswede$eGFR.computed = as.numeric(fullswede$eGFR.computed)
fullswede$`PeaktroponinT`= as.numeric(fullswede$`PeaktroponinT`)

#Make columbs to fit with Scottish data
#Which means make a PrevIHD column, anaemia and myocardial ischaemia and log troponin + egfr
fullswede = fullswede %>% 
  mutate(PrevIHD = case_when((`Prior CABG or PCI`== "Yes" | `Prior MI` == "Yes" )~ "Yes", TRUE ~ "No"))

fullswede = fullswede %>% 
  mutate(Anaemia = ifelse(fullswede$Sex=="Female", ifelse(fullswede$Heamoglobin <120 , "Yes" , "No"), 
                          ifelse(fullswede$Heamoglobin <130, "Yes", "No")))

fullswede$logtroponin = log(fullswede$`PeaktroponinT`) # Swedish actually used trop T despite the excel document saying I.

fullswede$logegfr = fullswede$`log efgr`

fullswede$log.topI = fullswede$logtroponin 
view(fullswede)

#Transform T data into I data from linear model for prediction 
fullswede$logtroponin =  -0.36759 + 1.35335*fullswede$log.topI

fullswede$PrevIHD = as.factor(fullswede$PrevIHD)
fullswede$Anaemia = as.factor(fullswede$Anaemia)

#Make outcome columbs and dates
#Make CV death or MI (secondary outcome) first as will needs censoring for Non-CV death. Done this by limiting the time to event variables

fullswede = fullswede %>% 
  mutate(secout = case_when((AMI == "1"  | CV_death == "1") ~ 1 , TRUE ~ 0))

#days to event for MI and censor to 365
fullswede$MI_days = as.numeric(round(difftime(fullswede$AMI_date,
                                              fullswede$indexdate, units = "days")))
fullswede$MI_days[is.na(fullswede$MI_days)==TRUE] = 365

#days to event for CV death and censor to 365 
fullswede$CV_death_days = as.numeric(round(difftime(fullswede$CV_death_date,
                                                    fullswede$indexdate, units = "days")))
fullswede$CV_death_days[is.na(fullswede$CV_death_days) == TRUE] = 365

#Days till secondary outcome
fullswede$secout_days = pmin(fullswede$MI_days, fullswede$CV_death_days)

#Censor everything above 365 so if had an event after 365 days secout is still 0
position <- which(fullswede$secout_days>365)
fullswede$MI_days[position] <- 365
fullswede$secout[position] <- 0

pos_zero <- which(fullswede$secout==0)
fullswede$secout_days[pos_zero] <- 365

#Now compute primary outcome (MI or All cause death) and days till primary outcome 

fullswede = fullswede %>% 
  mutate(primout = case_when((AMI == "1"  | dead == "1") ~ 1 , TRUE ~ 0))

fullswede$daysdeath = fullswede$days.to.death

fullswede$daysdeath[is.na(fullswede$daysdeath) == TRUE] = 365


#Censor everything above 365 for primary outcome as well. So if had an event after 365 days then primout is 0

fullswede$primout_days = pmin(fullswede$MI_days, fullswede$daysdeath)

position2 <- which(fullswede$primout_days>365)
fullswede$daysdeath[position2] <- 365
fullswede$primout[position2] <- 0

fullswede$primout = ifelse(fullswede$primout == 1 & fullswede$primout_days >=365, 0,
                           ifelse(fullswede$primout == 1 & fullswede$primout_days <365, 1, 0))


#further censor the secondary outcome now
fullswede$secout_days = pmin(fullswede$MI_days, fullswede$daysdeath)
view(fullswede)

# Now need to make a linear predictor column in this dataset using the equation 
# New equation inputed with the betas for MI or CV death (second outcome) 
fullswede$linearpredictor <- (-0.52936936 - 0.0047846242 * (fullswede$Age_Index) + 9.4179917e-06 * pmax(fullswede$Age_Index -48, 0)^3 + 
                                8.4420509e-05 * pmax(fullswede$Age_Index - 71, 0)^3 - 0.00020933838 * pmax(fullswede$Age_Index - 81, 0) ^3 +
                                0.00011549988 * pmax(fullswede$Age_Index - 91, 0)^3 -
                                0.010949948 * (fullswede$eGFR.computed) + 1.8856582e-06 * pmax(fullswede$eGFR.computed - 31, 0)^3 - 3.9537994e-06 * pmax(fullswede$eGFR.computed - 65, 0)^3 + 2.0681412e-06 * pmax(fullswede$eGFR.computed - 96, 0)^3 + 
                                0.15347577 * fullswede$logtroponin + 
                                0.45709098 * (fullswede$Anaemia == "Yes") + 
                                0.067704053 * (fullswede$`STT-deviation on ECG` == "Yes") + 
                                0.79012042 * (fullswede$PrevHFHospitalisation == "Yes") + 
                                0.35455466 * (fullswede$PrevDiabetes == "Yes") + 
                                0.17447512 * (fullswede$PrevIHD == "Yes") - 
                                0.0014624885 * (fullswede$HeartRate) - 8.8541454e-07 * pmax(fullswede$HeartRate - 65, 0)^3 + 1.493052e-06 * pmax(fullswede$HeartRate - 100, 0)^3 - 6.0763743e-07 * pmax(fullswede$HeartRate - 151, 0)^3)

# survival 365 for this outcome
s0365 = 0.8892512

fullswede$prob.secout = 1-s0365^exp(fullswede$linearpredictor)

summary(fullswede$prob.secout)

#New model - this is just an alternative validation using calibration slope
ddist <- datadist(fullswede)
options(datadist='ddist')
cox_external<- cph(Surv(secout_days, secout)~ linearpredictor, data=fullswede)
print(cox_external)


#New equation for primary (MI + All cause death) outcome using betas

fullswede$linearpredictor2 <-  (-2.7144998 + 0.028597071 * (fullswede$Age_Index) - 6.0631548e-06 * pmax(fullswede$Age_Index -48, 0)^3 + 
                                  0.00012444097 * pmax(fullswede$Age_Index - 71, 0)^3  - 0.00022281038 * pmax(fullswede$Age_Index - 81, 0)^3 +
                                  0.00010443256 * pmax(fullswede$Age_Index - 91 , 0)^3 - 0.016915066 * fullswede$eGFR.computed + 4.2808103e-06 * pmax(fullswede$eGFR.computed - 31, 0)^3 - 8.9758926e-06 * pmax(fullswede$eGFR.computed - 65, 0)^3 + 4.6950823e-06 * pmax(fullswede$eGFR.computed - 96, 0)^3 + 
                                  0.11030707 * fullswede$logtroponin + 
                                  0.41765085 * (fullswede$Anaemia == "Yes") + 
                                  0.16789179 * (fullswede$`STT-deviation on ECG` == "Yes") + 
                                  0.42463213 * (fullswede$PrevHFHospitalisation == "Yes") +
                                  0.30616934 * (fullswede$PrevDiabetes == "Yes") + 
                                  0.083418182 * (fullswede$PrevIHD == "Yes") +
                                  0.0062885784 * fullswede$HeartRate - 2.057656e-06 * pmax(fullswede$HeartRate - 65, 0)^3 + 3.4697729e-06 * pmax(fullswede$HeartRate - 100, 0)^3 - 1.4121169e-06 * pmax(fullswede$HeartRate - 151, 0)^3)

# 365 day survival for this outcome
s0365.2 = 0.7760240

fullswede$prob.primout = 1-s0365.2^exp(fullswede$linearpredictor2)

#new model for primary out

ddist <- datadist(fullswede)
options(datadist='ddist')
cox_external2<- cph(Surv(primout_days, primout)~ linearpredictor2, data=fullswede) 
cox_external2


#checking distribution with histogram
hist(fullswede$linearpredictor2, breaks = 100)

#AUC curve for new model for second outcome (MI or CV death)
fullswede$secout = as.factor(fullswede$secout)

roc1= roc(fullswede$secout, fullswede$prob.secout, auc.polygon = TRUE, 
          print.auc = TRUE, percent = TRUE, plot = TRUE, ci = TRUE)

ci.auc(roc1)

plot.roc(fullswede$secout, fullswede$prob.secout)

plot.roc(roc1, asp = NA, legacy.axes = TRUE, col="#00a087ff", pch=21, print.auc = TRUE,
         ylim = c(0,100))


#AUC curve for primary outcome of all-cause death or MI

sum(fullswede$primout)

roc2= roc(fullswede$primout, fullswede$prob.primout, auc.polygon = TRUE, ci = TRUE,
          print.auc = TRUE, percent = TRUE, legacy.axis = TRUE)


plot.roc(roc2, asp = NA, legacy.axes = TRUE, col="darkblue", pch=21, print.auc = TRUE, 
         ylim = c(0,100))



#For number of Mis at 1 year
fullswede = fullswede %>% 
  mutate(mi365 = case_when(( AMI == "1" & secout_days <365 ) ~ "Yes", TRUE ~ "No"))

table(fullswede$mi365)


#double ROC graph

plot.roc(roc1, asp=NA, legacy.axes = TRUE, lwd = 2.5, type = "l", col="#00a087ff", print.auc = TRUE, ci = TRUE, pch=21)


plot.roc(roc2, asp=NA, legacy.axes = TRUE, lwd = 2.5, pch=21, type = "l", col = "#3c588f", add = TRUE, ci = TRUE, print.auc = TRUE, ylim = c(0,100))

# exploratory code for cv death at one year

fullswede = fullswede %>% 
  mutate(CV_death1year = case_when (( CV_death_days <365 & CV_death == 1) ~ 1 , TRUE ~ 0))

table(fullswede$CV_death1year)

# Tables

fullswede$mygroup <- factor(fullswede$primout, 
                            levels = c(1,0), 
                            labels = c("Primary Outcome", "Censored"))

fullswede = fullswede %>% mutate(mygroup1 = case_when((mygroup == "Primary Outcome"  | primout_days <365 ) ~ "Primary Outcome" , TRUE ~ "Censored"))

table(fullswede$mygroup1)

fullswede$Sex <- relevel(fullswede$Sex, ref = "Male")

tab2_median <- fullswede %>% 
  select(Age_Index, Sex, `Prior MI`, PrevIHD, PrevCD, PrevDiabetes,
         PrevHFHospitalisation, Anaemia,  `STT-deviation on ECG`, 
         HeartRate, `Systolic BP`, Heamoglobin, eGFR.computed, `PeaktroponinT`, mygroup1) %>% 
  tbl_summary(by = mygroup1,
              label = list(Age_Index ~ "Age (years)",
                           Sex ~ "Male", 
                           `Prior MI` ~ "Myocardial infarction",
                           PrevIHD ~ "Ischaemic heart disease", 
                           PrevCD ~ "Cerebrovascular disease",
                           PrevDiabetes ~ "Diabetes Mellitus",
                           PrevHFHospitalisation ~ "Heart failure hospitalisation",
                           Anaemia ~ "Anaemia",
                           `STT-deviation on ECG` ~ "Myocardial Ischaemia",
                           HeartRate ~ "Heart rate (b.p.m)", 
                           `Systolic BP` ~ "Systolic blood pressure (mmHg)",
                           Heamoglobin ~ "Haemoglobin",
                           eGFR.computed ~ "eGFR (mL/min)", 
                           `PeaktroponinT` ~ "Peak hs-cTnT"), 
              statistic = list(Age_Index ~ "{mean} ({sd})",
                               Sex ~ "{n} ({p}%)",
                               `Prior MI` ~ "{n} ({p}%)", 
                               PrevIHD ~ "{n} ({p}%)",
                               PrevCD ~ "{n} ({p}%)",
                               PrevDiabetes ~ "{n} ({p}%)",
                               PrevHFHospitalisation ~ "{n} ({p}%)",
                               Anaemia ~ "{n} ({p}%)",
                               `STT-deviation on ECG` ~ "{n} ({p}%)",
                               HeartRate ~ "{mean} ({sd})",
                               `Systolic BP` ~ "{mean} ({sd})",
                               Heamoglobin ~ "{mean} ({sd})",
                               eGFR.computed ~ "{mean} ({sd})",  
                               `PeaktroponinT` ~ "{median} ({p25}, {p75})"),
              missing_text = "(Missing)", value = Sex~"Male") %>% 
  add_overall() %>% 
  modify_header(label ~ "") %>%
  modify_spanning_header(all_stat_cols() ~ "**Validation cohort**")%>%
  as_gt() %>%
  gt::tab_row_group(label = "Haematology and clinical chemistry",rows = 12:14)%>%
  gt::tab_row_group(label = "Physiological parameters", rows = 10:11) %>%
  gt::tab_row_group(label = "Electrocardiogram", rows = 9) %>%
  gt::tab_row_group(label = "Past medical history", rows = 3:8) %>% 
  gt::tab_row_group(label = "Demographics", rows = 1:2)


tab2_median


# Cumulative incidence plots for the primary oucome in risk categories

vTert = quantile(fullswede$prob.primout, c(0:3/3))
summary(fullswede$prob.primout)

fullswede$riskgroup = ifelse(fullswede$prob.primout <= 0.13, "Low",
                            ifelse(fullswede$prob.primout >0.13 & 
                                     fullswede$prob.primout <=0.34, "Intermediate" ,
                                    ifelse(fullswede$prob.primout >0.34, "High", "0" )))

survcurve.primout = survfit(Surv(primout_days, primout) ~ riskgroup, data = fullswede)

ggsurvplot(survcurve.primout, fun = "event" ,
           pval = TRUE, pval.method = TRUE, pval.size = 4, pval.coord = c(.2,.7), 
           pval.method.size = 4, pval.method.coord = c(.2, .8),
           risk.table = TRUE, risk.table.title = "",
           xlim = c(0 , 360), break.x.by = 90,
           xlab = "Time in days" , 
           ylab = "",
           legend.labs = c("High risk", "Intermediate risk", "Low risk") , 
           palette = c("#C00000", "#77CBDF", "#009A7F"),
          legend.title = "",
           surv.scale = "percent",
           surv.plot.height = .95,
           risk.table.height = .35,
           censor = FALSE )

survcurve.primout


#Sensitivity analysis - exclude events prior to 30 days

df30 = fullswede %>% 
  filter(primout_days > 30 & primout_days <365 | primout == 0 )


df30$linearpredictor30 <-  (-2.7144998 + 0.028597071 * (df30$Age_Index) - 6.0631548e-06 * pmax(df30$Age_Index -48, 0)^3 + 
                              0.00012444097 * pmax(df30$Age_Index - 71, 0)^3  - 0.00022281038 * pmax(df30$Age_Index - 81, 0)^3 +
                              0.00010443256 * pmax(df30$Age_Index - 91 , 0)^3 - 0.016915066 * df30$eGFR.computed + 4.2808103e-06 * pmax(df30$eGFR.computed - 31, 0)^3 - 8.9758926e-06 * pmax(df30$eGFR.computed - 65, 0)^3 + 4.6950823e-06 * pmax(df30$eGFR.computed - 96, 0)^3 + 
                              0.11030707 * df30$logtroponin + 
                              0.41765085 * (df30$Anaemia == "Yes") + 
                              0.16789179 * (df30$`STT-deviation on ECG` == "Yes") + 
                              0.42463213 * (df30$PrevHFHospitalisation == "Yes") +
                              0.30616934 * (df30$PrevDiabetes == "Yes") + 
                              0.083418182 * (df30$PrevIHD == "Yes") +
                              0.0062885784 * df30$HeartRate - 2.057656e-06 * pmax(df30$HeartRate - 65, 0)^3 + 3.4697729e-06 * pmax(df30$HeartRate - 100, 0)^3 - 1.4121169e-06 * pmax(df30$HeartRate - 151, 0)^3)

df30$prob.primout30 = 1-s0365.2^exp(df30$linearpredictor30)

roc30= roc(df30$primout, df30$prob.primout, auc.polygon = TRUE, ci = TRUE,
           print.auc = TRUE, percent = TRUE, legacy.axis = TRUE)


plot.roc(roc30, asp = NA, legacy.axes = TRUE, col="darkblue", pch=21, print.auc = TRUE, 
         ylim = c(0,100))

summary(fullswede$AMI)

#Calibration plots

library(pec)
Age_splines <- rcspline.eval(fullswede$Age_Index, knots = c(48, 71, 81, 91))
Age_splines = as.data.frame(Age_splines)
print(Age_splines)
fullswede_spline = fullswede
fullswede_spline$ageprime <- c(Age_splines$V1)
fullswede_spline$ageprime2 <- c(Age_splines$V2)
HeartRate_splines <-rcspline.eval(fullswede$HeartRate, knots = c(65, 100, 151))
HeartRate_splines = as.data.frame(HeartRate_splines)
fullswede_spline$HeartRateprime <-c(HeartRate_splines$V1)
eGFR_splines <- rcspline.eval(fullswede$eGFR.computed, knots = c(31, 65, 96))
eGFR_splines <- as.data.frame(eGFR_splines)
fullswede_spline$eGFRprime <- c(eGFR_splines$V1)

#To make probabilty compatable with survival primary outcome
prob.primout.surv = s0365.2^exp(fullswede$linearpredictor2)

# Primary outcome smooth
calPlot(prob.primout.surv,
         time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
  Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
  PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
         data=fullswede_spline, type="survival", 
         xlim=c(.10,1),
          ylim=c(.10,1),
        col = "#3c588f")

# Primary outcome binning
calPlot(prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "quantile",
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")

#To make probabilty compatable with survival second outcome
prob.secout.surv = s0365^exp(fullswede$linearpredictor)
sort(unique(prob.secout.surv))

# Second outcome calibration plot 
calPlot(prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk",
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "green")

# Combined plots

calPlot(prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "quantile", lty = 0, lwd = 6,  percent = FALSE, legend = FALSE,
        xlim=c(.00,1),
        ylim=c(.00,1),
        col = "#00a087ff")

calPlot(prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "quantile", lty = 0, lwd = 6, add = TRUE, legend = FALSE, percent = FALSE,
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")

#Smooth plot combined

calPlot(prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "nne",  lwd = 3,  percent = FALSE, legend = FALSE,
        xlim=c(.00,1),
        ylim=c(.00,1),
        col = "#00a087ff")

calPlot(prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "nne",  lwd = 3, add = TRUE, legend = FALSE, percent = FALSE,
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")

# Discrete plot combined

calPlot(prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "quantile",  lwd = 6,  percent = FALSE, legend = FALSE, lty = 0,
        xlim=c(.00,1),
        ylim=c(.00,1),
        col = "#00a087ff")

calPlot(prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponin + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=fullswede_spline, type="risk", method = "quantile",  lwd = 6, add = TRUE, legend = FALSE, percent = FALSE, lty = 0,
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")


