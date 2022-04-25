#Author: Caelan Taggart and Karla Monterubbio
#Date 24/09/2021
#Validation for single centre cohort 


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
GRACE_AR_220110 <- read_excel("PhD/Demand Score/GRACE_AR_220110.xlsx",
                              col_types = c("numeric", "date", "date",
                                            "numeric", "date", "numeric", "date",
                                            "numeric", "numeric", "numeric"))




fullswede = merge(GRACE_AR_220110, karolinska_t2_cleaned1,by="lpnr",all.y = TRUE)
View(fullswede)
View(test)




# Exclude the three st-deviation missing patients - 24185 and 29162 and one wit no outcome variables

fullswede = fullswede[-c(which(fullswede$lpnr==24185),
                         which(fullswede$lpnr==29162),which(fullswede$lpnr==2716)),]



#Sort out factors
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



fullswede$logtroponin = log(fullswede$`PeaktroponinT`) # Swedish used trop T 



fullswede$logegfr = fullswede$`log efgr`



fullswede$log.topI = fullswede$logtroponin
view(fullswede)



#Transform log trop T data into log trop I from linear regression model. 
fullswede$logtroponin = 0.04595 + 1.23005*fullswede$log.topI



fullswede$PrevIHD = as.factor(fullswede$PrevIHD)
fullswede$Anaemia = as.factor(fullswede$Anaemia)



#Make outcome columbs and dates
#Make CV death or MI (secondary outcome) first as will needs censoring for Non-CV death. Done this by limiting the time to event variables



fullswede = fullswede %>%
  mutate(secout = case_when((AMI == "1" | CV_death == "1") ~ 1 , TRUE ~ 0))



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
  mutate(primout = case_when((AMI == "1" | dead == "1") ~ 1 , TRUE ~ 0))



fullswede$daysdeath = fullswede$days.to.death



fullswede$daysdeath[is.na(fullswede$daysdeath) == TRUE] = 365


fullswede$primout_days = pmin(fullswede$MI_days, fullswede$daysdeath)



position2 <- which(fullswede$primout_days>365)
fullswede$daysdeath[position2] <- 365
fullswede$primout[position2] <- 0



fullswede$primout = ifelse(fullswede$primout == 1 & fullswede$primout_days >=365, 0,
                           ifelse(fullswede$primout == 1 & fullswede$primout_days <365, 1, 0))




#further censor the secondary outcome now
fullswede$secout_days = pmin(fullswede$MI_days, fullswede$daysdeath)



view(fullswede)






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



#New model - compute calibration slope
ddist <- datadist(fullswede)
options(datadist='ddist')
cox_external<- cph(Surv(secout_days, secout)~ linearpredictor, data=fullswede)
cox_external




sum(fullswede$secout)



#New equation for primary (MI + All cause death) outcome 



fullswede$linearpredictor2 <- (-2.7144998 + 0.028597071 * (fullswede$Age_Index) - 6.0631548e-06 * pmax(fullswede$Age_Index -48, 0)^3 +
                                 0.00012444097 * pmax(fullswede$Age_Index - 71, 0)^3 - 0.00022281038 * pmax(fullswede$Age_Index - 81, 0)^3 +
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
#complete_data = fullswede
#complete_data$cox1 = predictrms(cox_external, data = complete_data)



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



view(fullswede)




#For number of Mis at 1 year
fullswede = fullswede %>%
  mutate(mi365 = case_when(( AMI == "1" & secout_days <365 ) ~ "Yes", TRUE ~ "No"))



table(fullswede$mi365)




#double ROC graph



plot.roc(roc1, asp=NA, legacy.axes = TRUE, lwd = 2.5, type = "l", col="#00a087ff", print.auc = TRUE, ci = TRUE, pch=21)




plot.roc(roc2, asp=NA, legacy.axes = TRUE, lwd = 2.5, pch=21, type = "l", col = "#3c588f", add = TRUE, ci = TRUE, print.auc = TRUE, ylim = c(0,100)
)



# exploratory code for cv death at one year



fullswede = fullswede %>%
  mutate(CV_death1year = case_when (( CV_death_days <365 & CV_death == 1) ~ 1 , TRUE ~ 0))



table(fullswede$CV_death1year)



# Tables



fullswede$mygroup <- factor(fullswede$primout,
                            levels = c(1,0),
                            labels = c("Primary Outcome", "Censored"))



fullswede = fullswede %>% mutate(mygroup1 = case_when((mygroup == "Primary Outcome" | primout_days <365 ) ~ "Primary Outcome" , TRUE ~ "Censored"))



table(fullswede$mygroup1)



fullswede$Sex <- relevel(fullswede$Sex, ref = "Male")



tab2_median <- fullswede %>%
  select(Age_Index, Sex, `Prior MI`, PrevIHD, PrevCD, PrevDiabetes,
         PrevHFHospitalisation, Anaemia, `STT-deviation on ECG`,
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
           palette = "npg" ,
           legend.title = "",
           surv.scale = "percent",
           surv.plot.height = .95,
           risk.table.height = .35,
           censor = FALSE )




survcurve.primout