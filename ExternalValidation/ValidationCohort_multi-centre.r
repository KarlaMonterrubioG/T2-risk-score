#Author: Jasper Boedeinghaus and Caelan Taggart
#Date 11/04/2022
#Validation for DEMAND-MI Score  


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
library(haven)
library(Hmisc)
library(rio)
library(labelled)
update.packages("fastmap")

# Load dataframes
setwd("~/PhD/Demand Score/Validation data")
df<-read_sav("./DEMAND.sav",encoding = "latin1")
#df = dataframe containing variables in dictionary 

#change !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#rand<-sample(1:nrow(df), 100)
#df$PrevHFHospitalisation<-""
#df$PrevHFHospitalisation[rand] <- "yes"
#df$PrevHFHospitalisation[-rand] <- "no"

#basel conversion
df<-df %>% mutate_if(is.labelled,to_character)
df<-df %>% mutate(CV_death=replace_na(cvdeath, 0))
df<-df %>% mutate(CV_death_date=if_else(CV_death==1,FUDatumTod,as.Date(NA)))
df<-df %>% mutate(AMI=replace_na(AMI, 0))
df<-df %>% mutate(dead=replace_na(dead,0))
#df<-df %>% rowwise %>% mutate(AMI_date=if_else(AMI==1,min(FUDatumAMI.1,FUDatumAMI.2,na.rm = T),as.Date(NA)))
df<-df %>% rename(daysdeath=days_to_death)
df<-df %>% rename(indexdate=Indexdate)
df<-df %>% rename(eGFR.computed=eGFR_computed)
df<-df %>% rename(`STT-deviation on ECG`=STTdeviation)
df<-df %>% rename(`Prior MI` =PriorMI)
df<-df %>% rename(`Systolic BP` =SystolicBP)

df$logtroponinLN = -0.36759 + 1.35335*df$logtroponinLN

df<-df %>% select(dead,CV_death,CV_death_date,AMI,AMI_date,daysdeath,indexdate,Sex,Haemoglobin,logtroponinLN,
                  Age_Index,Anaemia,`STT-deviation on ECG`,PrevHFHospitalisation,PrevDiabetes,
                  PrevIHD,PrevDiabetes,eGFR.computed,HeartRate,
                  `Prior MI`,PrevCD,`Systolic BP`,PeakhscTnT
                  )

is_bool<-function(x){
  all(x %in% c(0,1,NA))
}

convert_<-function(x){
  x[x==1]<-"Yes"  
  x[x==0]<-"No"
  return(x)
}

df<-df %>% mutate_if(is_bool,convert_)


#remove 1 patient with incomplete data
df = df %>% drop_na(Haemoglobin)

#create factorial haemoglobin variable 
df = df %>% 
  mutate(Anaemia = ifelse(Sex=="female", ifelse(Haemoglobin <120 , "Yes" , "No"), 
                          ifelse(Haemoglobin <130, "Yes", "No")))

#log troponin
#df$logtroponin = log(df$`Peak hs-cTnI`) 
#df<-df %>% select(-logtroponin)
#df<-df %>% rename(logtroponin=logtroponinLN)


view(df)

#Create outcome columns and dates
#Make CV death or MI (secondary outcome) first as  need to censor for Non-CV death. 
#Follow up is to be curtailed at one year [events beyond one year are not included]

#days to event for MI and censor to 365
df$MI_days = as.numeric(round(difftime(df$AMI_date,
                                       df$indexdate, units = "days")))
df$MI_days[is.na(df$MI_days)==TRUE] = 365

#Redefinition of AMI without index events. 
# df<-df %>% 
 # mutate(AMI = case_when((MI_days > 0 & AMI == "Yes" ) ~ "Yes", TRUE ~ "No"))

df = df %>% 
  mutate(secout = case_when((AMI == "Yes"  | CV_death == "Yes") ~ 1 , TRUE ~ 0))

#days to event for CV death and censor to 365 
df$CV_death_days = as.numeric(round(difftime(df$CV_death_date,
                                                    df$indexdate, units = "days")))
df$CV_death_days[is.na(df$CV_death_days) == TRUE] = 365

#Days till secondary outcome
df$secout_days = pmin(df$MI_days, df$CV_death_days)

#Censor everything above 365 so if had an event after 365 days secout is still 0
position <- which(df$secout_days>365)
df$MI_days[position] <- 365
df$secout[position] <- 0

pos_zero <- which(df$secout==0)
df$secout_days[pos_zero] <- 365
view(df)

#Now compute primary outcome (MI or All cause death) and days til primary outcome 

df = df %>% 
  mutate(primout = case_when((AMI == "Yes"  | dead == "Yes") ~ 1 , TRUE ~ 0))

#df$daysdeath = df$days.to.death

df$daysdeath[is.na(df$daysdeath) == TRUE] = 365


#Censor everything above 365 for primary outcome as well. So if had an event after 365 days then primout is 0

#old position code
#position2 <- which(df$daysdeath>365)
#df$daysdeath[position2] <- 365
#df$second.out[position2] <- 0

df$primout_days = pmin(df$MI_days, df$daysdeath)

position2 <- which(df$primout_days>365)
df$daysdeath[position2] <- 365
df$primout[position2] <- 0

df$primout = ifelse(df$primout == 1 & df$primout_days >=365, 0,
                           ifelse(df$primout == 1 & df$primout_days <365, 1, 0))


#further censor the secondary outcome now
df$secout_days = pmin(df$MI_days, df$daysdeath)

df$secout = ifelse(df$secout == 1 & df$secout_days >=365, 0,
                           ifelse(df$secout == 1 & df$secout_days <365, 1, 0))

view(df)
#library(openxlsx)
#write.xlsx(df,"./output/df.xlsx",overwrite = T)
# Now need to make a linear predictor column in this dataset using the equation 
# New equation inputted with the betas for MI or CV death (second outcome) 

df$linearpredictor <- as.double(NA)
df$linearpredictor <- (-0.52936936 - 0.0047846242 * (df$Age_Index) + 9.4179917e-06 * pmax(df$Age_Index -48, 0)^3 + 
                                8.4420509e-05 * pmax(df$Age_Index - 71, 0)^3 - 0.00020933838 * pmax(df$Age_Index - 81, 0) ^3 +
                                0.00011549988 * pmax(df$Age_Index - 91, 0)^3 -
                                0.010949948 * (df$eGFR.computed) + 1.8856582e-06 * pmax(df$eGFR.computed - 31, 0)^3 - 3.9537994e-06 * pmax(df$eGFR.computed - 65, 0)^3 + 2.0681412e-06 * pmax(df$eGFR.computed - 96, 0)^3 + 
                                0.15347577 * df$logtroponinLN + 
                                0.45709098 * (df$Anaemia == "Yes") + 
                                0.067704053 * (df$`STT-deviation on ECG` == "Yes") + 
                                0.79012042 * (df$PrevHFHospitalisation == "Yes") + 
                                0.35455466 * (df$PrevDiabetes == "Yes") + 
                                0.17447512 * (df$PrevIHD == "Yes") - 
                                0.0014624885 * (df$HeartRate) - 8.8541454e-07 * pmax(df$HeartRate - 65, 0)^3 + 1.493052e-06 * pmax(df$HeartRate - 100, 0)^3 - 6.0763743e-07 * pmax(df$HeartRate - 151, 0)^3)

# survival 365 for this outcome
s0365 = 0.8892512
#s0365 =1-sum(df$secout)/length(df$secout)

df$prob.secout = 1-s0365^exp(df$linearpredictor)

summary(df$prob.secout)

#New model - this is just an alternative check of discrimination
ddist <- datadist(df)
options(datadist='ddist')
cox_external<- cph(Surv(secout_days, secout)~ linearpredictor, data=df)
cox_external
.491/2+.5

sum(df$secout)
sum(df$primout)

#New equation for primary (MI + All cause death) outcome using betas

df$linearpredictor2 <-  (-2.7144998 + 0.028597071 * (df$Age_Index) - 6.0631548e-06 * pmax(df$Age_Index -48, 0)^3 + 
                                  0.00012444097 * pmax(df$Age_Index - 71, 0)^3  - 0.00022281038 * pmax(df$Age_Index - 81, 0)^3 +
                                  0.00010443256 * pmax(df$Age_Index - 91 , 0)^3 - 0.016915066 * df$eGFR.computed + 4.2808103e-06 * pmax(df$eGFR.computed - 31, 0)^3 - 8.9758926e-06 * pmax(df$eGFR.computed - 65, 0)^3 + 4.6950823e-06 * pmax(df$eGFR.computed - 96, 0)^3 + 
                                  0.11030707 * df$logtroponinLN + 
                                  0.41765085 * (df$Anaemia == "Yes") + 
                                  0.16789179 * (df$`STT-deviation on ECG` == "Yes") + 
                                  0.42463213 * (df$PrevHFHospitalisation == "Yes") +
                                  0.30616934 * (df$PrevDiabetes == "Yes") + 
                                  0.083418182 * (df$PrevIHD == "Yes") +
                                  0.0062885784 * df$HeartRate - 2.057656e-06 * pmax(df$HeartRate - 65, 0)^3 + 3.4697729e-06 * pmax(df$HeartRate - 100, 0)^3 - 1.4121169e-06 * pmax(df$HeartRate - 151, 0)^3)

# 365 day survival for this outcome
#s0365.2 = 1-sum(df$primout)/length(df$primout)
s0365.2 = 0.776024

df$prob.primout = 1-s0365.2^exp(df$linearpredictor2)
summary(df$prob.primout)

#new model for primary out

ddist <- datadist(df)
options(datadist='ddist')
cox_external2<- cph(Surv(primout_days, primout)~ linearpredictor2, data=df) 
cox_external2

#checking distrobution with histogram
hist(df$linearpredictor2, breaks = 100)

#AUC curve for new model for second outcome (MI or CV death)
#complete_data = df
#complete_data$cox1 = predictrms(cox_external, data = complete_data)

df$secout = as.factor(df$secout)

roc1= roc(df$secout, df$prob.secout, auc.polygon = TRUE, 
          print.auc = TRUE, percent = TRUE, plot = TRUE, ci = TRUE)


ci.auc(roc1)

plot.roc(df$secout, df$prob.secout)

plot.roc(roc1, asp = NA, legacy.axes = TRUE, col="#00a087ff", pch=21, print.auc = TRUE,
         ylim = c(0,100))


#AUC curve for primary outcome of all-cause death or MI

sum(df$primout)

roc2= roc(df$primout, df$prob.primout, auc.polygon = TRUE, ci = TRUE,
          print.auc = TRUE, percent = TRUE, legacy.axis = TRUE)


plot.roc(roc2, asp = NA, legacy.axes = TRUE, col="darkblue", pch=21, print.auc = TRUE, 
         ylim = c(0,100))

view(df)



#test overlaying ROC curves
plot.roc(roc1, asp = NA, legacy.axes = TRUE, col="#00a087ff", pch=21, print.auc = TRUE, ylim = c(0,100)) 
plot.roc(roc2, asp = NA, legacy.axes = TRUE, col="darkblue", pch=21, print.auc = TRUE, ylim = c(0,100), (add = TRUE))

#double ROC graph
#png("./output/roc_secout.png", width = 1500, height = 1500,res=250)
#plot.roc(roc1, asp=NA, legacy.axes = TRUE, lwd = 2.5, type = "l", col="#00a087ff", print.auc = TRUE, ci = TRUE, pch=200)
#dev.off()

# svg("./output/roc_secout.svg", width = 1500, height = 1500,)
# plot.roc(roc1, asp=NA, legacy.axes = TRUE, lwd = 2.5, type = "l", col="#00a087ff", print.auc = TRUE, ci = TRUE, pch=200)
# dev.off()

#png("./output/roc_primout.png", width = 1500, height = 1500,res=250)
#plot.roc(roc2, asp=NA, legacy.axes = TRUE, lwd = 2.5, pch=21, type = "l", col = "#3c588f", ci = TRUE, print.auc = TRUE, ylim = c(0,100))
#dev.off()

#df<-data.frame(secout_sens=roc1$sensitivities,secout_spec=roc1$specificities,
 #          primout_sens=roc2$sensitivities,primout_spec=roc2$specificities)

#write.xlsx(df,"./output/sens_spec.xlsx")
# svg("./output/roc_primout.svg", width = 1500, height = 1500)
# plot.roc(roc2, asp=NA, legacy.axes = TRUE, lwd = 2.5, pch=21, type = "l", col = "#3c588f", ci = TRUE, print.auc = TRUE, ylim = c(0,100))
# dev.off()

# png("./output/roc_primout_secout.png", width = 1500, height = 1500,res=250)
# plot.roc(roc1, asp=NA, legacy.axes = TRUE, lwd = 2.5, type = "l", col="#00a087ff", print.auc = TRUE, ci = TRUE, pch=200)
# plot.roc(roc2, asp=NA, legacy.axes = TRUE,add=T, lwd = 2.5, pch=21, type = "l", col = "#3c588f", ci = TRUE, print.auc = TRUE, ylim = c(0,100))
# dev.off()

# exploratory code for cv death, death and MI at one year

df = df %>% 
  mutate(CV_death1year = case_when (( CV_death_days <365 & CV_death == "Yes") ~ 1 , TRUE ~ 0))

table(df$CV_death1year)

df = df %>% 
  mutate(mi365 = case_when(( AMI == "Yes" & secout_days <365 ) ~ "Yes", TRUE ~ "No"))

table(df$mi365)

df = df %>%  
  mutate(dead1y = case_when((primout_days <365 & dead == "Yes") ~ "Yes", TRUE ~ "No"))

table(df$dead1y)

#CIF code for Jasper 

#extra package needed

install.packages("survminer")
library(survminer)

# Look at the summary of the primary outcome probability column generated earlier
# This will provide you with the lower and upper quartile values of risk. (for example this could be 0.12 for lower and o.3 for higher)

summary(df$prob.primout)

# Define your risk groups around these values with the lower quartile of risk being "low" and the higest quartile of risk being "high"
# With the middle 50% being intermediate. Just delete txt and insert values where indicated. 

df$riskgroup = ifelse(df$prob.primout <= 0.13
                        , "Low",
                      ifelse(df$prob.primout > 0.13
                               & 
                               df$prob.primout <= 0.34
                               , "Intermediate" ,
                             ifelse(df$prob.primout > 0.34
                                      , "High", "0" )))

# Cumulative incidence equation - primout_days is time to event.

survcurve.primout = survfit(Surv(primout_days, primout) ~ riskgroup, data = df)

# Plot curve - this needs the survminer package

ggsurvplot(survcurve.primout, fun = "event" ,
           pval = TRUE, pval.method = TRUE, pval.size = 4, pval.coord = c(.2,.7), 
           pval.method.size = 4, pval.method.coord = c(.4, .6), 
           risk.table = TRUE, risk.table.title = "",
           xlim = c(0 , 360), break.x.by = 90, ylim = c(0, .7),
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

table(df$AMI)

table(df$primout)
table(df$secout)

#Landmark analysis 

df30 = df %>% 
  filter(primout_days > 30 & primout_days <365 | primout == 0 )


df30$linearpredictor30 <-  (-2.7144998 + 0.028597071 * (df30$Age_Index) - 6.0631548e-06 * pmax(df30$Age_Index -48, 0)^3 + 
                              0.00012444097 * pmax(df30$Age_Index - 71, 0)^3  - 0.00022281038 * pmax(df30$Age_Index - 81, 0)^3 +
                              0.00010443256 * pmax(df30$Age_Index - 91 , 0)^3 - 0.016915066 * df30$eGFR.computed + 4.2808103e-06 * pmax(df30$eGFR.computed - 31, 0)^3 - 8.9758926e-06 * pmax(df30$eGFR.computed - 65, 0)^3 + 4.6950823e-06 * pmax(df30$eGFR.computed - 96, 0)^3 + 
                              0.11030707 * df30$logtroponinLN + 
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

table(df$secout)


#Calibration plots

library(pec)
Age_splines <- rcspline.eval(df$Age_Index, knots = c(48, 71, 81, 91))
Age_splines = as.data.frame(Age_splines)
print(Age_splines)
df_spline = df
df_spline$ageprime <- c(Age_splines$V1)
df_spline$ageprime2 <- c(Age_splines$V2)
HeartRate_splines <-rcspline.eval(df$HeartRate, knots = c(65, 100, 151))
HeartRate_splines = as.data.frame(HeartRate_splines)
df_spline$HeartRateprime <-c(HeartRate_splines$V1)
eGFR_splines <- rcspline.eval(df$eGFR.computed, knots = c(31, 65, 96))
eGFR_splines <- as.data.frame(eGFR_splines)
df_spline$eGFRprime <- c(eGFR_splines$V1)

#To make probabilty compatable with survival primary outcome
df_spline$prob.primout.surv = s0365.2^exp(df$linearpredictor2)

# Prime outcome smooth
calPlot(df_spline$prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=df_spline, type="survival", 
        xlim=c(.10,1),
        ylim=c(.10,1),
        col = "#3c588f")

# KM prime outcome
calPlot(df_spline$prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=df_spline, type="survival", method = "quantile",
        xlim=c(.10,1),
        ylim=c(.10,1),
        col = "#3c588f")



#To make probabilty compatable with survival second outcome
df_spline$prob.secout.surv = s0365^exp(df$linearpredictor)

# Second outcome smooth
calPlot(df_spline$prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=df_spline, type="survival", 
        xlim=c(.10,1),
        ylim=c(.10,1),
        col = "green")

# KM second outcome
calPlot(df_spline$prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime,
        data=df_spline, type="survival", method = "quantile",
        xlim=c(.10,1),
        ylim=c(.10,1),
        col = "green")

# combined KM
calPlot(df_spline$prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime, lwd = 6, lty = 0,
        data=df_spline, type="risk", method = "quantile" , percent = FALSE, legend = FALSE,
        xlim=c(.00,1),
        ylim=c(.00,1),
        col = "#00a087ff")

calPlot(df_spline$prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime, lwd = 6, lty = 0,
        data=df_spline, type="risk", method = "quantile", add = TRUE, legend = FALSE, percent = FALSE,
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")


# combined smooth
calPlot(df_spline$prob.secout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime, lwd = 3,
        data=df_spline, type="risk", method = "nne" , percent = FALSE, legend = FALSE,
        xlim=c(.00,1),
        ylim=c(.00,1),
        col = "#00a087ff")

calPlot(df_spline$prob.primout.surv,
        time=365, Surv(primout_days,primout)~ Age_Index + ageprime + ageprime2 + eGFR.computed + eGFRprime + logtroponinLN + 
          Anaemia + `STT-deviation on ECG` + PrevHFHospitalisation + 
          PrevDiabetes + PrevIHD + HeartRate + HeartRateprime, lwd = 3,
        data=df_spline, type="risk", method = "nne", add = TRUE, legend = FALSE, percent = FALSE,
        xlim=c(.0,1),
        ylim=c(.0,1),
        col = "#3c588f")


summary
summary(df$logtroponinLN)

# Tables

df$mygroup <- factor(df$primout, 
                     levels = c(1,0), 
                     labels = c("Primary Outcome", "Censored"))

df = df %>% mutate(mygroup1 = case_when((mygroup == "Primary Outcome"  | primout_days <365 ) ~ "Primary Outcome" , TRUE ~ "Censored"))

table(df$mygroup1)

df$Sex <- relevel(as.factor(df$Sex), ref = "male")

tab2_median <- df %>% 
  select(Age_Index, Sex, `Prior MI`, PrevIHD, PrevCD, PrevDiabetes,
         PrevHFHospitalisation, Anaemia,  `STT-deviation on ECG`, 
         HeartRate, `Systolic BP`, Haemoglobin, eGFR.computed, `PeakhscTnT`, mygroup1) %>% 
  tbl_summary(by = mygroup1,
              label = list(Age_Index ~ "Age (years)",
                           Sex ~ "male", 
                           `Prior MI` ~ "Myocardial infarction",
                           PrevIHD ~ "Ischaemic heart disease", 
                           PrevCD ~ "Cerebrovascular disease",
                           PrevDiabetes ~ "Diabetes Mellitus",
                           PrevHFHospitalisation ~ "Heart failure hospitalisation",
                           Anaemia ~ "Anaemia",
                           `STT-deviation on ECG` ~ "Myocardial Ischaemia",
                           HeartRate ~ "Heart rate (b.p.m)", 
                           `Systolic BP` ~ "Systolic blood pressure (mmHg)",
                           Haemoglobin ~ "Haemoglobin",
                           eGFR.computed ~ "eGFR (mL/min)", 
                           `PeakhscTnT` ~ "Peak hs-cTnT"), 
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
                               Haemoglobin ~ "{mean} ({sd})",
                               eGFR.computed ~ "{mean} ({sd})",  
                               `PeakhscTnT` ~ "{median} ({p25}, {p75})"),
              missing_text = "(Missing)", value = Sex~"male") %>% 
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


