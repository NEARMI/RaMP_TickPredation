## copy of TickConsumption.R - test area for survival model
## analysis of experimental data for invertebrate feeding trials to red-backed 
## salamanders, in simple and complex mesocosms
## The purpose of this script is to calculate and compare survival curves for 
## prey exposure experiment
## Written by ehgrant
## Questions: ehgrant at usgs dot gov

install.packages(c("ggplot2"))
####################
#0. Preliminaries 
# load libraries
####################
library(survival)
library(coxme)#for mixed effects models
library(ggsurvfit)
library(gtsummary) #use to generate table of cox Hazard Ratio results
library(riskRegression) #use to plot 
library(adjustedCurves) #use to plot 
library(rms) #needed in adjustedCurves
library(pammtools) #needed in adjustedCurves
library(dplyr)
library(survminer)
library(ggplot2)
library(survMisc)


##################***FRUIT FLY SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
setwd("C:\\Users\\eedwards\\DOI\\GS-PWRC-NEARMI - General\\RaMP_TickPredation\\RaMP_TickPredation_Git\\data")
prey<-read.csv(file = "Trial5_Fly_TreatmentCSV.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
gof(coxmod.frail)
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("darkgray", "#0072B2", "#F0E442", "#D55E00"))+labs(y= "Fruit Fly Survival", x = "Days") 








##################***DEER TICK ADULT SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "Trial5_Deer_TreatmentCSV.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 
### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("#0072B2", "#F0E442", "#D55E00"))+labs(y= "Adult Deer Tick Survival", x = "Days") 





##################***DOG TICK NYMPH SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "DogNymph_ALLCOMBINED.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
gof(coxmod.frail)
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("darkgrey", "#F0E442", "#D55E00"))+labs(y= "Dog Tick Nymph Survival", x = "Days") 






##################***DOG TICK ADULT SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "Trial5_DogAdult_TreatmentCSV.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
gof(coxmod.frail,4)
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("#0072B2", "#F0E442", "#D55E00"))+labs(y= "Dog Tick Adult Survival", x = "Days") 









##################***ALL DATA COMBINED***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "AllData2.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("orange", "lightblue"))+labs(y= "Prey Survival", x = "Days") 





##################***Deer Adult Dog Nymph Alt***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "DeerAdult_DogNymph_CPLXcondensed2.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx) #specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable="Rx",
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("orange", "lightblue"))+labs(y= "Tick Survival", x = "Days") 
#0x 1x colors "darkgray", "#0072B2"
#2x 3x colors "#F0E442", "#D55E00"







##################***Deer Adult/Dog Nymph COMBINED***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/data")
prey<-read.csv(file = "DeerDog_preyfactor.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx)
prey$Sp<-as.factor(prey$Sp)#specify as factor
########################
#2. survival analysis
########################

###Kaplan-Meier survival model
#overall survival analysis (individual salamanders as a factor)
(s1 <- survfit(Surv(Eaten_day,outcome) ~ Rx+cluster, data = prey))

#plot the survival curves
survfit2(Surv(Eaten_day,outcome) ~ Rx+Sp+cluster, data = prey) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

### Cox proportional hazards model 
#mixed effects cox ph model (random intercept for each salamander)
(coxmod.me<-coxme(Surv(Eaten_day,outcome) ~ Rx + Sp + (1|cluster), data = prey))

#same as above but using frailty model form
(coxmod.frail<-coxph(Surv(Eaten_day,outcome) ~ Rx + Sp + frailty(cluster), data = prey))
summary(coxmod.frail) #exp(coef) is the Hazard Ratio for a covariate
#########################
#3. Plot survival curve by treatment
#########################
predict_fun <- function(...) {
  1 - predictRisk(...)
}

# bootstrap confidence intervals using adjustedCurves package
adjsurv.sals.bs <- adjustedsurv(data=prey,
                                variable= c("Rx", "Sp"),  #not sure how to add Sp into this part of the model may need a different model (see below)
                                ev_time="Eaten_day",
                                event="outcome",
                                method="direct",
                                bootstrap=TRUE,
                                n_boot=150,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE)+labs(y= "Tick Survival", x = "Days") 







###########################################
####Multivariate Cox Regression Analysis###
prey<-read.csv(file = "DeerDog_preyfactor.csv",header=TRUE)
#rename sal -> cluster  for use in riskRegression package
prey<-rename(prey,cluster = sal)
prey$Rx<-as.factor(prey$Rx)
prey$Sp<-as.factor(prey$Sp)

res.cox <- coxph(Surv(Eaten_day,outcome) ~  Sp + frailty(cluster), data = prey)
summary(res.cox)

# Plot the baseline survival function
ggsurvplot(survfit(res.cox, data=prey), palette = "#0072B2")


######### Plotting #########
#using survminer package
library("survminer")
library("survival")
# Fit cox ph model - no frailty term
res.cox<-coxph(Surv(Eaten_day,outcome) ~ Rx + Sp , data = prey)

# Create the new data  
Rx_Sp_df <- with(prey,
                 data.frame(Rx = c("0x", "1x", "2x", "3x"), 
                            Sp = c("I. scapularis (Adult)", "D. variabilis (Nymph)")  
                 )
)
Rx_Sp_df$Sp<-as.factor(Rx_Sp_df$Sp)

# Survival curves with new data
fit <- survfit(res.cox, newdata = Rx_Sp_df)
ggsurvplot(fit, data=Rx_Sp_df, conf.int = TRUE,  
           surv.median.line = "hv", )
######################NO GOOD^^^







###################Plot Median Surv Time results##########################
# library
library(ggplot2)

setwd("~/Git/RaMP_TickPredation/data")
medsurv<-read.csv(file = "R_Results.csv",header=TRUE)
medsurv$Prey<-as.factor(medsurv$Prey)
head(medsurv)

# The iris dataset is provided natively by R
#head(iris)

# basic scatterplot
ggplot(medsurv, aes(x=Treatment, y=MedianSurvTime, color=Prey)) + 
  geom_jitter(size=3, width = 0.35, height = 0.35) +
  geom_smooth(method=lm, color="black") + 
  ylab("Median survival time (days)") +
  xlab("Stems/m^2") +
  ggtitle("Median Prey Survival by Stem Density") +
  theme(plot.title = element_text(hjust = 0.5))


