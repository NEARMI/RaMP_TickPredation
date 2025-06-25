## copy of TickConsumption.R - test area for survival model
## analysis of experimental data for invertebrate feeding trials to red-backed 
## salamanders, in simple and complex mesocosms
## The purpose of this script is to calculate and compare survival curves for 
## prey exposure experiment
## Written by ehgrant
## Questions: ehgrant at usgs dot gov


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


##################***FRUIT FLY SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/code")
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
setwd("~/Git/RaMP_TickPredation/code")
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
setwd("~/Git/RaMP_TickPredation/code")
prey<-read.csv(file = "Trial6_Nymph_TreatmentCSV.csv",header=TRUE)
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
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("darkgrey", "#F0E442", "#D55E00"))+labs(y= "Dog Tick Nymph Survival", x = "Days") 






##################***DOG TICK ADULT SURVIVAL***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation/code")
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
setwd("~/Git/RaMP_TickPredation/code")
prey<-read.csv(file = "AllData.csv",header=TRUE)
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
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("darkgray", "#0072B2", "#F0E442", "#D55E00"))+labs(y= "Prey Survival", x = "Days") 




##################***Deer Adult/Dog Nymph COMBINED***#######################
#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable. 
setwd("~/Git/RaMP_TickPredation")
prey<-read.csv(file = "data/DeerAdult_DogNymph.csv",header=TRUE)
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
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE, median_surv_lines=TRUE, custom_colors=c("darkgray", "#0072B2", "#F0E442", "#D55E00"))+labs(y= "Prey Survival", x = "Days") 




