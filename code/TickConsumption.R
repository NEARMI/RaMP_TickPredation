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

#########################
#1. import data
#########################
# Note the format of the data from experimental trials.
# For each salamander mesocosm, reformat the daily count data to 
# create individual prey survival time datatable.
prey<-read.csv(file = "data/reformattedtrial3.csv",header=TRUE)
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
                                n_boot=500,
                                outcome_model=coxmod.frail,
                                predict_fun=predict_fun)
#now show the plot
plot(adjsurv.sals.bs, conf_int=TRUE, use_boot=TRUE)

