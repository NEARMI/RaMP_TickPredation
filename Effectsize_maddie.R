#Code for RaMP power analysis
#Created by Elise Edwards
#Last Updated: 1/29/2025 

library(pwr)


# calculate minimal sample size
pwr.anova.test(k=5,            # 5 groups are compared
               f=.08,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               power=.8)  


#figure out what effect size should be for more leaf litter/ more fruit flies

mu_x <- 0.5790     ### Average of more leaf litter based on pilot data
mu_y <- 0.5335      ### Average of more flies based on pilot data

sd_x <- 0.0726242      ### Standard deviation  more leaf litter based on pilot data
sd_y <- 0.0565663     ### Standard deviation of more flies based on pilot data

rho <- 0.5      ### Correlation between measures before and after the treatment

sd_z <- sqrt(sd_x^2 + sd_y^2 - 2*rho*sd_x*sd_y)

d_z <- abs(mu_x - mu_y) / sd_z
d_z



#figure out what effect size should be for complex vs simple


mu_x <- 0.5538     ### Average of complex based on pilot data
mu_y <- 0.5588      ### Average of simple based on pilot data

sd_x <- 0.0686626      ### Standard deviation complex based on pilot data
sd_y <- 0.0649919     ### Standard deviation simple based on pilot data

rho <- 0.5      ### Correlation between measures before and after the treatment

sd_z <- sqrt(sd_x^2 + sd_y^2 - 2*rho*sd_x*sd_y)

#calculate Cohen's d
d_z <- abs(mu_x - mu_y) / sd_z
d_z
