### Project Pension - Advanced Econometrics Methods, HSG

# librarys
library(readr)
library(zoo)
library(stats)
library(TSPred)
library(forecast)
library(lubridate)
library(data.table)
library(tidyverse)
library(hdm)
library(GGally)
library(ggcorrplot)
library(xtable)
library(fastDummies)
library(boot)
library(Rmisc)

rm(list = ls())
setwd("~/Documents/HSG/MiQEF/3.Semester/AdEconometricMethods/Assignment")

## functions
filter <- dplyr::filter
select <- dplyr::select
ggpairs <- GGally::ggpairs
ggcorrplot <- ggcorrplot::ggcorrplot



####  0) Import Data ####
load("./output/mydata_transform.Rdata")



#---------------------------------------------------------------------------------------------------------------------

attach(mydata_transform)
x <- cbind(age,inc,db,marr,male,twoearn,pira,hs,smcol,col,hown,withdrawal,age_sq,age_cub,inc_sq,inc_cub)
d <- as.matrix(e401)
colnames(d)[1] <- "d"
y <- as.matrix(tw_adjust_original)

# create subsample for CATE estimation
mydata_transform_CATE1 <- filter(mydata_transform, inc<=quantile(inc, 0.2))
mydata_transform_CATE2 <- filter(mydata_transform, inc<=quantile(inc, 0.4) & inc>quantile(inc, 0.2))
mydata_transform_CATE3 <- filter(mydata_transform, inc<=quantile(inc, 0.6) & inc>quantile(inc, 0.4))
mydata_transform_CATE4 <- filter(mydata_transform, inc<=quantile(inc, 0.8) & inc>quantile(inc, 0.6))
mydata_transform_CATE5 <- filter(mydata_transform, inc<=quantile(inc, 1.0) & inc>quantile(inc, 0.8))


# Function for confidence interval
# E:I changed - we want 5% CI, so 0.025 & 0.975
CI <- function(bootsample){
  CI = cbind(quantile(bootsample$t, 0.025), quantile(bootsample$t, 0.975))
  colnames(CI) = cbind("2.5%", "97.5%")
  rownames(CI) = "CI"
  return(CI)
}
  

  
# OLS ----------------------------------------------------------------------------------------------------

ate_ols = lm(y~d+x)
summary(ate_ols)


# Mean comparison ----------------------------------------------------------------------------------------------------

ols_yd <- lm(y~d)
summary(ols_yd)

# computes the mean-difference estimator
MC_ate_fn <- function(data, index){
  y <- data$tw_adjust_original[index]
  d <- data$e401[index]
  out <- mean(y[d==1]) - mean(y[d==0])
  return(out)
}

# ATE
ate_md = MC_ate_fn(mydata_transform, 1:nrow(mydata_transform))
ate_md

# CATE
cate1_md = MC_ate_fn(mydata_transform_CATE1, 1:nrow(mydata_transform_CATE1))
cate2_md = MC_ate_fn(mydata_transform_CATE2, 1:nrow(mydata_transform_CATE2))
cate3_md = MC_ate_fn(mydata_transform_CATE3, 1:nrow(mydata_transform_CATE3))
cate4_md = MC_ate_fn(mydata_transform_CATE4, 1:nrow(mydata_transform_CATE4))
cate5_md = MC_ate_fn(mydata_transform_CATE5, 1:nrow(mydata_transform_CATE5))


# Bootstraping for the estimate and model accuracy: ATE
#E: difference - D used bootstrap for simple means, i did not
boot_ate_md = boot(mydata_transform, MC_ate_fn, R=1000)

CI_ate_md = CI(boot_ate_md)
std_ate_md = sd(boot_ate_md$t)


# Bootstraping for the estimate and model accuracy: CATE

boot_cate1_md = boot(mydata_transform_CATE1, MC_ate_fn, R=1000)
boot_cate2_md = boot(mydata_transform_CATE2, MC_ate_fn, R=1000)
boot_cate3_md = boot(mydata_transform_CATE3, MC_ate_fn, R=1000)
boot_cate4_md = boot(mydata_transform_CATE4, MC_ate_fn, R=1000)
boot_cate5_md = boot(mydata_transform_CATE5, MC_ate_fn, R=1000)

CI_cate1_md = CI(boot_cate1_md)
CI_cate2_md = CI(boot_cate2_md)
CI_cate3_md = CI(boot_cate3_md)
CI_cate4_md = CI(boot_cate4_md)
CI_cate5_md = CI(boot_cate5_md)
std_cate1_md = sd(boot_cate1_md$t)
std_cate2_md = sd(boot_cate2_md$t)
std_cate3_md = sd(boot_cate3_md$t)
std_cate4_md = sd(boot_cate4_md$t)
std_cate5_md = sd(boot_cate5_md$t)


# Combine results

res_md_ate = rbind(ate_md, CI_ate_md[1], CI_ate_md[2], std_ate_md)
res_md_cate1 = rbind(cate1_md, CI_cate1_md[1], CI_cate1_md[2], std_cate1_md)
res_md_cate2 = rbind(cate2_md, CI_cate2_md[1], CI_cate2_md[2], std_cate2_md)
res_md_cate3 = rbind(cate3_md, CI_cate3_md[1], CI_cate3_md[2], std_cate3_md)
res_md_cate4 = rbind(cate4_md, CI_cate4_md[1], CI_cate4_md[2], std_cate4_md)
res_md_cate5 = rbind(cate5_md, CI_cate5_md[1], CI_cate5_md[2], std_cate5_md)
res_md = cbind(res_md_ate, res_md_cate1, res_md_cate2, res_md_cate3, res_md_cate4, res_md_cate5)
rownames(res_md) = cbind("effect", "lower CI", "upper CI", "std-error")
colnames(res_md) =  cbind("ate_md", "cate1_md", "cate2_md", "cate3_md", "cate4_md", "cate5_md")
res_md



# Parametric model ---------------------------------------------------------------------------------------------------


## DOUBLY ROBUST ESTIMATOR

# FN: Computes the doubly robust estimator
DR_ate_fn <- function(data, index){
  
  # prepare data vectors
  # E: edit: add $data in front, otherwise it uses the full datasets
  d   = data$e401[index]
  xfn = cbind(data$age[index],data$inc[index],data$db[index],data$marr[index],data$male[index], data$twoearn[index],data$pira[index],
              data$hs[index],data$smcol[index],data$col[index],data$hown[index],data$withdrawal[index],data$age_sq[index],
              data$age_cub[index],data$inc_sq[index],data$inc_cub[index])
  y   = data$tw_adjust_original[index]
  
  # compute mu by OLS and propensity scores by logit
  ols <- lm(y~d+xfn)
  
  mu1 = ols$fitted.values + (1-d)*ols$coefficients[2]
  mu0 = ols$fitted.values - ols$coefficients[2]*d 
  
  probit <- glm(d~xfn, family = "binomial"(link = "probit"))
  yhat_probit <- as.matrix(probit$fitted.values)
  
  # Add when cut the sample because of \hat{p}
  #help <- as.data.frame(cbind(y, yhat_probit, mu1, mu0, d, xfn))
  # help <- help[help$V2 < 0.95 & help$V2 > 0.05, ]
  #y          <- as.matrix(help[,1])
  #yhat_logit <- as.matrix(help[,2])
  #mu1        <- as.matrix(help[,3])
  #mu0        <- as.matrix(help[,4])
  #d          <- as.matrix(help[,5])
  #xfn        <- help[,6:21]
  
  # compuate the doubly robust ate
  diff_yhat = mu1 - mu0
  frac_1    = d*(y-mu1)/yhat_probit
  frac_2    = (1-d)*(y-mu0)/(1-yhat_probit)
  ate       = 1/length(y)*sum(diff_yhat + frac_1 - frac_2)
  
  return(ate)
}

# Average treatment effect (ATE)
ate_dr = DR_ate_fn(mydata_transform, 1:nrow(mydata_transform))

# Conditional average treatment effect (CATE)
cate1_dr = DR_ate_fn(mydata_transform_CATE1, 1:nrow(mydata_transform_CATE1))
cate2_dr = DR_ate_fn(mydata_transform_CATE2, 1:nrow(mydata_transform_CATE2))
cate3_dr = DR_ate_fn(mydata_transform_CATE3, 1:nrow(mydata_transform_CATE3))
cate4_dr = DR_ate_fn(mydata_transform_CATE4, 1:nrow(mydata_transform_CATE4))
cate5_dr = DR_ate_fn(mydata_transform_CATE5, 1:nrow(mydata_transform_CATE5))


# Bootstraping for the estimate and model accuracy: ATE

boot_ate_dr = boot(mydata_transform, DR_ate_fn, R=1000)

CI_ate_dr = CI(boot_ate_dr)
std_ate_dr = sd(boot_ate_dr$t)


# Bootstraping for the estimate and model accuracy: CATE

boot_cate1_dr = boot(mydata_transform_CATE1, DR_ate_fn, R=1000)
boot_cate2_dr = boot(mydata_transform_CATE2, DR_ate_fn, R=1000)
boot_cate3_dr = boot(mydata_transform_CATE3, DR_ate_fn, R=1000)
boot_cate4_dr = boot(mydata_transform_CATE4, DR_ate_fn, R=1000)
boot_cate5_dr = boot(mydata_transform_CATE5, DR_ate_fn, R=1000)

CI_cate1_dr = CI(boot_cate1_dr)
CI_cate2_dr = CI(boot_cate2_dr)
CI_cate3_dr = CI(boot_cate3_dr)
CI_cate4_dr = CI(boot_cate4_dr)
CI_cate5_dr = CI(boot_cate5_dr)
std_cate1_dr = sd(boot_cate1_dr$t)
std_cate2_dr = sd(boot_cate2_dr$t)
std_cate3_dr = sd(boot_cate3_dr$t)
std_cate4_dr = sd(boot_cate4_dr$t)
std_cate5_dr = sd(boot_cate5_dr$t)



# Combine results

res_dr_ate = rbind(ate_dr, CI_ate_dr[1], CI_ate_dr[2], std_ate_dr)
res_dr_cate1 = rbind(cate1_dr, CI_cate1_dr[1], CI_cate1_dr[2], std_cate1_dr)
res_dr_cate2 = rbind(cate2_dr, CI_cate2_dr[1], CI_cate2_dr[2], std_cate2_dr)
res_dr_cate3 = rbind(cate3_dr, CI_cate3_dr[1], CI_cate3_dr[2], std_cate3_dr)
res_dr_cate4 = rbind(cate4_dr, CI_cate4_dr[1], CI_cate4_dr[2], std_cate4_dr)
res_dr_cate5 = rbind(cate5_dr, CI_cate5_dr[1], CI_cate5_dr[2], std_cate5_dr)
res_dr = cbind(res_dr_ate, res_dr_cate1, res_dr_cate2, res_dr_cate3, res_dr_cate4, res_dr_cate5)
rownames(res_dr) = cbind("effect", "lower CI", "upper CI", "std-error")
colnames(res_dr) =  cbind("ate_dr", "cate1_dr", "cate2_dr", "cate3_dr", "cate4_dr", "cate5_dr")
res_dr









