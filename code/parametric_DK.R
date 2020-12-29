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

# balance check function 
balance_check <- function(vars, treat){
  
  md = rep(0,ncol(vars))
  md_se = rep(0,ncol(vars))
  diff = rep(0,ncol(vars))
  
  for (i in 1:ncol(vars)){
    md[i] = mean(vars[,i][treat==1])-mean(vars[,i][treat==0])
    md_se[i] = sqrt((var(vars[,i][treat==1])+var(vars[,i][treat==0]))/2)
    diff[i] = md[i] / md_se[i]
  }
  
  res = cbind(md,md_se,diff,abs(diff)>0.1)
  colnames(res) = c("mean_diff","estimated_sd","standardized_difference","relevant_difference")
  rownames(res) = colnames(vars)
  return(res)
}


outpath <- "./output/" # output
outpath_des_before_trans <- "./output/descriptives/before_trans/"
outpath_des_after_trans <- "./output/descriptives/after_trans/"
datapath <- "./data/" # data files (input datafile from package)

####  0) Import Data ####
mydata <- as.data.frame(pension)

#### 1) Data Overview (before transformations) ####
# define variable sets
colnames(mydata)
key_vars = c("p401", "e401", "a401", "tw")
financial_vars <-c("inc", "tfa", "tfa_he", "ira", "net_tfa", "nifa", "net_nifa", "net_n401")
other_controls_with_key_vars <- c( "age","fsize", "marr", "db", "hown", "educ", "male", "twoearn","hmort","hequity","hval")





#### 2) Data Transformation ####
mydata_transform <- mydata
mydata_transform$tw_original <- mydata_transform$tw

## Remove outlier observations and invalid values
# drop observations with negative income (only 5 obs, same as benjamin (2003))
mydata_transform <- filter(mydata_transform, inc > 0)

# drop observations with negative home equity (only 27 obs) to avoid overfitting to outliers.
mydata_transform <- filter(mydata_transform, hequity >= 0)

#### Total wealth variable Outliers- Important ####
# Issue: negative outliers in tw will have impact on log transformation, interpretability and lead to overfitting of outliers
# Heuristic idea: since all asset values (not net asset values) are 0 / positive: remove obs with total wealth < -50000 + house equity. 50000 could be e.g. consumer loans, tax payments,...
# Result: removed 25 observations
mydata_transform <- filter(mydata_transform, tw > -(-hequity + 50000))

#### Remove Regions without common support ####
# income: no common support for <5000USD income (only e401=0). Lose ~350 observations
mydata_transform <- filter(mydata_transform, inc >7000)


## New variables
# Withdrawal without penalty variable (above age 59.5)
mydata_transform$withdrawal <- as.numeric(mydata_transform$age > 59.5)

# other assets as residual variable, includes: other property, vehicles, ...
mydata_transform$other_assets <- mydata_transform$tw - mydata_transform$net_tfa - mydata_transform$hequity

## Interaction terms
# Family variable
mydata_transform$busy_couple <- as.numeric(mydata_transform$fsize == 2 & mydata_transform$twoearn == 1)
#### add more interaction terms! especially useful for lasso. ####


## Variable transformations
continuous_vars <- c("a401","tw", "tfa","ira","net_tfa","nifa","net_nifa","net_n401","inc","hmort","hequity","hval","other_assets","tw_original","educ","age") 
# keep age and educ as last elements. not truely continuous and does not need to be logged. (so exlude for some operations)


# family size dummies
mydata_transform <- mutate(mydata_transform, fsize = case_when(as.double(fsize) > 5 ~ 5, TRUE ~ as.double(fsize))) # transform >5 familiy size to 5
fsize_dummies <- fastDummies::dummy_cols(mydata_transform$fsize, remove_first_dummy = T)[, 2:5] # dummies for family size. remove fist column which is original variable
colnames(fsize_dummies) <- c(paste0("fsize_", c(2, 3, 4, "5_or_above")))
mydata_transform <- cbind(mydata_transform, fsize_dummies)

# Dummies for cutoffs of continuous vars
# Dummies for left cutoff (0)
# check which vars have left cutoff:
# result: hmort, hval
# BUT: with removed negative equity variables _> hown = hval_dummy
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform[, continuous_vars[i]] == min(mydata_transform[, continuous_vars[i]])))
}
mydata_transform <- mutate(mydata_transform, hmort_dummy = as.numeric(hmort > 0)) # mortgage dummy


# Dummies for right cutoff
# check which vars have right cutoff:
# result: hmort, hval, hequity
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform[, continuous_vars[i]] == max(mydata_transform[, continuous_vars[i]])))
}
mydata_transform <- mutate(mydata_transform, hval_dummy_right_censored = as.numeric(max(hval) == hval)) # house value dummy
mydata_transform <- mutate(mydata_transform, hmort_dummy_right_censored = as.numeric(max(hmort) == hmort)) # mortgage dummy
mydata_transform <- mutate(mydata_transform,hequity_dummy_right_censored = as.numeric(max(hequity) == hequity)) # hequity dummy

# Wealth variable adjustments due to db following Benjamin (2003) (defined benefit plans. their value is not included in wealth variables)
# 1. replacement of preexisting db-plan: subtract 20% of 401k assets from total wealth & financial asset variables that include 401k
mydata_transform$tfa_adjust <- mydata_transform$tfa - mydata_transform$a401 * 0.2
mydata_transform$net_tfa_adjust <- mydata_transform$net_tfa - mydata_transform$a401 * 0.2
mydata_transform$tw_adjust <- mydata_transform$tw - mydata_transform$a401 * 0.2

# 2. marginal substitution effect: firms could reduce benefits of DB-plan if they instroduce 401k.Subtract income-related correction term from total wealth & financial asset variables that include 401k
wealth_correction_per_dollar_income <- 0.032
mydata_transform$tfa_adjust[mydata_transform$db & mydata_transform$e401] <-  mydata_transform$tfa_adjust[mydata_transform$db & mydata_transform$e401] - mydata_transform$inc[mydata_transform$db & mydata_transform$e401] * wealth_correction_per_dollar_income
mydata_transform$net_tfa_adjust[mydata_transform$db & mydata_transform$e401] <- mydata_transform$net_tfa_adjust[mydata_transform$db & mydata_transform$e401] - mydata_transform$inc[mydata_transform$db & mydata_transform$e401] * wealth_correction_per_dollar_income
mydata_transform$tw_adjust[mydata_transform$db & mydata_transform$e401] <- mydata_transform$tw_adjust[mydata_transform$db & mydata_transform$e401] - mydata_transform$inc[mydata_transform$db & mydata_transform$e401] * wealth_correction_per_dollar_income

continuous_vars <- c(paste0(c("tfa", "net_tfa", "tw"), "_adjust"), continuous_vars) # add variables to continuous. Add in front (because last variables are excluded for log operation)


# Log monetary variables
# Note: if variable can be negative, we transform it to be positive ("move the minimum value") such that we can use logs. We move the minimum of the series to 1.
# !Attention: for variables with negative minimum (only home equity), the interpretability of the coefficient is killed.)
for (i in 1:(length(continuous_vars) - 3)) {
  # exclude tw_original, educ and age variable in this loop
  if (min(mydata_transform[, continuous_vars[i]]) <= 1) {
    mydata_transform[, continuous_vars[i]] <-
      log(mydata_transform[, continuous_vars[i]] - min(mydata_transform[, continuous_vars[i]]) +
            1)
  }
  else {
    mydata_transform[, continuous_vars[i]] <-
      log(mydata_transform[, continuous_vars[i]])
  }
}


# polynomials: (var-mean(var))^n
# age
mydata_transform$age_sq <- (mydata_transform$age - mean(mydata_transform$age)) ^ 2
mydata_transform$age_cub <- (mydata_transform$age - mean(mydata_transform$age)) ^ 3

# income
mydata_transform$inc_sq <- (mydata_transform$inc - mean(mydata_transform$inc)) ^ 2
mydata_transform$inc_cub <- (mydata_transform$inc - mean(mydata_transform$inc)) ^ 3

# wealth outcome variables in quantiles (doesnt matter if before or after standardizing)
for (i in 1:(length(continuous_vars) - 3)) {
  mydata_transform <-
    cbind(mydata_transform,
          ecdf(mydata_transform[, continuous_vars[i]])(mydata_transform[, continuous_vars[i]])) # function for empirical quantile
  colnames(mydata_transform)[ncol(mydata_transform)] <-
    paste0(continuous_vars[i], "_quantile")
}

## Drop Variables
mydata_transform <-select(mydata_transform, -c(zhat, dum91, icat, ecat, tfa_he))    # remove unnecessary variables
mydata_transform <- select(mydata_transform, -c(nohs, a1, i1))  # remove first age category, income category and no high school dummy as reference category


## Standardize variables.
# ! Hint: need to do all "content-related" variable transformations before this step.
# ! Hint 2: for lasso etc, also binary variables need to be standardized. thats why we do it here
standardized_vars <- scale(mydata_transform)
colnames(standardized_vars) <- paste0(colnames(mydata_transform), "_std")
mydata_transform <- cbind(mydata_transform, standardized_vars)

## Save transformed data
save(mydata_transform, file = paste0(outpath, "mydata_transform.Rdata")) 


#### 3) Define Variable Sets ####
# dependent variables
dependent_vars <- c("tw_adjust", "net_tfa_adjust","net_nifa","tw","tw_original", "tfa","ira","net_tfa", "nifa","net_n401","tfa_adjust")
dependent_vars_std <- paste0(dependent_vars, "_std")
dependent_vars_quantile <- paste0(dependent_vars, "_quantile")
dependent_vars_selection <- c("tw_adjust", "net_tfa_adjust", "net_nifa", "tw_original")
dependent_vars_selection_std <- paste0(dependent_vars_selection, "_std")
dependent_vars_selection_quantile <- paste0(dependent_vars_selection[dependent_vars_selection != "tw_original"], "_quantile")

# independent variables
non_used_wealth_variables <- c("a401", "hval", "hmort", "hequity")
independent_vars_temp <- colnames(mydata_transform)[!grepl("_std", colnames(mydata_transform), TRUE)]
independent_vars <- independent_vars_temp[!(independent_vars_temp %in% dependent_vars) & !(independent_vars_temp %in% non_used_wealth_variables)]
independent_vars_std <-  paste0(independent_vars, "_std")

independent_vars_selection <-c("e401","p401","age", "inc", "db","marr","male","twoearn", "pira","hs","smcol", "col","hown", "fsize_2", "fsize_3","fsize_4", "fsize_5_or_above",
    "withdrawal","busy_couple", "hmort_dummy","hval_dummy_right_censored","hmort_dummy_right_censored", "hequity_dummy_right_censored", "age_sq",
    "age_cub","inc_sq" , "inc_cub")
independent_vars_selection_std = paste0(independent_vars_selection, "_std")

dependent_vars_benjamin <- c("tw", "tw_original", "tfa", "net_nifa") # do not know if benjamin used logs
dependent_vars_benjamin_std <- paste0(dependent_vars_benjamin, "_std")
independent_vars_benjamin <- c("e401", "p401","fsize","ira", "pira","hown","hs", "smcol","col", "marr","twoearn","db",
    paste0("a", seq(from = 2, to = 5, by = 1)),
    paste0("i", seq(from = 2, to = 7, by = 1)) ) # do not know if benjamin used logs
independent_vars_benjamin_std <- paste0(independent_vars_benjamin, "_std")

# collect sets and save as list
variable_sets <-list( dependent_vars,dependent_vars_std, dependent_vars_selection,dependent_vars_selection_std,non_used_wealth_variables,
    independent_vars, independent_vars_std, independent_vars_selection,independent_vars_selection_std,dependent_vars_benjamin,
    dependent_vars_benjamin_std, independent_vars_benjamin, independent_vars_benjamin_std)
names(variable_sets) <- c("dependent_vars", "dependent_vars_std","dependent_vars_selection","dependent_vars_selection_std",
    "non_used_wealth_variables", "independent_vars", "independent_vars_std","independent_vars_selection", "independent_vars_selection_std",
    "dependent_vars_benjamin", "dependent_vars_benjamin_std","independent_vars_benjamin", "independent_vars_benjamin_std")

## save dataset
save(variable_sets, file = paste0(outpath, "variable_sets.Rdata")) # save list


#---------------------------------------------------------------------------------------------------------------------

attach(mydata_transform)
x <- cbind(age,inc,db,marr,male,twoearn,pira,hs,smcol,col,hown,fsize_2,fsize_3,fsize_4,fsize_5_or_above
           ,busy_couple,hmort_dummy,hval_dummy_right_censored,hmort_dummy_right_censored,hequity_dummy_right_censored
           ,age_sq,age_cub,inc_sq,inc_cub)
d <- as.matrix(e401)
colnames(d)[1] <- "d"
y <- as.matrix(exp(tw))

# create subsample for CATE estimation
mydata_transform_CATE1 <- filter(mydata_transform, inc<=quantile(inc, 0.2))
mydata_transform_CATE2 <- filter(mydata_transform, inc<=quantile(inc, 0.4) & inc>quantile(inc, 0.2))
mydata_transform_CATE3 <- filter(mydata_transform, inc<=quantile(inc, 0.6) & inc>quantile(inc, 0.4))
mydata_transform_CATE4 <- filter(mydata_transform, inc<=quantile(inc, 0.8) & inc>quantile(inc, 0.6))
mydata_transform_CATE5 <- filter(mydata_transform, inc<=quantile(inc, 1.0) & inc>quantile(inc, 0.8))

# Mean comparison ----------------------------------------------------------------------------------------------------

# for comparison
est_mc <- lm(y~d)
summary(est_mc)

# computes the mean-difference estimator
MC_ate_fn <- function(data, index){
  y <- data$tw[index]
  y <- exp(y)
  d <- data$e401[index]
  out <- mean(y[d==1]) - mean(y[d==0])
  return(out)
}

# ATE
ate_md = MC_ate_fn(mydata_transform, 1:nrow(mydata_transform))
# CATE
cate1_md = MC_ate_fn(mydata_transform_CATE1, 1:nrow(mydata_transform_CATE1))
cate2_md = MC_ate_fn(mydata_transform_CATE2, 1:nrow(mydata_transform_CATE2))
cate3_md = MC_ate_fn(mydata_transform_CATE3, 1:nrow(mydata_transform_CATE3))
cate4_md = MC_ate_fn(mydata_transform_CATE4, 1:nrow(mydata_transform_CATE4))
cate5_md = MC_ate_fn(mydata_transform_CATE5, 1:nrow(mydata_transform_CATE5))
cate_md = rbind(cate1_md, cate2_md, cate3_md, cate4_md, cate5_md)
par(mar=cbind(2,2,2,2))
plot(cate_md)

# Bootstraping for the estimate and model accuracy
# ATE
boot(mydata_transform, MC_ate_fn, R=1000)
# CATE
boot(mydata_transform_CATE1, MC_ate_fn, R=1000)
boot(mydata_transform_CATE2, MC_ate_fn, R=1000)
boot(mydata_transform_CATE3, MC_ate_fn, R=1000)
boot(mydata_transform_CATE4, MC_ate_fn, R=1000)
boot(mydata_transform_CATE5, MC_ate_fn, R=1000)


# Parametric model ---------------------------------------------------------------------------------------------------


## DOUBLY ROBUST ESTIMATOR

# FN: Computes the doubly robust estimator
DR_ate_fn <- function(data, index, minp, maxp){
  
  # prepare data vectors
  d   = data$e401[index]
  xfn = cbind(age[index],inc[index],db[index],marr[index],male[index], twoearn[index],pira[index],hs[index],smcol[index],col[index],hown[index],fsize_2[index],fsize_3[index],fsize_4[index],fsize_5_or_above[index]
              ,busy_couple[index],hmort_dummy[index],hval_dummy_right_censored[index],hmort_dummy_right_censored[index],hequity_dummy_right_censored[index]
              ,age_sq[index],age_cub[index],inc_sq[index],inc_cub[index])
  y   = data$tw[index]
  y   = exp(y)
  
  # compute mu by OLS and propensity scores by logit
  ols <- lm(y~d+xfn)
  mu1 = ols$fitted.values - ols$coefficients[2]*d + ols$coefficients[2]
  mu0 = ols$fitted.values - ols$coefficients[2]*d 
  logit <- glm(d~xfn, family = "binomial")
  yhat_logit <- as.matrix(logit$fitted.values)
  
  # cut sample when propensity score too close to 0 or 1
  help <- as.data.frame(cbind(y, yhat_logit, mu1, mu0, d, xfn))
  help <- help[help$V2 < maxp & help$V2 > minp, ]
  y          <- as.matrix(help[,1])
  yhat_logit <- as.matrix(help[,2])
  mu1        <- as.matrix(help[,3])
  mu0        <- as.matrix(help[,4])
  d          <- as.matrix(help[,5])
  xfn        <- help[,6:29]
  
  # compuate the doubly robust ate
  ate <- mean( mu1 - mu0 + (d*(y-mu1)/yhat_logit) - ((1-d)*(y-mu0)/(1-yhat_logit)))
  return(ate)
}

# Average treatment effect (ATE)
ate_dr_l = DR_ate_fn(mydata_transform, 1:nrow(mydata_transform), 0.05, 0.95)
ate_dr_m = DR_ate_fn(mydata_transform, 1:nrow(mydata_transform), 0.075, 0.925)
ate_dr_u = DR_ate_fn(mydata_transform, 1:nrow(mydata_transform), 0.1, 0.9)
ate_dr   = rbind(ate_dr_l, ate_dr_m, ate_dr_u)

# Conditional average treatment effect (CATE)
cate1_dr = DR_ate_fn(mydata_transform_CATE1, 1:nrow(mydata_transform_CATE1),0.075, 0.925)
cate2_dr = DR_ate_fn(mydata_transform_CATE2, 1:nrow(mydata_transform_CATE2),0.075, 0.925)
cate3_dr = DR_ate_fn(mydata_transform_CATE3, 1:nrow(mydata_transform_CATE3),0.075, 0.925)
cate4_dr = DR_ate_fn(mydata_transform_CATE4, 1:nrow(mydata_transform_CATE4),0.075, 0.925)
cate5_dr = DR_ate_fn(mydata_transform_CATE5, 1:nrow(mydata_transform_CATE5),0.075, 0.925)
cate_dr  = rbind(cate1_dr, cate2_dr, cate3_dr, cate4_dr, cate5_dr)


# Bootstraping for the estimate and model accuracy
# R will be increased for actual results (computation takes some time)
boot(mydata_transform, DR_ate_fn, R=100)
boot(mydata_transform_CATE1, DR_ate_fn, R=100)
boot(mydata_transform_CATE2, DR_ate_fn, R=100)
boot(mydata_transform_CATE3, DR_ate_fn, R=100)
boot(mydata_transform_CATE4, DR_ate_fn, R=100)
boot(mydata_transform_CATE5, DR_ate_fn, R=100)

boot_DR = boot(mydata_transform, DR_ate_fn, R=100)

# Confidence Interval
par(mar=(c(2,2,1,1)))
hist(boot_DR[["t"]], breaks = 30)
CI(boot_DR[["t"]], ci=0.95)



# Comparison ---------------------------------------------------------------------------------------------------

plot(cate_dr, xlim=c(1,5), ylim=c(0,18000), col="steelblue", pch=8)
lines(lowess(cate_dr), col="steelblue")
par(new=TRUE)
plot(cate_md, xlim=c(1,5), ylim=c(0,18000), col="tomato", pch=16)
lines(lowess(cate_md), col="tomato")





