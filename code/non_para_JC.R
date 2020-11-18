library(ATE)
library (BNSP)
library(bbemkr)
library(tidyverse)

setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

#####Non-Parametric####


#PCA of confounders everything apart of D and outcome
  #two PCA for D=1 and D=0, number of variables 1-26 
#kernel estimator of the joint fY,X(y,x) and fX(x) for both D=1 and D=0
  # lecture 3 eq.32 
  #package npudens (code on canvas) estimate the K then use equation 32
  #estimate h by rule of thumb and CV and compare
#difference of both cond. means





#############################################################
npdata <- mydata_transform
Y <- npdata$tw #numeric vector with outcome variable
Ti <- npdata$e401 #Treatment 
indep_vars <- variable_sets_modelling[["independent_vars_selection"]]
squared_vars <- c("age_sq","age_cub","inc_sq","inc_cub")

X <- select(npdata, all_of(indep_vars))
X <- select(X, -c("e401"))#no treatment
X <- select(X, -squared_vars)#dont need squared variables

#get the initial values why a A numeric vector or matrix of possible initial values for the Newton-Raphson
#algorithm. Must be a J X K matrix where J is the number of treatment arms.

#need starting values!!!!!!!!!!!!!!!
#reduce diminsions!!!!

npATE <- ATE (Y, Ti, X, theta = 0, ATT = FALSE,
     verbose = FALSE, max.iter = 100, tol = 1e-10,
     initial.values = NULL,
     backtrack = TRUE, backtrack.alpha = 0.3,
     backtrack.beta = 0.5)

#plot(npATE)
summary(npATE)


####Semi-Parametric####

#what in the  para part? what in the non-para part

#define parametric variables

#define non parametric variables
