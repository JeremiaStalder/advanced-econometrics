library(ATE)
library (BNSP)
library(bbemkr)
library(tidyverse)
library(glmnet)
#lasso with glmnet with all vars
#choose vars, interpret vars (but biased)
#normal OLS compare with lasso 

setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

#Data prep
lassodata <- mydata_transform
X <- as.matrix(select(lassodata, c(variable_sets_modelling[["independent_vars_selection_std"]])))
Y <- as.matrix(lassodata$tw_adjust_std)


#Lasso CV
lasso.cv <- cv.glmnet(X, Y, type.measure = "mse", family = "gaussian", nfolds = 15, alpha = 1)
model <- glmnet(X, Y, alpha = 1, family = "gaussian",
                lambda = lasso.cv$lambda.min)

#Post lasso
variables <- coef_lasso1@Dimnames[[1]][which(coef_lasso1 != 0 ) ]
postlasso <- lm(as.formula(paste("tw_adjust_std ~ ", paste(variables[2:length(variables)], collapse= "+"))), data = lassodata)
summary(postlasso)

#ATE post lasso
plasso_ATE <- postlasso$coefficients[["e401_std"]]

#CATE
#split data for 5 quantiles use the variables as before
#run whole lasso again?

