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

lassodata <- mydata_transform
X <- as.matrix(select(lassodata, c(variable_sets_modelling[["independent_vars_benjamin_std"]])))
Y <- as.matrix(lassodata$tw_adjust_std)

lasso.cv <- cv.glmnet(X, Y, type.measure = "mse", family = "gaussian", nfolds = 15, alpha = 1)

lambdas <- cbind(c(lasso.cv$lambda),as.numeric(c(lasso.cv$nzero)))
rownames(lambdas) <- lambdas[,2]
lambdas <- as.data.frame(t(lambdas[,-2]))
lambda2 <- lambdas$`2`
lambda2



coef_lasso1 <- coef(lasso.cv, s = lambda2) # save for later comparison
print(coef_lasso1)
coef_lasso1[which(coef_lasso1 != 0 ) ]

coef_lasso1@Dimnames[[1]][which(coef_lasso1 != 0 ) ]

postlasso <- lm(tw_adjust_std ~ ira_std + hown_std + e401_std, data = lassodata)
summary(postlasso)
