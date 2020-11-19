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
load("./output/variable_sets_modelling.Rdata")

#Data prep
lassodata <- mydata_transform
Xvars <- c(variable_sets_modelling[["independent_vars_std"]][1:(length(variable_sets_modelling[["independent_vars_std"]])-2)])
#X <- as.matrix(select(lassodata, c(variable_sets_modelling[["independent_vars_std"]])))
Y <- as.matrix(lassodata$tw_adjust_std)
X <- as.matrix(select(lassodata, all_of(Xvars)))

#Lasso CV
lasso.cv <- cv.glmnet(X, Y, type.measure = "mse", family = "gaussian", nfolds = 15, alpha = 1)
model <- glmnet(X, Y, alpha = 1, family = "gaussian",
                lambda = lasso.cv$lambda.min)
coef_lasso1 <- coef(lasso.cv, s = lasso.cv$lambda.min)
#Post lasso
variables <- coef_lasso1@Dimnames[[1]][which(coef_lasso1 != 0 ) ]
postlasso <- lm(as.formula(paste("tw_adjust_std ~ ", paste(variables[2:length(variables)], collapse= "+"))), data = lassodata)
summary(postlasso)

#ATE post lasso
plasso_ATE <- postlasso$coefficients[["e401_std"]]
plasso_SE <- coef(summary(postlasso))["e401","Std. Error"]




#CATE
#split data for 5 quantiles use the variables as before
#run whole lasso again?

postlasso <- function(Y,X){#input in matrix Y= outcome, X covariates including D,
  lasso.cv <- cv.glmnet(X, Y, type.measure = "mse", family = "gaussian", nfolds = 15, alpha = 1)
  model <- glmnet(X, Y, alpha = 1, family = "gaussian",
                  lambda = lasso.cv$lambda.min)
  variables <- coef_lasso1@Dimnames[[1]][which(coef_lasso1 != 0 ) ]
  postlasso <- lm(as.formula(paste("tw_adjust_std ~ ", paste(variables[2:length(variables)], collapse= "+"))), data = lassodata)
  
  plasso_ATE <- postlasso$coefficients[["e401"]]
  plasso_SE <- plasso_SE <- coef(summary(postlasso))["e401","Std. Error"]
  
  TE <- c(plasso_ATE, plasso_SE)
  output <- list(TE,variables)
  return(output)
}

#####Function Lasso####
lasso <- function(Y,X,C){#Y = outcome, X=covariates inculding D, C= conditional varibale
  output_matrix <- matrix(NA, 6, 2)
  names_list <- list(1,2,3,4,5)
  X_vars <- colnames(X)
  Y_vars <- colnames(Y)
  #Combine the three dataframes
  dat <- as.data.frame(cbind(Y,X,C))
  colnames(dat)[ncol(dat)] <- "quantile"
  
  
  for (n in 1:5) {
  #dat_n <- dat[dat[,"quantile"] == n]#split into quantile datasets
  dat_n <- dat[dat[,"inc_quintile"] < (n/5)]
  dat_n <- dat_n[dat_n[,"quantile"] > (n/5-1/5)]
  #seperate datasets
  Y_cate <- as.matrix(select(dat_n, Y_vars))
  X_cate <- as.matrix(select(dat_n, X_vars))
  p
  output <- postlasso(Y_cate,X_cate)
  output_matrix[n+1,] <- output[[1]]
  names_list[[n]] <- output[[2]]
  
  }
  
  #ATE
  output_matrix[1,] <- postlasso(as.matrix(Y),as.matrix(X))
  
  
  output_list <- list(names_list,output_matrix)
  return(output_list)
}



#####################TEST#####################
C <- as.matrix(lassodata$inc_quantile)
lasso_outputtest <- lasso(X,Y,C)