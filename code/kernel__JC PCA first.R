library(ATE)
library (BNSP)
library(bbemkr)
library(tidyverse)
library(factoextra)
library(np)	

setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")


data_ker <- as.data.frame(select(mydata_transform, c(variable_sets_modelling[["independent_vars_selection_std"]])))
Y <- as.data.frame(select(mydata_transform, c("tw_adjust_std")))
D <- as.data.frame(select(mydata_transform, c("e401_std")))
#####Kernel Estimator####


#PCA of confounders everything apart of D and outcome
#two PCA for D=1 and D=0, number of variables 1-26
# PCA
PCA_function <- function(f){
  mean = as.vector(colMeans(f))
  m    = matrix(mean, nrow(f), NROW(mean), byrow = T)
  x    = f - m
  eig  = eigen(cov(x))  # spectral decomposition  
  eva  = eig$values
  eve  = eig$vectors
  xm   = as.matrix(x)
  y    = xm %*% eve
  ym   = y[, 1:2] 
  output <- list(xm,y,ym)
  return(output)
}

PCA <- PCA_function(data_ker)
PCA_data <- PCA[[3]]
##################################
non_para_data <- cbind(Y,D,PCA_data)
colnames(non_para_data)[3:4] <- c("dim1","dim2")


model.pl_nonpara <- npplreg(tw_adjust_std ~ e401_std
                    | dim1 + dim2,
                    data = non_para_data)
summary(model.pl_nonpara)
