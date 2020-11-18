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


data_ker <- as.data.frame(select(mydata_transform, c(variable_sets_modelling[["independent_vars_benjamin_std"]])))
data_ker0 <- data_ker[data_ker[,"e401_std"] < 0,]
data_ker1 <- data_ker[data_ker[,"e401_std"] > 0,]
Y <- as.data.frame(select(mydata_transform, c("tw_adjust_std")))
#####Kernel Estimator####

#rule of thumb
X= Varibale
sigma <- sd(X)
N <- nrow(data)
h.rot <- 1.06*Sigma*N^{-1/5}
dens1=npudens(bws=h.rot, X)
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

PCA0 <- PCA_function(data_ker0)
PCA1 <- PCA_function(data_ker1)

#kernel estimator of the joint fY,X(y,x) and fX(x) for both D=1 and D=0
# lecture 3 eq.32 
#package npudens (code on canvas) estimate the K then use equation 32
#estimate h by rule of thumb and CV and compare
#difference of both cond. means


kernel <- function(X){
h.cv=npudensbw(X, bwmethod="cv.ls")	# least squares cross validation. 
# Need first to calculate the bandwidth 
dens4=npudens(bws=h.cv)			# Now calculate the density
#plot(dens4, main ="Cross validation")
kernelvalues <- dens4[["dens"]]
return(kernelvalues)
}

kernelvalues1 <- kernel(PCA1[[3]])
kernelvalues0 <- kernel(PCA0[[3]])

m_function <- function(dens, y){
  mx <- sum(y*dens)/sum(dens)
  return(mx)
}

m1 <- m_function(kernelvalues1,Y)
m0 <- m_function(kernelvalues0,Y)

ATE <- m1-m0
ATE

