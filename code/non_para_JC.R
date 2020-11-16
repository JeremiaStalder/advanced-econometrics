library(ATE)
library (BNSP)

setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

#####Non-Parametric####
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
