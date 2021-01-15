#######################################################################################################################
# Project - Advanced Econometric Methods
# Erik Senn, Johannes Cordier, Mila Gorkun-Voevoda, Davia Kündig and Jeremia Stalder
#
# Description:
# This file is meant as a guide through the whole project and helps to understand the structure of our code-files. 
# It will automatically execute the different scripts for data cleaning, estimation and result display, and produce all outputs automatically.
# Each file run here, can also be run independently. All code files are stored in the "code" folder.
# Note that some methods might take really long to run, that is why we indicated approximate runtimes. 

# ------------------------- Libraries ---------------------------------
# List of all required libraries, please make sure that they are all installed.

library(utils) # To get working directory of current file
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
library(rlist)
library(mgcv)
library(ggplot2)
library(grf)
library(dplyr)
library(np)
library(ATE)
library(bbemkr)
library(glmnet)
library(factoextra)
library(BNSP)
library(misc3d)

# Note: most libraries are specified again in the external files, such that each file can run independently. 
# ---------------------------------------------------------------------

# First, set the working directory to the location of the current file. The folders output and code should be in the same folder as this file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

# ------------ Data cleanup -------------
# We start with some descriptive statistics
# Estimated computation time: 2-5 minutes
source("./code/data_cleaning.R")

# ------------ Parametric -------------
# Now, we go on to estimate our parametric ATEs and CATEs.
# The following methods are used: Simple mean comparison, conditional outcome regression, flexible conditional outcome regression, generalized additive model, inverse probability weighting (ipw), restricted ipw, restricted ipw 2,
# doubly robust regression, restricted doubly robust regression and restricted doubly robust regression 2
# Estimated computation time: 5-10 minutes (the bootstraps take some time and might in rare cases cause problems with RAM-usage)
source("./code/parametric.R")
# Note: see "./code/parametric_functions.R" for additional functions used in "parametric.R".

# ------------ Lasso -------------
# Lasso
# Estimated computation time: around 10 minutes
source("./code/lasso_final.R")

# Double Selection Lasso
# Estimated computation time: around 3 minutes
source("./code/double-selection-lasso.R")

# ------------ Semi-Parametric -------------
# Semi-Parametric estimation
# For computation time reason we calculate the Semi-Parametric Estimation only with one non-parametric parameter.
# However, in "semi-parametric_JC.R" you can set it to two or three non-parametric parameters (option at the top of the file, might take up to 36h)
# Estimated computation time: around one hour
source("./code/semi-parametric_JC.R")

# ------------ Causal Random Forest -------------
# Causal Random Forest estimation
# Estimated computation time: 1-2 minutes
source("./code/causal_random_forest.R")

# ------------ Non-Parametric -------------
# Non-parametric Kernel Regression
# Estimated computation time: around 12 hours
source("./code/non-parametric__JC.R")

# Nadaraya-Watson Estimator
# Estimated computation time: 10 seconds with loaded pre-estimated bandwidths (default)
#                             1 hour without loaded pre-estimated bandwidths (could be changed in code in section "Bandwidth estimation")
source("./code/non_para_M_2.R")
# Note: see "./code/non_para_M_functions.R" for additional functions used in "non_para_M_2.R".

# ------------ Result collection -------------
# Finally, the results are collected and the graphs for the paper are created.
# Estimated computation time: a few seconds
source("./code/result_merge.R")

