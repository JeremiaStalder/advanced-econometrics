#######################################################################################################################
# Project - Advanced Econometric Methods
# Erik Senn, Johannes Cordier, Mila Gorkun-Voedova, Davia Kündig and Jeremia Stalder
#
# Description:
# This file is meant as a guide through the whole project and helps to understand the structure of our code-files. 
# It will automatically execute the different scripts for data cleaning, estimation and result display, and produce all outputs automatically.
# Each file run here, can also be run independently. All code files are stored in the "code" folder.

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

# Note: most libraries are specified again in the external files, such that each file can run independently. 
# ---------------------------------------------------------------------

# First, set the working directory to the location of the current file. The folders output and code should be in the same folder as this file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

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

# Lasso
# Estimated computation time: 8-16 minutes
source("./code/lasso_final.R")


# ------------ Semi-Parametric -------------



# ------------ Non-Parametric -------------
# Causal random forest estimation
# Estimated computation time: 1-2 minutes
source("./code/causal_random_forest.R")



# Kernel estimation
source("./code/non_para_M_2.R")
# Note: see "./code/non_para_M_functions.R" for additional functions used in "non_para_M_2.R".










# Finally, the results are collected and the graphs for the paper are created.

source("./code/result_merge.R")
