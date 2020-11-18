### Causal Random Forest

setwd("D:/GitHub/advanced-econometrics") # setwd
outpath_results_crf <- "./output/results/random_forest/"

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
library(grf) # random forest package

## functions
filter <- dplyr::filter
select <- dplyr::select

# load data and variable sets
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")

mydata_crf <- mydata_transform

##### Parametric Estimators ---------------------------------------------
# select variable sets: crf does not need transformed confounders, each conf. only once
d_name <- "e401"
d <- as.matrix(mydata_crf[,d_name]) # treatment
colnames(d)[1] <- "d"
x_name <- c("ira","hval","hmort","hequity","age","inc","fsize","educ","db","marr","male","twoearn") # confounders: all raw variables (no transformations needed)
x <- as.matrix(mydata_crf[,x_name]) # use all base vars except treatment as confounders
y_possible_names <- c(variable_sets_modelling$dependent_vars_selection,variable_sets_modelling$dependent_vars_selection_quantile) # names of outcome vars to consider
y_possible <- as.matrix(mydata_crf[,y_possible_names]) # matrix of outcomes to choose from
cate_variable_name <- variable_sets_modelling$cate_vars # select cate variable name. IMPORTANT: CATE CAN ONLY HAVE VALUES 1-5 atm, otherwise change code (cate_variable==1,2,3,4,5 below)
cate_variable <- as.vector(mydata_crf[,cate_variable_name])

# Use different outcome variables 
results_outcome <- vector(mode = "list", length = length(y_possible_names))
names(results_outcome) <- y_possible_names

for (iter_outcome_name in 1:length(y_possible_names)){ 
  outcome_name <- y_possible_names[iter_outcome_name]
  outcome <- y_possible[,outcome_name]# selected outcome for loop iteration
  
  
  # 1) Causal Random Forest ------------------------------------------------------------------------------------------------------
  crf_model <- causal_forest(x,outcome,d,num.trees = 2000, sample.fraction = 0.5,honesty = T, min.node.size = 5) #train forest
  
  # ATE & CATE estimation
  # Note: CATES only with 5 quintes implemented. Otherwise change code.
    # "by hand"
    # NOTE: idk how to get sds for ATE then
    individual_treatment_effects <- predict(crf_model,estimate.variance=T)
    hist(individual_treatment_effects$predictions)
    crf_ate <- mean(individual_treatment_effects$predictions)
    crf_cate_income <- c(mean(individual_treatment_effects$predictions[cate_variable==1]),
                         mean(individual_treatment_effects$predictions[cate_variable==2]),
                         mean(individual_treatment_effects$predictions[cate_variable==3]),
                         mean(individual_treatment_effects$predictions[cate_variable==4]),
                         mean(individual_treatment_effects$predictions[cate_variable==5]))
    
    # package with augmented IPW (i dont know what this is, but results are similar)
    crf_ate_aipw <- average_treatment_effect(crf_model,
                                               target.sample = c("all"),
                                               method = "AIPW",
                                               subset=NULL) # augmented IPW for estimation.
    
    crf_cate_income_aipw <- cbind(average_treatment_effect(crf_model,
                                                      target.sample = c("all"),
                                                      method = "AIPW",
                                                      subset=cate_variable==1),
                              average_treatment_effect(crf_model,
                                                       target.sample = c("all"),
                                                       method = "AIPW",
                                                       subset=cate_variable==2),
                              average_treatment_effect(crf_model,
                                                       target.sample = c("all"),
                                                       method = "AIPW",
                                                       subset=cate_variable==3),
                              average_treatment_effect(crf_model,
                                                       target.sample = c("all"),
                                                       method = "AIPW",
                                                       subset=cate_variable==4),
                              average_treatment_effect(crf_model,
                                                       target.sample = c("all"),
                                                       method = "AIPW",
                                                       subset=cate_variable==5)
                                                       
                              )
    colnames(crf_cate_income_aipw) <- paste0("inc_quantile",c(1:5))
 

  # Comparison ---------------------------------------------------------------------------------------------------
  crf_est<- c(crf_ate,crf_cate_income)
  crf_est_aipw <- cbind(crf_ate_aipw,crf_cate_income_aipw)
  names(crf_est) <-c("ATE", paste0("CATE_inc_quantile",c(1:5)))
  colnames(crf_est_aipw) <- c("ATE", paste0("CATE_inc_quantile",c(1:5)))
  
  # Collect in list
  results_outcome[[iter_outcome_name]] = crf_est_aipw # only use aipw estimator (because I have standard error)
  
}

# print results for all measures
for (i in 1:length(results_outcome)){
  print(names(results_outcome[i]))
  print(results_outcome[[i]])
}

# overview table of only ATEs in latex
  ate_cate_table_all_outcomes <- as.data.frame(t(results_outcome[[1]][1,]))

for (i in 2:length(results_outcome)){
  ate_cate_table_all_outcomes<- rbind(ate_cate_table_all_outcomes,t(results_outcome[[i]][1,]))
}
rownames(ate_cate_table_all_outcomes) <- names(results_outcome)

print(xtable(ate_cate_table_all_outcomes), type="latex",paste0(outpath_results_crf, "ate_cate_table_all_outcomes.tex"))
