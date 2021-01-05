### Linear Estimators - Simple, Conditional Means, IPW, Doubly-Robust

setwd("D:/GitHub/advanced-econometrics") # setwd
outpath_results_parametric <- "./output/results/parametric/"
sign_level <- 0.05

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
library(rlist)

## functions
source("./code/parametric_extended_functions.R")
filter <- dplyr::filter
select <- dplyr::select

# load data and variable sets
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")

# sort by cate variable (to have correct order of quintiles for cate estimation later. Not pretty, but cate code not robust towards non-sorted data)
mydata_linear_model <- mydata_transform %>%
  arrange(eval(as.name(variable_sets_modelling$cate_vars)))

##### Parametric Estimators ---------------------------------------------
# select variable sets 
d_name <- "e401"
d <- as.matrix(mydata_linear_model[,d_name]) # treatment
x_name <- as.vector(variable_sets_modelling$independent_vars_selection[variable_sets_modelling$independent_vars_selection!="e401"])
x <- as.matrix(mydata_linear_model[,x_name]) # use all base vars except treatment as confounders
y_possible_names <- c(variable_sets_modelling$dependent_vars_selection) # names of outcome vars to consider
y_possible <- as.matrix(mydata_linear_model[,y_possible_names]) # matrix of outcomes to choose from
colnames(d)[1] <- "d"


# parameters
number_bootstrap_samples_ate <- 1000 #number of bootstrap samples for ate
number_bootstrap_samples_cate <- 1000 #number of bootstrap samples
ATE_via_bootstrap <- TRUE# boolean. If FALSE, then ATE calculated with full sample, if TRUE, then ATE is mean of bootstrapped ATEs

# A) ATE ESTIMATION -----------------------------------------------------------------------------------------------------------------

# Bootstrapping Samples for ATE
  # draw bootstrapped samples here
  # purpose: bootstrapped SDs and confidence intervalls
  set.seed(42000) # set seed for comparability
  index_bootstrap_samples_ate <- replicate(number_bootstrap_samples_ate, sample(x=c(1:length(d)), size=length(d), replace = TRUE, prob =NULL), simplify=FALSE) # draw random indices for observations with replacement
  
  # convert list of indices for samples to list of data for samples
  # Result: list of bootstrapped samples
    # function to get sample if matrix with given row indices. As function s.t. it can be used by lapply
    slide_user <- function(row_indices,matrix) {
      return(matrix[row_indices,])
    }
    
    # separate x,d,y bootstrapped samples
     d_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=d)
     x_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=x)
     y_possible_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=y_possible)


# Use different outcome variables 
parametric_results_ate <- vector(mode = "list", length = length(y_possible_names))
names(parametric_results_ate) <- y_possible_names

for (iter_outcome_name in 1:length(y_possible_names)){
  outcome_name <- y_possible_names[iter_outcome_name]
  outcome <- y_possible[,outcome_name]# selected outcome for loop iteration
  # select conditioning:
    # adjust data: run model for 5 quintiles of income data
  
  # 1) Simple Mean comparison ----------------------------------------------------------------------------------------------------
  est <- lm(outcome~d)
  est_mc <- summary(est)
  mc_ate <- summary(est)$coefficients["d",]
  # add confidence intervalls (normally distributed)
  CI_up <- mc_ate[names(mc_ate)=="Estimate"]+qnorm(1-sign_level/2)*mc_ate[names(mc_ate)=="Std. Error"]
  CI_low <- mc_ate[names(mc_ate)=="Estimate"]+qnorm(sign_level/2)*mc_ate[names(mc_ate)=="Std. Error"]
  names(CI_up) <- "Upper CI"
  names(CI_low) <- "Lower CI"
  mc_ate <- c(mc_ate, CI_up, CI_low)
  
  
  # 2) Conditional Means via Ols -------------------------------------------------------------------------------------------------
    # same coefficients for confounders for both treatment states (1 OLS)
      est <- lm(outcome~d+x)
      ols_cond_means <- summary(est)
      OLS_ate <- summary(est)$coefficients["d", ]
      # add confidence intervalls (normally distributed)
      CI_up <- OLS_ate[names(OLS_ate)=="Estimate"]+qnorm(1-sign_level/2)*OLS_ate[names(OLS_ate)=="Std. Error"]
      CI_low <- OLS_ate[names(OLS_ate)=="Estimate"]+qnorm(sign_level/2)*OLS_ate[names(OLS_ate)=="Std. Error"]
      names(CI_up) <- "Upper CI"
      names(CI_low) <- "Lower CI"
      OLS_ate <- c(OLS_ate, CI_up, CI_low)
      
    # flexible coefficients for confounders for treatment states (2 OLS)
      est <- lm(outcome~ x + d*x) # confounders and confounders*treatment as dummies
      ols_cond_means_flex <- summary(est)
      OLS_flex_ate <- summary(est)$coefficients["d", ]
      # add confidence intervalls (normally distributed)
      CI_up <- OLS_flex_ate[names(OLS_flex_ate)=="Estimate"]+qnorm(1-sign_level/2)*OLS_flex_ate[names(OLS_flex_ate)=="Std. Error"]
      CI_low <- OLS_flex_ate[names(OLS_flex_ate)=="Estimate"]+qnorm(sign_level/2)*OLS_flex_ate[names(OLS_flex_ate)=="Std. Error"]
      names(CI_up) <- "Upper CI"
      names(CI_low) <- "Lower CI"
      OLS_flex_ate <- c(OLS_flex_ate, CI_up, CI_low)
      
      
  # Data For 3) and 4) Bootstrapping -------------------------------------------------------------------------------------
      # list for each sample with sublist of x,d and specific outcome variable 
      # all data for bootstrapped samples
      bootstrap_samples_ate <- list(list(y_possible_bootstrap_samples_ate[[1]][,colnames(y_possible_bootstrap_samples_ate[[1]])==outcome_name], 
                                          x_bootstrap_samples_ate[[1]],
                                          d_bootstrap_samples_ate[[1]]))
      names(bootstrap_samples_ate[[1]]) <- c("outcome","x","d")
      for (i in 2:number_bootstrap_samples_ate){
        bootstrap_samples_ate <- list.append(bootstrap_samples_ate, list(y_possible_bootstrap_samples_ate[[i]][,colnames(y_possible_bootstrap_samples_ate[[i]])==outcome_name],
                                                                         x_bootstrap_samples_ate[[i]],
                                                                         d_bootstrap_samples_ate[[i]]))
        names(bootstrap_samples_ate[[i]]) <- c("outcome","x","d")
      }
      
      
  # 3) Inverse Probability Weighting -------------------------------------------------------------------------------------------------
    # ATE full sample
      IPW_input_fct <- list(outcome,x,d) #use original sample
      names(IPW_input_fct) <- c("outcome","x","d")
      IPW_results_fct <- IPW_all_samples(IPW_input_fct,x_name)
      IPW_estimates <- IPW_results_fct$IPW_estimates_fct
      IPW_ate <- c(IPW_estimates$Base,NA,NA,NA,NA,NA) 
      IPW_ate2 <- c(IPW_estimates$Restricted,NA,NA,NA,NA,NA)  
      IPW_ate3 <- c(IPW_estimates$Restricted2,NA,NA,NA,NA,NA) 
      
    # SD-Errors and Confidence Bounds of ATE via Bootstrapping
      IPW_results_fct_bootstrap <- lapply(bootstrap_samples_ate, IPW_all_samples, x_name = x_name)
      
      # get vector of ATE estimates for each sample from list
      IPW_estimates_bootstrap <- NULL
      for (i in 1:length(IPW_results_fct_bootstrap)){
        IPW_estimates_bootstrap <- rbind(IPW_estimates_bootstrap, c(IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Base,
                                         IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Restricted,
                                         IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Restricted2))
      }
      IPW_estimates_bootstrap <- as.data.frame(IPW_estimates_bootstrap)
      colnames(IPW_estimates_bootstrap) <- c("Base","Restricted","Restricted2")

      # Standard errors (2nd position in summary vectors)
      IPW_ate[2] <- sd(IPW_estimates_bootstrap$Base) 
      IPW_ate2[2] <- sd(IPW_estimates_bootstrap$Restricted)
      IPW_ate3[2] <- sd(IPW_estimates_bootstrap$Restricted2)
      
      # Confidence intervals (5th (upper CI) and 6th (lower CI) position in summary vectors)
      IPW_ate[5:6] <- c(quantile(IPW_estimates_bootstrap$Base, 1-sign_level/2),
                        quantile(IPW_estimates_bootstrap$Base, sign_level/2))
      IPW_ate2[5:6] <- c(quantile(IPW_estimates_bootstrap$Restricted, 1-sign_level/2),
                        quantile(IPW_estimates_bootstrap$Restricted, sign_level/2))
      IPW_ate3[5:6] <- c(quantile(IPW_estimates_bootstrap$Restricted2, 1-sign_level/2),
                        quantile(IPW_estimates_bootstrap$Restricted2, sign_level/2))
      
    # ATE via bootstrap
      # replace full sample estimated ATE with mean of bootstrapped ate if ATE_via_bootstrap is true
      if(ATE_via_bootstrap){
        IPW_ate[1] <- mean(IPW_estimates_bootstrap$Base)  
        IPW_ate2[1] <- mean(IPW_estimates_bootstrap$Restricted)
        IPW_ate3[1] <- mean(IPW_estimates_bootstrap$Restricted2)
      }

     
      
  
  # 4) Doubly Robust -------------------------------------------------------------------------------------------------------------------
    # Note: DR requires the results from the propensity score estimation
      DR_results_fct <- DR_all_samples(IPW_results_fct , x_name, outcome_name)
      DR_base_ate <- c(DR_results_fct$Base,NA,NA,NA,NA,NA) 
      DR_help_ate <- c(DR_results_fct$Restricted,NA,NA,NA,NA,NA)  
      DR_help2_ate <- c(DR_results_fct$Restricted2,NA,NA,NA,NA,NA) 
      
    # SD-Errors and Confidence Bounds of ATE via Bootstrapping
      DR_results_fct_bootstrap <- lapply(IPW_results_fct_bootstrap, DR_all_samples, x_name = x_name, outcome_name = outcome_name)
      
      # get vector of ATE estimates for each sample from list
      DR_estimates_bootstrap <- NULL
      for (i in 1:length(DR_results_fct_bootstrap)){
        DR_estimates_bootstrap <- rbind(DR_estimates_bootstrap, c(DR_results_fct_bootstrap[[i]]$Base,
                                                                  DR_results_fct_bootstrap[[i]]$Restricted,
                                                                  DR_results_fct_bootstrap[[i]]$Restricted2))
      }
      DR_estimates_bootstrap <- as.data.frame(DR_estimates_bootstrap)
      colnames(DR_estimates_bootstrap) <- c("Base","Restricted","Restricted2")
      
      # Standard errors (2nd position in summary vectors)
      DR_base_ate[2] <- sd(DR_estimates_bootstrap$Base) 
      DR_help_ate[2] <- sd(DR_estimates_bootstrap$Restricted)
      DR_help2_ate[2] <- sd(DR_estimates_bootstrap$Restricted2)
      
      # Confidence intervals (5th (upper CI) and 6th (lower CI) position in summary vectors)
      DR_base_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Base, 1-sign_level/2),
                        quantile(DR_estimates_bootstrap$Base, sign_level/2))
      DR_help_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Restricted, 1-sign_level/2),
                         quantile(DR_estimates_bootstrap$Restricted, sign_level/2))
      DR_help2_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Restricted2, 1-sign_level/2),
                         quantile(DR_estimates_bootstrap$Restricted2, sign_level/2))
      
    # ATE via bootstrap
      # replace full sample estimated ATE with mean of bootstrapped ate if ATE_via_bootstrap is true
      if(ATE_via_bootstrap){
        DR_base_ate[1] <- mean(DR_estimates_bootstrap$Base)  
        DR_help_ate[1] <- mean(DR_estimates_bootstrap$Restricted)
        DR_help2_ate[1] <- mean(DR_estimates_bootstrap$Restricted2)
      }
      
  
  # Comparison ---------------------------------------------------------------------------------------------------
  parametric_est <- cbind(mc_ate, OLS_ate, OLS_flex_ate,IPW_ate, IPW_ate2, IPW_ate3,DR_base_ate,DR_help_ate,DR_help2_ate)
  colnames(parametric_est) <- c("Mean Comparison", "Cond. Means", "Cond_Means_flex", "IPW", "IPW_restricted", "IPW_restricted2","Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")
  
  # Collect in list
  parametric_results_ate[[iter_outcome_name]] = parametric_est
}

# save image of all results
save(parametric_results_ate,file=paste0(outpath_results_parametric,"_parametric_results_ate.Rdata"))


# print results for all measures
for (i in 1:length(parametric_results_ate)){
  print(names(parametric_results_ate[i]))
  print(parametric_results_ate[[i]])
}

# overview table of only ATEs in latex
  ate_table_all_outcomes <- as.data.frame(t(parametric_results_ate[[1]][1,]))
  
  for (i in 2:length(parametric_results_ate)){
    ate_table_all_outcomes<- rbind(ate_table_all_outcomes,t(parametric_results_ate[[i]][1,]))
  }
  rownames(ate_table_all_outcomes) <- names(parametric_results_ate)
    
  print(xtable(ate_table_all_outcomes), type="latex",paste0(outpath_results_parametric, "parametric_ate.tex"))
  
# table all estimators in latex
  print(xtable(parametric_results_ate$tw_adjust_original[c(1,2,5,6),]), type="latex",paste0(outpath_results_parametric, "parametric_tw_original_all.tex"))
  print(xtable(parametric_results_ate$tw_adjust_quantile[c(1,2,5,6),]), type="latex",paste0(outpath_results_parametric, "parametric_tw_quantile_all.tex"))
  
# table selected estimators in latex
  print(xtable(parametric_results_ate$tw_adjust_original[c(1,2,5,6),c("Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")]), type="latex",paste0(outpath_results_parametric, "parametric_tw_original_DR.tex"))
  print(xtable(parametric_results_ate$tw_adjust_quantile[c(1,2,5,6),c("Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")]), type="latex",paste0(outpath_results_parametric, "parametric_tw_quantile_DR.tex"))
  
  

# B) CATE ESTIMATION -----------------------------------------------------------------------------------------------------------------

# Use income quintiles as CATE
cate_variable_name <- variable_sets_modelling$cate_vars # select cate variable name. IMPORTANT: CATE CAN ONLY HAVE VALUES 1-5 atm, otherwise change code (cate_variable==1,2,3,4,5 below)
cate_variable <- as.vector(mydata_linear_model[,cate_variable_name])

# loop for each different value of X (cate_variable)
results_cate <- vector(mode = "list", length = length(unique(cate_variable))) # only for effect estimate, no CIs
parametric_results_cate_all <- vector(mode = "list", length = length(unique(cate_variable)))
names(results_cate) <- paste0("CATE_inc_quantile",c(1:5))
names(parametric_results_cate_all) <- paste0("CATE_inc_quantile",c(1:5))

# rename original data to avoid renaming in code
y_possible_original <- y_possible
x_original<- x
d_original <- d

# check if sorted
print("Check if Sorted after quintiles (all outputs true). If not, then need to sort!")
print(all(unique(cate_variable)==c(1:length(unique(cate_variable)))))

for (iter_cate in 1:length(unique(cate_variable))){
  # condition dataset used: X=x
  y_possible <- as.matrix(y_possible_original[unique(cate_variable)[iter_cate]==cate_variable,])
  x <-  as.matrix(x_original[unique(cate_variable)[iter_cate]==cate_variable,])
  d <- as.matrix(d_original[unique(cate_variable)[iter_cate]==cate_variable,])
  
  # Use different outcome variables 
  results_outcome <- vector(mode = "list", length = length(y_possible_names))
  names(results_outcome) <- y_possible_names
  
  # Bootstrapping Samples for CATE
  # draw bootstrapped samples here
  # purpose: bootstrapped SDs and confidence intervalls
  set.seed(42000) # set seed for comparability
  index_bootstrap_samples_ate <- replicate(number_bootstrap_samples_cate, sample(x=c(1:length(d)), size=length(d), replace = TRUE, prob =NULL), simplify=FALSE) # draw random indices for observations with replacement
  
  # convert list of indices for samples to list of data for samples
  # Result: list of bootstrapped samples
  
  # separate x,d,y bootstrapped samples
  d_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=d)
  x_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=x)
  y_possible_bootstrap_samples_ate <- lapply(index_bootstrap_samples_ate,slide_user,matrix=y_possible)
  
  for (iter_outcome_name in 1:length(y_possible_names)){
    outcome_name <- y_possible_names[iter_outcome_name]
    outcome <- y_possible[,outcome_name]# selected outcome for loop iteration
    # select conditioning:
    # adjust data: run model for 5 quintiles of income data
    
    # 1) Simple Mean comparison ----------------------------------------------------------------------------------------------------
    est <- lm(outcome~d)
    est_mc <- summary(est)
    mc_ate <- summary(est)$coefficients["d",]
    # add confidence intervalls (normally distributed)
    CI_up <- mc_ate[names(mc_ate)=="Estimate"]+qnorm(1-sign_level/2)*mc_ate[names(mc_ate)=="Std. Error"]
    CI_low <- mc_ate[names(mc_ate)=="Estimate"]+qnorm(sign_level/2)*mc_ate[names(mc_ate)=="Std. Error"]
    names(CI_up) <- "Upper CI"
    names(CI_low) <- "Lower CI"
    mc_ate <- c(mc_ate, CI_up, CI_low)
    
    
    # 2) Conditional Means via Ols -------------------------------------------------------------------------------------------------
      # same coefficients for confounders for both treatment states (1 OLS)
      est <- lm(outcome~d+x)
      ols_cond_means <- summary(est)
      OLS_ate <- summary(est)$coefficients["d", ]
      # add confidence intervalls (normally distributed)
      CI_up <- OLS_ate[names(OLS_ate)=="Estimate"]+qnorm(1-sign_level/2)*OLS_ate[names(OLS_ate)=="Std. Error"]
      CI_low <- OLS_ate[names(OLS_ate)=="Estimate"]+qnorm(sign_level/2)*OLS_ate[names(OLS_ate)=="Std. Error"]
      names(CI_up) <- "Upper CI"
      names(CI_low) <- "Lower CI"
      OLS_ate <- c(OLS_ate, CI_up, CI_low)
      
      # flexible coefficients for confounders for treatment states (2 OLS)
      est <- lm(outcome~ x + d*x) # confounders and confounders*treatment as dummies
      ols_cond_means_flex <- summary(est)
      OLS_flex_ate <- summary(est)$coefficients["d", ]
      # add confidence intervalls (normally distributed)
      CI_up <- OLS_flex_ate[names(OLS_flex_ate)=="Estimate"]+qnorm(1-sign_level/2)*OLS_flex_ate[names(OLS_flex_ate)=="Std. Error"]
      CI_low <- OLS_flex_ate[names(OLS_flex_ate)=="Estimate"]+qnorm(sign_level/2)*OLS_flex_ate[names(OLS_flex_ate)=="Std. Error"]
      names(CI_up) <- "Upper CI"
      names(CI_low) <- "Lower CI"
      OLS_flex_ate <- c(OLS_flex_ate, CI_up, CI_low)
    
    
      
    # Data For 3) and 4) Bootstrapping -------------------------------------------------------------------------------------
      # list for each sample with sublist of x,d and specific outcome variable 
      # all data for bootstrapped samples
      bootstrap_samples_ate <- list(list(y_possible_bootstrap_samples_ate[[1]][,colnames(y_possible_bootstrap_samples_ate[[1]])==outcome_name], 
                                         x_bootstrap_samples_ate[[1]],
                                         d_bootstrap_samples_ate[[1]]))
      names(bootstrap_samples_ate[[1]]) <- c("outcome","x","d")
      for (i in 2:number_bootstrap_samples_cate){
        bootstrap_samples_ate <- list.append(bootstrap_samples_ate, list(y_possible_bootstrap_samples_ate[[i]][,colnames(y_possible_bootstrap_samples_ate[[i]])==outcome_name],
                                                                         x_bootstrap_samples_ate[[i]],
                                                                         d_bootstrap_samples_ate[[i]]))
        names(bootstrap_samples_ate[[i]]) <- c("outcome","x","d")
      }
      
      # 3) Inverse Probability Weighting -------------------------------------------------------------------------------------------------
      # ATE full sample
      IPW_input_fct <- list(outcome,x,d) #use original sample
      names(IPW_input_fct) <- c("outcome","x","d")
      IPW_results_fct <- IPW_all_samples(IPW_input_fct,x_name)
      IPW_estimates <- IPW_results_fct$IPW_estimates_fct
      IPW_ate <- c(IPW_estimates$Base,NA,NA,NA,NA,NA) 
      IPW_ate2 <- c(IPW_estimates$Restricted,NA,NA,NA,NA,NA)  
      IPW_ate3 <- c(IPW_estimates$Restricted2,NA,NA,NA,NA,NA) 
      
      # SD-Errors and Confidence Bounds of ATE via Bootstrapping
      IPW_results_fct_bootstrap <- lapply(bootstrap_samples_ate, IPW_all_samples, x_name = x_name)
      
      # get vector of ATE estimates for each sample from list
      IPW_estimates_bootstrap <- NULL
      for (i in 1:length(IPW_results_fct_bootstrap)){
        IPW_estimates_bootstrap <- rbind(IPW_estimates_bootstrap, c(IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Base,
                                                                    IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Restricted,
                                                                    IPW_results_fct_bootstrap[[i]]$IPW_estimates_fct$Restricted2))
      }
      IPW_estimates_bootstrap <- as.data.frame(IPW_estimates_bootstrap)
      colnames(IPW_estimates_bootstrap) <- c("Base","Restricted","Restricted2")
      
      # Standard errors (2nd position in summary vectors)
      IPW_ate[2] <- sd(IPW_estimates_bootstrap$Base) 
      IPW_ate2[2] <- sd(IPW_estimates_bootstrap$Restricted)
      IPW_ate3[2] <- sd(IPW_estimates_bootstrap$Restricted2)
      
      # Confidence intervals (5th (upper CI) and 6th (lower CI) position in summary vectors)
      IPW_ate[5:6] <- c(quantile(IPW_estimates_bootstrap$Base, 1-sign_level/2),
                        quantile(IPW_estimates_bootstrap$Base, sign_level/2))
      IPW_ate2[5:6] <- c(quantile(IPW_estimates_bootstrap$Restricted, 1-sign_level/2),
                         quantile(IPW_estimates_bootstrap$Restricted, sign_level/2))
      IPW_ate3[5:6] <- c(quantile(IPW_estimates_bootstrap$Restricted2, 1-sign_level/2),
                         quantile(IPW_estimates_bootstrap$Restricted2, sign_level/2))
      
      # ATE via bootstrap
      # replace full sample estimated ATE with mean of bootstrapped ate if ATE_via_bootstrap is true
      if(ATE_via_bootstrap){
        IPW_ate[1] <- mean(IPW_estimates_bootstrap$Base)  
        IPW_ate2[1] <- mean(IPW_estimates_bootstrap$Restricted)
        IPW_ate3[1] <- mean(IPW_estimates_bootstrap$Restricted2)
      }
      
      
      
      
      # 4) Doubly Robust -------------------------------------------------------------------------------------------------------------------
      # Note: DR requires the results from the propensity score estimation
      DR_results_fct <- DR_all_samples(IPW_results_fct , x_name, outcome_name)
      DR_base_ate <- c(DR_results_fct$Base,NA,NA,NA,NA,NA) 
      DR_help_ate <- c(DR_results_fct$Restricted,NA,NA,NA,NA,NA)  
      DR_help2_ate <- c(DR_results_fct$Restricted2,NA,NA,NA,NA,NA) 
      
      # SD-Errors and Confidence Bounds of ATE via Bootstrapping
      DR_results_fct_bootstrap <- lapply(IPW_results_fct_bootstrap, DR_all_samples, x_name = x_name, outcome_name = outcome_name)
      
      # get vector of ATE estimates for each sample from list
      DR_estimates_bootstrap <- NULL
      for (i in 1:length(DR_results_fct_bootstrap)){
        DR_estimates_bootstrap <- rbind(DR_estimates_bootstrap, c(DR_results_fct_bootstrap[[i]]$Base,
                                                                  DR_results_fct_bootstrap[[i]]$Restricted,
                                                                  DR_results_fct_bootstrap[[i]]$Restricted2))
      }
      DR_estimates_bootstrap <- as.data.frame(DR_estimates_bootstrap)
      colnames(DR_estimates_bootstrap) <- c("Base","Restricted","Restricted2")
      
      # Standard errors (2nd position in summary vectors)
      DR_base_ate[2] <- sd(DR_estimates_bootstrap$Base) 
      DR_help_ate[2] <- sd(DR_estimates_bootstrap$Restricted)
      DR_help2_ate[2] <- sd(DR_estimates_bootstrap$Restricted2)
      
      # Confidence intervals (5th (upper CI) and 6th (lower CI) position in summary vectors)
      DR_base_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Base, 1-sign_level/2),
                            quantile(DR_estimates_bootstrap$Base, sign_level/2))
      DR_help_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Restricted, 1-sign_level/2),
                            quantile(DR_estimates_bootstrap$Restricted, sign_level/2))
      DR_help2_ate[5:6] <- c(quantile(DR_estimates_bootstrap$Restricted2, 1-sign_level/2),
                             quantile(DR_estimates_bootstrap$Restricted2, sign_level/2))
      
      # ATE via bootstrap
      # replace full sample estimated ATE with mean of bootstrapped ate if ATE_via_bootstrap is true
      if(ATE_via_bootstrap){
        DR_base_ate[1] <- mean(DR_estimates_bootstrap$Base)  
        DR_help_ate[1] <- mean(DR_estimates_bootstrap$Restricted)
        DR_help2_ate[1] <- mean(DR_estimates_bootstrap$Restricted2)
      }
    
    
    # Comparison ---------------------------------------------------------------------------------------------------
    parametric_est <- cbind(mc_ate, OLS_ate, OLS_flex_ate,IPW_ate, IPW_ate2, IPW_ate3,DR_base_ate,DR_help_ate,DR_help2_ate)
    colnames(parametric_est) <- c("Mean Comparison", "Cond. Means", "Cond_Means_flex", "IPW", "IPW_restricted", "IPW_restricted2","Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")
    
    # Collect in list
    results_outcome[[iter_outcome_name]] = parametric_est
  }
  
  
  # list CATE, std, CIs
   parametric_results_cate_all[[iter_cate]] <- results_outcome
  
  # overview table of only estimate CATEs in latex
    cate_table_all_outcomes <- as.data.frame(t(results_outcome[[1]][1,]))
    
    for (i in 2:length(results_outcome)){
      cate_table_all_outcomes<- rbind(cate_table_all_outcomes,t(results_outcome[[i]][1,]))
    }
    rownames(cate_table_all_outcomes) <- names(results_outcome)
    
    # collect all cate results in table
    results_cate[[iter_cate]] <- cate_table_all_outcomes
}

# save all results
save(parametric_results_cate_all,file=paste0(outpath_results_parametric,"_parametric_results_cate_all.Rdata"))

# overview latex table
  # build small overview tables
    #tw original initialize
    result_tw_original <- matrix(nrow=4, ncol=5)
    colnames(result_tw_original) <- paste0("Income_Quintile_",c(1:5))
    rownames(result_tw_original) <- c("Mean Comparison","Cond. Means","IPW_restricted","Doubly_robust_base")
    
   #tw quantiles initialize
    result_tw_quantiles <- matrix(nrow=4, ncol=5)
    colnames(result_tw_quantiles) <- paste0("Income_Quintile_",c(1:5))
    rownames(result_tw_quantiles) <- c("Mean Comparison","Cond. Means","IPW_restricted","Doubly_robust_base")
    
    # fill tables 
    for (i in 1:ncol(result_tw_original)){
      result_tw_original[,i] <- parametric_results_cate_all[[i]]$tw_adjust_original[1,colnames(parametric_results_cate_all[[i]]$tw_adjust_original) %in% rownames(result_tw_original)]
      result_tw_quantiles[,i] <- parametric_results_cate_all[[i]]$tw_adjust_quantile[1,colnames(parametric_results_cate_all[[i]]$tw_adjust_quantile) %in% rownames(result_tw_quantiles)]
    }
    # save latex tables
    print(xtable(result_tw_original), type="latex",paste0(outpath_results_parametric, "parametric_cate_result_tw_original.tex"))
    print(xtable(result_tw_quantiles), type="latex",paste0(outpath_results_parametric, "parametric_cate_result_tw_quantiles.tex"))
    
    
    # build cate table all estimation methods
    cate_table_all_outcomes_all_cates <- rbind(NA,results_cate[[1]])  # add na as placeholder line for new value x to condition on + add cate estimates
    for (i in 2:length(unique(cate_variable))){
      cate_table_all_outcomes_all_cates<- rbind(cate_table_all_outcomes_all_cates, NA) # add na as placeholder line for new value x to condition on 
      cate_table_all_outcomes_all_cates<- rbind(cate_table_all_outcomes_all_cates,results_cate[[i]]) # add cate estimates
    }
    # adjust rowname for placehoder row with na when new cate starts
    for (i in 1:length(results_cate)){
      rownames(cate_table_all_outcomes_all_cates)[1+(i-1)*(length(y_possible_names)+1)] <- names(results_cate)[i]
    }
    
    print(xtable(cate_table_all_outcomes_all_cates), type="latex",paste0(outpath_results_parametric, "cate_table_all_outcomes_all_cates.tex"))

    
# C) create formatted result list
    # create lists for each estimator (easier to combine with other estimators)
    df_estimator_results <- as.data.frame(matrix(nrow=4, ncol=6))
    colnames(df_estimator_results) <- c("ATE","CATE_inc_quantile1","CATE_inc_quantile2","CATE_inc_quantile3","CATE_inc_quantile4","CATE_inc_quantile5")
    rownames(df_estimator_results) <- rownames(parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original)[c(1,2,5,6)]
    
    all_results_parametric_tw = vector(mode = "list", length = ncol(parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original))
    names(all_results_parametric_tw) = colnames(parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original)
    
    all_results_parametric_tw_quantiles = vector(mode = "list", length = ncol(parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original))
    names(all_results_parametric_tw) = colnames(parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original)
    
    for (i in 1:length(all_results_parametric_tw)){
      df_estimator_results_loop = df_estimator_results
      df_estimator_results_loop$ATE = parametric_results_ate$tw_adjust_original[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile1 = parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_original[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile2 = parametric_results_cate_all$CATE_inc_quantile2$tw_adjust_original[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile3 = parametric_results_cate_all$CATE_inc_quantile3$tw_adjust_original[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile4 = parametric_results_cate_all$CATE_inc_quantile4$tw_adjust_original[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile5 = parametric_results_cate_all$CATE_inc_quantile5$tw_adjust_original[,i][c(1,2,5,6)]
      all_results_parametric_tw[[i]] = df_estimator_results_loop
    }
    
    for (i in 1:length(all_results_parametric_tw_quantiles)){
      df_estimator_results_loop = df_estimator_results
      df_estimator_results_loop$ATE = parametric_results_ate$tw_adjust_quantile[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile1 = parametric_results_cate_all$CATE_inc_quantile1$tw_adjust_quantile[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile2 = parametric_results_cate_all$CATE_inc_quantile2$tw_adjust_quantile[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile3 = parametric_results_cate_all$CATE_inc_quantile3$tw_adjust_quantile[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile4 = parametric_results_cate_all$CATE_inc_quantile4$tw_adjust_quantile[,i][c(1,2,5,6)]
      df_estimator_results_loop$CATE_inc_quantile5 = parametric_results_cate_all$CATE_inc_quantile5$tw_adjust_quantile[,i][c(1,2,5,6)]
      all_results_parametric_tw_quantiles[[i]] = df_estimator_results_loop
    }
    
    all_results_parametric <- list(all_results_parametric_tw, all_results_parametric_tw_quantiles)
    names(all_results_parametric) = c("tw_adjust_original","tw_adjust_quantile")
    
    # save all results
    save(all_results_parametric,file=paste0(outpath_results_parametric,"_parametric_all_results.Rdata"))
    