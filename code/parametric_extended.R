### Linear Estimators - Simple, Conditional Means, IPW, Doubly-Robust

setwd("D:/GitHub/advanced-econometrics") # setwd
outpath_results_parametric <- "./output/results/parametric/"

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

## functions
filter <- dplyr::filter
select <- dplyr::select

# load data and variable sets
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")

mydata_linear_model <- mydata_transform

##### Parametric Estimators ---------------------------------------------
# select variable sets 
d_name <- "e401"
d <- as.matrix(mydata_linear_model[,d_name]) # treatment
x_name <- as.vector(variable_sets_modelling$independent_vars_selection[variable_sets_modelling$independent_vars_selection!="e401"])
x <- as.matrix(mydata_linear_model[,x_name]) # use all base vars except treatment as confounders
y_possible_names <- c(variable_sets_modelling$dependent_vars_selection) # names of outcome vars to consider
y_possible <- as.matrix(mydata_linear_model[,y_possible_names]) # matrix of outcomes to choose from
colnames(d)[1] <- "d"

# A) ATE ESTIMATION -----------------------------------------------------------------------------------------------------------------

# Use different outcome variables 
results_outcome <- vector(mode = "list", length = length(y_possible_names))
names(results_outcome) <- y_possible_names

for (iter_outcome_name in 1:length(y_possible_names)){
  outcome_name <- y_possible_names[iter_outcome_name]
  outcome <- y_possible[,outcome_name]# selected outcome for loop iteration
  # select conditioning:
    # adjust data: run model for 5 quintiles of income data
  
  
  
  # 1) Simple Mean comparison ----------------------------------------------------------------------------------------------------
  est <- lm(outcome~d)
  summary(est)
  est_mc <- summary(est)
  mc_ate <- summary(est)$coefficients["d",]
  #mc_ate <- summary(est)$coefficients["d",1]
  
  
  # 2) Conditional Means via Ols -------------------------------------------------------------------------------------------------
    # same coefficients for confounders for both treatment states (1 OLS)
      est <- lm(outcome~d+x)
      summary(est)
      
      ols_cond_means <- summary(est)
      OLS_ate <- summary(est)$coefficients["d", ]
      #OLS_ate <- summary(est)$coefficients["d", 1] # remove after  I get sds for all estimates
      
    # flexible coefficients for confounders for treatment states (2 OLS)
      est <- lm(outcome~ x + d*x) # confounders and confounders*treatment as dummies
      summary(est)
      
      ols_cond_means_flex <- summary(est)
      OLS_flex_ate <- summary(est)$coefficients["d", ]
      #OLS_flex_ate <- summary(est)$coefficients["d", 1] # remove after I get sds for all estimates
      
      
  # 3) Inverse Probability Weighting -------------------------------------------------------------------------------------------------
    # estimate propensity score via probit
      probit <- function(d, x){
        out <- glm(d~x, family = "binomial"(link = "probit"))
        return(as.matrix(out$fitted.values))
      }
      
      out <- glm(d~x, family = "binomial")
      summary(out)
      phat <- probit(d, x)
  
    # test if propensity score close to 0 / 1
      mean <- mean(phat)
      max <- max(phat)
      min <- min(phat)
      colname <- cbind("min", "mean", "max")
      desc_phat <- cbind(min, mean, max)
      hist(phat, breaks=100)
      
    # davia idea: remove obs with prop scores close to 0 / 1 according to boundries in hubner, lechner, wunsch (2013)
      ipw_data <- as.data.frame(cbind(outcome, phat, d, x))
      colnames(ipw_data) <- c(outcome_name, "phat", "d", x_name)
      count_obs_removed_help <- sum(!(ipw_data$phat < 0.95 & ipw_data$phat > 0.05))
      count_obs_removed_help2 <- sum(!(ipw_data$phat < 0.90 & ipw_data$phat > 0.10))
      help <- ipw_data[ipw_data$phat < 0.95 & ipw_data$phat > 0.05, ]
      help2 <- ipw_data[ipw_data$phat < 0.90 & ipw_data$phat > 0.10, ]
      
      print(paste("Removed", count_obs_removed_help ,"obs with min 5% distance from 0/1"))
      print(paste("Removed", count_obs_removed_help2 ,"obs with min 10% distance from 0/1"))
  
    # IPW for treatment effect
      IPW <- function(phat,y,d){
        ate <- mean((d*y)/phat - ((1-d)*y/(1-phat)))
        return(ate)
      }
  
    IPW_ate <- c(IPW(phat,outcome,d),NA,NA,NA) # ate_estimate, sd, t-val, p-val
    IPW_ate2 <- c(IPW(help$phat, help[,outcome_name], help$d),NA,NA,NA)  # ate_estimate, sd, t-val, p-val
    IPW_ate3 <- c(IPW(help2$phat, help2[,outcome_name], help2$d),NA,NA,NA) # ate_estimate, sd, t-val, p-val
    
    print("IPW Effects:")
    print(paste("Base:",IPW_ate[1]))
    print(paste("Help:",IPW_ate2[1]))
    print(paste("Help2:",IPW_ate3[1]))
    
    # todo: sd-errors. t-val, p-val
  
  # 4) Doubly Robust -------------------------------------------------------------------------------------------------------------------
    # use propensity scores as estimated above
    
    # estimate conditional means models with the 3 prop specifications (removing of obs)
    ipw_base_est <- lm(ipw_data[,outcome_name]~ as.matrix(ipw_data[,c("d",x_name)])) # confounders and confounders*treatment as dummies
    ipw_help_est <- lm(help[,outcome_name]~ as.matrix(help[,c("d",x_name)]))  # same for restricted data
    ipw_help2_est <- lm(help2[,outcome_name]~ as.matrix(help2[,c("d",x_name)])) #same for restricted data
    
    # estimate ate
    doubly_robust <- function(yhat_d0, yhat_d1,phat,y,d){
      diff_yhat <- yhat_d1- yhat_d0 # difference conditional means
      frac_1 <- d*(y-yhat_d1)/phat 
      frac_2 <- (1-d)*(y-yhat_d0)/(1-phat)
      ate <- 1/length(y)*sum(diff_yhat+frac_1-frac_2)
      return(ate)
    }
      # base
      yhat_d0 <- ipw_base_est$fitted.values - ipw_data$d*ipw_base_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
      yhat_d1 <- ipw_base_est$fitted.values + (1-ipw_data$d)*ipw_base_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
      DR_base_ate <- c(doubly_robust(yhat_d0,
                              yhat_d1,
                              ipw_data$phat,
                              ipw_data[,outcome_name],
                              ipw_data$d),
                       NA,NA,NA) # ate_estimate, sd, t-val, p-val
      
      # subsample help
      yhat_d0 <- ipw_help_est$fitted.values - help$d*ipw_help_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
      yhat_d1 <- ipw_help_est$fitted.values + (1-help$d)*ipw_help_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
      DR_help_ate <- c(doubly_robust(yhat_d0,
                                   yhat_d1,
                                   help$phat,
                                   help[,outcome_name],
                                   help$d),
                       NA,NA,NA) # ate_estimate, sd, t-val, p-val
      
      # subsample help2
      yhat_d0 <- ipw_help2_est$fitted.values - help2$d*ipw_help2_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
      yhat_d1 <- ipw_help2_est$fitted.values + (1-help2$d)*ipw_help2_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
      DR_help2_ate <- c(doubly_robust(yhat_d0,
                                   yhat_d1,
                                   help2$phat,
                                   help2[,outcome_name],
                                   help2$d),
                        NA,NA,NA)# ate_estimate, sd, t-val, p-val
      
      print("Double Robust Effects:")
      print(paste("Base:",DR_base_ate[1]))
      print(paste("Help:",DR_help_ate[1]))
      print(paste("Help2:",DR_help2_ate[1]))
      # ?? variance estimation : bootstrap or delta-method? -> not done yet, bootstrap would be possible to implement.
    # ?? pvalues: dont know its distribution
    
  
  # Comparison ---------------------------------------------------------------------------------------------------
  parametric_est <- cbind(mc_ate, OLS_ate, OLS_flex_ate,IPW_ate, IPW_ate2, IPW_ate3,DR_base_ate,DR_help_ate,DR_help2_ate)
  colnames(parametric_est) <- c("Mean Comparison", "OLS", "OLS_flex", "IPW", "IPW_restricted", "IPW_restricted2","Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")
  
  # Collect in list
  results_outcome[[iter_outcome_name]] = parametric_est
}

# print results for all measures
for (i in 1:length(results_outcome)){
  print(names(results_outcome[i]))
  print(results_outcome[[i]])
}

# overview table of only ATEs in latex
ate_table_all_outcomes <- as.data.frame(t(results_outcome[[1]][1,]))

for (i in 2:length(results_outcome)){
  ate_table_all_outcomes<- rbind(ate_table_all_outcomes,t(results_outcome[[i]][1,]))
}
rownames(ate_table_all_outcomes) <- names(results_outcome)
  
print(xtable(ate_table_all_outcomes), type="latex",paste0(outpath_results_parametric, "ate_table_all_outcomes.tex"))




# B) CATE ESTIMATION -----------------------------------------------------------------------------------------------------------------

# Use income quintiles as CATE
cate_variable_name <- variable_sets_modelling$cate_vars # select cate variable name. IMPORTANT: CATE CAN ONLY HAVE VALUES 1-5 atm, otherwise change code (cate_variable==1,2,3,4,5 below)
cate_variable <- as.vector(mydata_linear_model[,cate_variable_name])

# loop for each different value of X (cate_variable)
results_cate <- vector(mode = "list", length = length(unique(cate_variable)))
names(results_cate) <- paste0("CATE_inc_quantile",c(1:5))

for (iter_cate in 1:length(unique(cate_variable))){
  # condition dataset used: X=x
  y_possible_cate <- y_possible[unique(cate_variable)[iter_cate]==cate_variable,]
  x_cate <-  x[unique(cate_variable)[iter_cate]==cate_variable,]
  d_cate <- d[unique(cate_variable)[iter_cate]==cate_variable,]
  
  # Use different outcome variables 
  results_outcome <- vector(mode = "list", length = length(y_possible_names))
  names(results_outcome) <- y_possible_names
  
  for (iter_outcome_name in 1:length(y_possible_names)){
    outcome_name <- y_possible_names[iter_outcome_name]
    outcome_cate <- y_possible_cate[,outcome_name]# selected outcome for loop iteration
    # select conditioning:
    # adjust data: run model for 5 quintiles of income data
    
    # 1) Simple Mean comparison ----------------------------------------------------------------------------------------------------
    est <- lm(outcome_cate~d_cate)
    summary(est)
    est_mc <- summary(est)
    mc_ate <- summary(est)$coefficients["d_cate",]
    #mc_ate <- summary(est)$coefficients["d",1]
    
    
    # 2) Conditional Means via Ols -------------------------------------------------------------------------------------------------
    # same coefficients for confounders for both treatment states (1 OLS)
    est <- lm(outcome_cate~d_cate+x_cate)
    summary(est)
    
    ols_cond_means <- summary(est)
    OLS_ate <- summary(est)$coefficients["d_cate", ]
    #OLS_ate <- summary(est)$coefficients["d", 1] # remove after  I get sds for all estimates
    
    # flexible coefficients for confounders for treatment states (2 OLS)
    est <- lm(outcome_cate~ x_cate + d_cate*x_cate) # confounders and confounders*treatment as dummies
    summary(est)
    
    ols_cond_means_flex <- summary(est)
    OLS_flex_ate <- summary(est)$coefficients["d_cate", ]
    #OLS_flex_ate <- summary(est)$coefficients["d", 1] # remove after I get sds for all estimates
    
    
    # 3) Inverse Probability Weighting -------------------------------------------------------------------------------------------------
    # estimate propensity score via probit
    probit <- function(d, x){
      out <- glm(d~x, family = "binomial"(link = "probit"))
      return(as.matrix(out$fitted.values))
    }
    
    out <- glm(d_cate~x_cate, family = "binomial")
    summary(out)
    phat <- probit(d_cate, x_cate)
    
    # test if propensity score close to 0 / 1
    mean <- mean(phat)
    max <- max(phat)
    min <- min(phat)
    colname <- cbind("min", "mean", "max")
    desc_phat <- cbind(min, mean, max)
    hist(phat, breaks=100)
    
    # davia idea: remove obs with prop scores close to 0 / 1 according to boundries in hubner, lechner, wunsch (2013)
    ipw_data <- as.data.frame(cbind(outcome_cate, phat, d_cate, x_cate))
    colnames(ipw_data) <- c(outcome_name, "phat", "d_cate", x_name)
    count_obs_removed_help <- sum(!(ipw_data$phat < 0.95 & ipw_data$phat > 0.05))
    count_obs_removed_help2 <- sum(!(ipw_data$phat < 0.90 & ipw_data$phat > 0.10))
    help <- ipw_data[ipw_data$phat < 0.95 & ipw_data$phat > 0.05, ]
    help2 <- ipw_data[ipw_data$phat < 0.90 & ipw_data$phat > 0.10, ]
    
    print(paste("Removed", count_obs_removed_help ,"obs with min 5% distance from 0/1"))
    print(paste("Removed", count_obs_removed_help2 ,"obs with min 10% distance from 0/1"))
    
    # IPW for treatment effect
    IPW <- function(phat,y,d){
      ate <- mean((d*y)/phat - ((1-d)*y/(1-phat)))
      return(ate)
    }
    
    IPW_ate <- c(IPW(phat,outcome_cate,d_cate),NA,NA,NA) # ate_estimate, sd, t-val, p-val
    IPW_ate2 <- c(IPW(help$phat, help[,outcome_name], help$d_cate),NA,NA,NA)  # ate_estimate, sd, t-val, p-val
    IPW_ate3 <- c(IPW(help2$phat, help2[,outcome_name], help2$d_cate),NA,NA,NA) # ate_estimate, sd, t-val, p-val
    
    print("IPW Effects:")
    print(paste("Base:",IPW_ate[1]))
    print(paste("Help:",IPW_ate2[1]))
    print(paste("Help2:",IPW_ate3[1]))
    
    # todo: sd-errors. t-val, p-val
    
    # 4) Doubly Robust -------------------------------------------------------------------------------------------------------------------
    # use propensity scores as estimated above
    
    # estimate conditional means models with the 3 prop specifications (removing of obs)
    ipw_base_est <- lm(ipw_data[,outcome_name]~ as.matrix(ipw_data[,c("d_cate",x_name)])) # confounders and confounders*treatment as dummies
    ipw_help_est <- lm(help[,outcome_name]~ as.matrix(help[,c("d_cate",x_name)]))  # same for restricted data
    ipw_help2_est <- lm(help2[,outcome_name]~ as.matrix(help2[,c("d_cate",x_name)])) #same for restricted data
    
    # estimate ate
    doubly_robust <- function(yhat_d0, yhat_d1,phat,y,d){
      diff_yhat <- yhat_d1- yhat_d0 # difference conditional means
      frac_1 <- d*(y-yhat_d1)/phat 
      frac_2 <- (1-d)*(y-yhat_d0)/(1-phat)
      ate <- 1/length(y)*sum(diff_yhat+frac_1-frac_2)
      return(ate)
    }
    # base
    yhat_d0 <- ipw_base_est$fitted.values - ipw_data$d_cate*ipw_base_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- ipw_base_est$fitted.values + (1-ipw_data$d_cate)*ipw_base_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_base_ate <- c(doubly_robust(yhat_d0,
                                   yhat_d1,
                                   ipw_data$phat,
                                   ipw_data[,outcome_name],
                                   ipw_data$d_cate),
                     NA,NA,NA) # ate_estimate, sd, t-val, p-val
    
    # subsample help
    yhat_d0 <- ipw_help_est$fitted.values - help$d_cate*ipw_help_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- ipw_help_est$fitted.values + (1-help$d_cate)*ipw_help_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_help_ate <- c(doubly_robust(yhat_d0,
                                   yhat_d1,
                                   help$phat,
                                   help[,outcome_name],
                                   help$d_cate),
                     NA,NA,NA) # ate_estimate, sd, t-val, p-val
    
    # subsample help2
    yhat_d0 <- ipw_help2_est$fitted.values - help2$d_cate*ipw_help2_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- ipw_help2_est$fitted.values + (1-help2$d_cate)*ipw_help2_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_help2_ate <- c(doubly_robust(yhat_d0,
                                    yhat_d1,
                                    help2$phat,
                                    help2[,outcome_name],
                                    help2$d_cate),
                      NA,NA,NA)# ate_estimate, sd, t-val, p-val
    
    print("Double Robust Effects:")
    print(paste("Base:",DR_base_ate[1]))
    print(paste("Help:",DR_help_ate[1]))
    print(paste("Help2:",DR_help2_ate[1]))
    # ?? variance estimation : bootstrap or delta-method? -> not done yet, bootstrap would be possible to implement.
    # ?? pvalues: dont know its distribution
    
    
    # Comparison ---------------------------------------------------------------------------------------------------
    parametric_est <- cbind(mc_ate, OLS_ate, OLS_flex_ate,IPW_ate, IPW_ate2, IPW_ate3,DR_base_ate,DR_help_ate,DR_help2_ate)
    colnames(parametric_est) <- c("Mean Comparison", "OLS", "OLS_flex", "IPW", "IPW_restricted", "IPW_restricted2","Doubly_robust_base","Doubly_robust_restricted","Doubly_robust_restricted2")
    
    # Collect in list
    results_outcome[[iter_outcome_name]] = parametric_est
  }
  
  # overview table of only ATEs in latex
  cate_table_all_outcomes <- as.data.frame(t(results_outcome[[1]][1,]))
  
  for (i in 2:length(results_outcome)){
    cate_table_all_outcomes<- rbind(cate_table_all_outcomes,t(results_outcome[[i]][1,]))
  }
  rownames(cate_table_all_outcomes) <- names(results_outcome)
  
  # collect all cate results in table
  results_cate[[iter_cate]] <- cate_table_all_outcomes
}

# build cate table.
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
