# Functions - Parametric Estimation 

# probit
probit <- function(d, x){
  out <- glm(d~x, family = "binomial"(link = "probit"))
  return(as.matrix(out$fitted.values))
}

# IPW for treatment effect
IPW <- function(phat,y,d){
  ate <- mean((d*y)/phat - ((1-d)*y/(1-phat)))
  return(ate)
}

# Wrapper function: returns IPW ATE estimate for full and restricted samples.
IPW_all_samples <- function(IPW_input_fct,x_name){
  # split up data inputs
  outcome = IPW_input_fct$outcome
  x = IPW_input_fct$x
  d = IPW_input_fct$d

  # estimate propensity score with probit
  out <- glm(d~x, family = "binomial")
  summary(out)
  phat <- probit(d, x)
  
  # test if propensity score close to 0 / 1
  mean <- mean(phat)
  max <- max(phat)
  min <- min(phat)
  colname <- cbind("min", "mean", "max")
  desc_phat <- cbind(min, mean, max)
  # print(hist(phat, breaks=100))
  
  # davia idea: remove obs with prop scores close to 0 / 1 according to boundries in hubner, lechner, wunsch (2013)
  ipw_data <- as.data.frame(cbind(outcome, phat, d, x))
  colnames(ipw_data) <- c(outcome_name, "phat", "d", x_name)
  count_obs_removed_help <- sum(!(ipw_data$phat < 0.95 & ipw_data$phat > 0.05))
  count_obs_removed_help2 <- sum(!(ipw_data$phat < 0.90 & ipw_data$phat > 0.10))
  help <- ipw_data[ipw_data$phat < 0.95 & ipw_data$phat > 0.05, ]
  help2 <- ipw_data[ipw_data$phat < 0.90 & ipw_data$phat > 0.10, ]
  
  # print(paste("Removed", count_obs_removed_help ,"obs with min 5% distance from 0/1"))
  # print(paste("Removed", count_obs_removed_help2 ,"obs with min 10% distance from 0/1"))
  
  #format outputs
    # IPW estimates
    IPW_estimates_fct <- as.data.frame(t(c(IPW(phat,outcome,d),IPW(help$phat, help[,outcome_name], help$d),IPW(help2$phat, help2[,outcome_name], help2$d))))
    colnames(IPW_estimates_fct) <- c("Base","Restricted","Restricted2")
    
    # Used datasets (because they are also used by DR-estimation)
    data_ipw <- list(ipw_data, help, help2)
    names(data_ipw) <- c("Base","Restricted","Restricted2")
    
    # list for all fct outputs
    IPW_output_fct <- list(IPW_estimates_fct, data_ipw)
    names(IPW_output_fct) <- c("IPW_estimates_fct","data_ipw")
  
  # print("IPW Effects:")
  # print(paste("Base:",IPW_estimates_fct$Base))
  # print(paste("Restricted:",IPW_estimates_fct$Restricted))
  # print(paste("Restricted2:",IPW_estimates_fct$Restricted2))
  
  
  return(IPW_output_fct)
}

# function to estimate DR-ATE given conditional outcome regression and propensity scores
doubly_robust <- function(yhat_d0, yhat_d1,phat,y,d){
  diff_yhat <- yhat_d1- yhat_d0 # difference conditional means
  frac_1 <- d*(y-yhat_d1)/phat 
  frac_2 <- (1-d)*(y-yhat_d0)/(1-phat)
  ate <- 1/length(y)*sum(diff_yhat+frac_1-frac_2)
  return(ate)
}

# Wrapper Function to get DR-ATEs for all samples. Input: propensity scores & data for all data set versions (all, restricted, restricted2)
DR_all_samples <- function(input_data_list, x_name,outcome_name){
  #input data list to names in fct
  data_ipw_base = input_data_list$data_ipw$Base
  data_ipw_restricted = input_data_list$data_ipw$Restricted
  data_ipw_restricted2 = input_data_list$data_ipw$Restricted2
 
  # estimate conditional means models with the 3 prop specifications (removing of obs)
  outcome_reg_base_est <- lm(data_ipw_base[,outcome_name]~ as.matrix(data_ipw_base[,c("d",x_name)])) # confounders and confounders*treatment as dummies
  outcome_reg_restricted_est <- lm(data_ipw_restricted[,outcome_name]~ as.matrix(data_ipw_restricted[,c("d",x_name)]))  # same for restricted data
  outcome_reg_restricted2_est <- lm(data_ipw_restricted2[,outcome_name]~ as.matrix(data_ipw_restricted2[,c("d",x_name)])) #same for restricted data
  
  # estimate ate
    # base
    yhat_d0 <- outcome_reg_base_est$fitted.values - data_ipw_base$d*outcome_reg_base_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- outcome_reg_base_est$fitted.values + (1-data_ipw_base$d)*outcome_reg_base_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_base_ate <- doubly_robust(yhat_d0,
                                 yhat_d1,
                                 data_ipw_base$phat,
                                 data_ipw_base[,outcome_name],
                                 data_ipw_base$d)
    
    # subsample Restricted
    yhat_d0 <- outcome_reg_restricted_est$fitted.values - data_ipw_restricted$d*outcome_reg_restricted_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- outcome_reg_restricted_est$fitted.values + (1-data_ipw_restricted$d)*outcome_reg_restricted_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_restricted_ate <- doubly_robust(yhat_d0,
                                       yhat_d1,
                                       data_ipw_restricted$phat,
                                       data_ipw_restricted[,outcome_name],
                                       data_ipw_restricted$d)
    
    # subsample Restricted2
    yhat_d0 <- outcome_reg_restricted2_est$fitted.values - data_ipw_restricted2$d*outcome_reg_restricted2_est$coefficients[2]    # yhat_d0 is the potential estimated value with treatment = 0 even if treatment was 1: added estimated coef for obs with treatment = 1
    yhat_d1 <- outcome_reg_restricted2_est$fitted.values + (1-data_ipw_restricted2$d)*outcome_reg_restricted2_est$coefficients[2]    # yhat_d1 is the potential estimated value with treatment = 1 even if treatment was 0: added estimated coef for obs with treatment = 0
    DR_restricted2_ate <- doubly_robust(yhat_d0,
                                        yhat_d1,
                                        data_ipw_restricted2$phat,
                                        data_ipw_restricted2[,outcome_name],
                                        data_ipw_restricted2$d)
    
  #format outputs
  # DR estimates
  DR_estimates_fct <- as.data.frame(t(c(DR_base_ate,DR_restricted_ate,DR_restricted2_ate)))
  colnames(DR_estimates_fct) <- c("Base","Restricted","Restricted2")
  
  # print("Double Robust Effects:")
  # print(paste("Base:",DR_estimates_fct$Base))
  # print(paste("Restricted:",DR_estimates_fct$Restricted))
  # print(paste("Restricted2:",DR_estimates_fct$Restricted2))
  
  return(DR_estimates_fct)
}