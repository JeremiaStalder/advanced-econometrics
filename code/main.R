### Project Pension - Advanced Econometrics Methods, HSG

setwd("D:/GitHub/advanced-econometrics") # setwd
# setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics") # setwd
options(scipen=10000)
outpath <- "./output/" # output
outpath_des_before_trans <- "./output/descriptives/before_trans/"
outpath_des_after_trans <- "./output/descriptives/after_trans/"
outpath_des_paper <- "./output/descriptives/paper/"
datapath <- "./data/" # data files (input datafile from package)

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
library(plotly)
library(hexbin)

## functions
filter <- dplyr::filter
select <- dplyr::select
ggpairs <- GGally::ggpairs
ggcorrplot <- ggcorrplot::ggcorrplot
# source("functions.R") # functions

# balance check function 
balance_check <- function(vars, treat){

md = rep(0,ncol(vars))
md_se = rep(0,ncol(vars))
diff = rep(0,ncol(vars))

for (i in 1:ncol(vars)){
md[i] = mean(vars[,i][treat==1])-mean(vars[,i][treat==0])
md_se[i] = sqrt((var(vars[,i][treat==1])+var(vars[,i][treat==0]))/2)
diff[i] = md[i] / md_se[i]
}

res = cbind(md,md_se,diff,abs(diff)>0.1)
colnames(res) = c("mean_diff","estimated_sd","standardized_difference","relevant_difference")
rownames(res) = colnames(vars)
return(res)
}



####  0) Import Data ####
View(data("pension"))
mydata <- as.data.frame(pension)

#### 1) Data Overview (before transformations) ####
# define variable sets
colnames(mydata)
key_vars = c("p401", "e401", "a401", "tw")
financial_vars <-
  c("inc",
    "tfa",
    "tfa_he",
    "ira",
    "net_tfa",
    "nifa",
    "net_nifa",
    "net_n401")
other_controls_with_key_vars <-
  c(
    "age",
    "fsize",
    "marr",
    "db",
    "hown",
    "educ",
    "male",
    "twoearn",
    "hmort",
    "hequity",
    "hval"
  )

# descriptives
summary(mydata)
summary(is.na(mydata))

# Correlations
ggpairs(mydata[, key_vars]) +
  ggtitle(paste("Correlations Variables - Base Variables - before Trans"))
ggsave(
  file = paste0(
    outpath_des_before_trans,
    "corr_plot_key_vars_before_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

ggpairs(mydata[, c(key_vars, financial_vars)]) +
  ggtitle(paste("Correlations Variables - Financial Vars"))
ggsave(
  file = paste0(
    outpath_des_before_trans,
    "corr_plot_key_vars_before_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

ggcorrplot(cor(mydata[, c(key_vars, financial_vars, other_controls_with_key_vars)], method =
                 "pearson"), tl.cex = 6) +
  ggtitle(paste("Correlation Matrix - before trans"))
ggsave(
  file = paste0(
    outpath_des_before_trans,
    "corr_matrix_all_before_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

# Table:  Means / Medians conditional on eligibility
cond_mean_before_trans <- matrix(NA, nrow = ncol(mydata), ncol = 4)
for (i in 1:ncol(mydata)) {
  means <- by(data = mydata[, i],
              INDICES = mydata$e401,
              FUN = mean)
  medians <-
    by(data = mydata[, i],
       INDICES = mydata$e401,
       FUN = median)
  cond_mean_before_trans[i, 1] <- means[[1]]
  cond_mean_before_trans[i, 2] <- means[[2]]
  cond_mean_before_trans[i, 3] <- medians[[1]]
  cond_mean_before_trans[i, 4] <- medians[[2]]
}
colnames(cond_mean_before_trans) <-
  c("mean_non_eligible",
    "mean_eligible",
    "median_non_eligible",
    "median_eligible")
rownames(cond_mean_before_trans) <- colnames(mydata)
print(cond_mean_before_trans, digits = 3)
print(xtable(cond_mean_before_trans, title = "Conditional Means (before variable transformations", digits = 3), type="latex",paste0(outpath_des_before_trans, "cond_mean_before_trans.tex"))
save(
  cond_mean_before_trans,
  file = paste0(outpath_des_before_trans, "cond_means_table_before.Rdata")
)

# Histograms
for (i in 1:ncol(mydata)) {
  png(
    file = paste0(
      outpath_des_before_trans,
      "hist_before_trans_",
      colnames(mydata)[i],
      ".png"
    ),
    width = 600,
    height = 350
  )
  hist(mydata[, i], main = paste("Hist for", colnames(mydata)[i]))
  dev.off()
}

# Overlapping Histograms
for (i in 1:ncol(mydata)) {
  ggplot(mydata, aes_string(x = paste0(colnames(mydata)[i]))) +
    geom_histogram(data = subset(mydata, e401 == 0),
                   fill = "red",
                   alpha = 0.2) + #Red eligible not for 401
    geom_histogram(data = subset(mydata, e401 == 1),
                   fill = "blue",
                   alpha = 0.2) + #blue eligible  for 401
    ggsave(
      paste0(
        outpath_des_before_trans,
        "overlap_hist_before_trans_",
        colnames(mydata)[i],
        ".png"
      )
    )
}

#Boxplot
for (i in 1:ncol(mydata)) {
  png(
    file = paste0(
      outpath_des_before_trans,
      "boxplot_before_trans_",
      colnames(mydata)[i],
      ".png"
    ),
    width = 600,
    height = 350
  )
  boxplot(
    subset(mydata, e401 == 0)[, i],
    subset(mydata, e401 == 1)[, i],
    main = paste("Boxplot e401 = 0 or 1 and", colnames(mydata)[i]),
    at = c(1, 2),
    names = c("e401 = 0", "e401 = 1"),
    las = 2,
    horizontal = FALSE,
    notch = FALSE
  )
  dev.off()
}


## Plausibility Checks ##
# home value variables
eq_check <- -mydata$hmort + mydata$hval
print("Home Equity Check")
sum(eq_check == mydata$hequity)
sum(eq_check != mydata$hequity)

hown_check <- mydata$hval > 0
print("Home Owner Check")
sum(hown_check == mydata$hown)
sum(hown_check != mydata$hown)

# education dummies
educ_dummy_check <-
  mydata$col + mydata$smcol + mydata$hs + mydata$nohs
print("All education Dummys sum up to 1?")
mean(educ_dummy_check)

educ_dummy_check2 <- mydata$educ < 12
print("Non-High-School Dummy constructed via education years")
all(educ_dummy_check2 == mydata$nohs)

# ALL WEALTH VARIABLES:  hard to replicate
# tfa_he: tfa and housing equity
# Could not fully replicate variable. Do not use. Difference could be partly caused by right censoring of hval and hmort
diff_tfa = mydata$tfa - mydata$tfa_he + mydata$hequity
hist(diff_tfa)
sum(diff_tfa == 0)
summary(diff_tfa)
for (i in 1:ncol(mydata)) {
  print(paste("Corr diff and ", colnames(mydata)[i], cor(diff_tfa, mydata[, i])))
}
data_test = mydata_transform[mydata_transform$hval == max(mydata_transform$hval), ]
data_test = mydata_transform[mydata_transform$hmort == max(mydata_transform$hmort), ]
diff_tfa_test = data_test$tfa - data_test$tfa_he + data_test$hequity
hist(diff_tfa_test)
sum(diff_tfa_test == 0)
sum(diff_tfa_test != 0)

# tfa
# successful replication (apart from 80 obs)
tfa_check <- mydata$a401 + mydata$nifa + mydata$ira
sum(tfa_check == mydata$tfa)
sum(tfa_check != mydata$tfa)
(mydata$tfa - tfa_check)[tfa_check != mydata$tfa]

# net fin assets
# successful replication only for non-ira users (apart from 30 obs)
net_tfa_check <- mydata$a401 + mydata$net_nifa + mydata$ira
hist(net_tfa_check - mydata$net_tfa)
sum(net_tfa_check == mydata$net_tfa)
sum(net_tfa_check != mydata$net_tfa)
(mydata$net_tfa - net_tfa_check)[(net_tfa_check != mydata$net_tfa) &
                                   mydata$pira == 0] # works only for non-ira participants

# total wealth
# Cannot replicate. Missing variable value of other property, vehicles,... is residual.
tw_check  = mydata$net_tfa + mydata$hequity
summary(tw_check)
sum(tw_check == mydata$tw)
sum(tw_check != mydata$tw)
other_assets <-
  mydata$tw - mydata$net_tfa - mydata$hequity # residual variable: value of other property, vehicles,...
hist(other_assets)
summary(other_assets)


#### 2) Data Transformation ####
mydata_transform_work <- mydata
mydata_transform_work$tw_original <- mydata_transform_work$tw

## Remove outlier observations and invalid values
# drop observations with negative income (only 5 obs, same as benjamin (2003))
mydata_transform_work <- filter(mydata_transform_work, inc > 0)

# drop observations with negative home equity (only 27 obs) to avoid overfitting to outliers.
mydata_transform_work <- filter(mydata_transform_work, hequity >= 0)

#### Total wealth variable Outliers- Important ####
# Issue: negative outliers in tw will have impact on log transformation, interpretability and lead to overfitting of outliers
# Heuristic idea: since all asset values (not net asset values) are 0 / positive: remove obs with total wealth < -50000 + house equity. 50000 could be e.g. consumer loans, tax payments,...
# Result: removed 25 observations
mydata_transform_work <- filter(mydata_transform_work, tw > -(-hequity + 50000))

#### Remove Regions without common support ####
# income: no common support for <5000USD income (only e401=0). Lose ~350 observations
mydata_transform_work <- filter(mydata_transform_work, inc >7000)

## New variables
# Withdrawal without penalty variable (above age 59.5)
mydata_transform_work$withdrawal <-
  as.numeric(mydata_transform_work$age > 59.5)

# other assets as residual variable, includes: other property, vehicles, ...
mydata_transform_work$other_assets <-
  mydata_transform_work$tw - mydata_transform_work$net_tfa - mydata_transform_work$hequity

## Interaction terms
# Family variable
mydata_transform_work$working_couple <-
  as.numeric(mydata_transform_work$fsize == 2 &
               mydata_transform_work$twoearn == 1)
#### add more interaction terms! especially useful for lasso. ####


## Variable transformations
continuous_vars <-
  c(
    "a401",
    "tw",
    "tfa",
    "ira",
    "net_tfa",
    "nifa",
    "net_nifa",
    "net_n401",
    "inc",
    "hmort",
    "hequity",
    "hval",
    "other_assets",
    "tw_original",
    "educ",
    "age"
  ) # keep age and educ as last elements. not truely continuous and does not need to be logged. (so exlude for some operations)


# family size dummies
mydata_transform_work <-
  mutate(mydata_transform_work, fsize = case_when(as.double(fsize) > 5 ~ 5, TRUE ~ as.double(fsize))) # transform >5 familiy size to 5
fsize_dummies <-
  fastDummies::dummy_cols(mydata_transform_work$fsize, remove_first_dummy = T)[, 2:5] # dummies for family size. remove fist column which is original variable
colnames(fsize_dummies) <-
  c(paste0("fsize_", c(2, 3, 4, "5_or_above")))
mydata_transform_work <-
  cbind(mydata_transform_work, fsize_dummies)

# Dummies for cutoffs of continuous vars
# Dummies for left cutoff (0)
# check which vars have left cutoff:
# result: hmort, hval
# BUT: with removed negative equity variables _> hown = hval_dummy
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform_work[, continuous_vars[i]] == min(mydata_transform_work[, continuous_vars[i]])))
}
mydata_transform_work <-
  mutate(mydata_transform_work, hmort_dummy = as.numeric(hmort > 0)) # mortgage dummy


# Dummies for right cutoff
# check which vars have right cutoff:
# result: hmort, hval, hequity
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform_work[, continuous_vars[i]] == max(mydata_transform_work[, continuous_vars[i]])))
}
mydata_transform_work <-
  mutate(mydata_transform_work, hval_dummy_right_censored = as.numeric(max(hval) ==
                                                                         hval)) # house value dummy
mydata_transform_work <-
  mutate(mydata_transform_work, hmort_dummy_right_censored = as.numeric(max(hmort) ==
                                                                          hmort)) # mortgage dummy
mydata_transform_work <-
  mutate(mydata_transform_work,
         hequity_dummy_right_censored = as.numeric(max(hequity) == hequity)) # hequity dummy

# Wealth variable adjustments due to db following Benjamin (2003) (defined benefit plans. their value is not included in wealth variables)
# 1. replacement of preexisting db-plan: subtract 20% of 401k assets from total wealth & financial asset variables that include 401k
mydata_transform_work$tfa_adjust <-
  mydata_transform_work$tfa - mydata_transform_work$a401 * 0.2
mydata_transform_work$net_tfa_adjust <-
  mydata_transform_work$net_tfa - mydata_transform_work$a401 * 0.2
mydata_transform_work$tw_adjust <-
  mydata_transform_work$tw - mydata_transform_work$a401 * 0.2

# 2. marginal substitution effect: firms could reduce benefits of DB-plan if they instroduce 401k.Subtract income-related correction term from total wealth & financial asset variables that include 401k
wealth_correction_per_dollar_income <- 0.032
mydata_transform_work$tfa_adjust[mydata_transform_work$db &
                                   mydata_transform_work$e401] <-
  mydata_transform_work$tfa_adjust[mydata_transform_work$db &
                                     mydata_transform_work$e401] - mydata_transform_work$inc[mydata_transform_work$db &
                                                                                               mydata_transform_work$e401] * wealth_correction_per_dollar_income
mydata_transform_work$net_tfa_adjust[mydata_transform_work$db &
                                       mydata_transform_work$e401] <-
  mydata_transform_work$net_tfa_adjust[mydata_transform_work$db &
                                         mydata_transform_work$e401] - mydata_transform_work$inc[mydata_transform_work$db &
                                                                                                   mydata_transform_work$e401] * wealth_correction_per_dollar_income
mydata_transform_work$tw_adjust[mydata_transform_work$db &
                                  mydata_transform_work$e401] <-
  mydata_transform_work$tw_adjust[mydata_transform_work$db &
                                    mydata_transform_work$e401] - mydata_transform_work$inc[mydata_transform_work$db &
                                                                                              mydata_transform_work$e401] * wealth_correction_per_dollar_income
# create variables as "original" again, do not log-transform the original vars
mydata_transform_work$tfa_adjust_original <- mydata_transform_work$tfa_adjust
mydata_transform_work$net_tfa_adjust_original <- mydata_transform_work$net_tfa_adjust
mydata_transform_work$tw_adjust_original <- mydata_transform_work$tw_adjust

# add to continuous variable set
continuous_vars <-
  c(paste0(c("tfa", "net_tfa", "tw"), "_adjust"), continuous_vars,paste0(c("tfa", "net_tfa", "tw"), "_adjust_original")) # add variables to continuous. Add vars to transform in front (because last variables are excluded for log operation)




# Log monetary variables
# Note: if variable can be negative, we transform it to be positive ("move the minimum value") such that we can use logs. We move the minimum of the series to 1.
# !Attention: for variables with negative minimum (only home equity), the interpretability of the coefficient is killed.)
for (i in 1:(length(continuous_vars) - 6)) {
  # exclude tw_original, educ and age variable in this loop
  if (min(mydata_transform_work[, continuous_vars[i]]) <= 1) {
    mydata_transform_work[, continuous_vars[i]] <-
      log(mydata_transform_work[, continuous_vars[i]] - min(mydata_transform_work[, continuous_vars[i]]) +
            1)
  }
  else {
    mydata_transform_work[, continuous_vars[i]] <-
      log(mydata_transform_work[, continuous_vars[i]])
  }
}


# polynomials: (var-mean(var))^n
# age
mydata_transform_work$age_sq <-
  (mydata_transform_work$age - mean(mydata_transform_work$age)) ^ 2
mydata_transform_work$age_cub <-
  (mydata_transform_work$age - mean(mydata_transform_work$age)) ^ 3

# income
mydata_transform_work$inc_sq <-
  (mydata_transform_work$inc - mean(mydata_transform_work$inc)) ^ 2
mydata_transform_work$inc_cub <-
  (mydata_transform_work$inc - mean(mydata_transform_work$inc)) ^ 3

# wealth outcome variables in quantiles (doesnt matter if before or after standardizing)
for (i in 1:(length(continuous_vars) - 6)) {
  mydata_transform_work <-
    cbind(mydata_transform_work,
          ecdf(mydata_transform_work[, continuous_vars[i]])(mydata_transform_work[, continuous_vars[i]])) # function for empirical quantile
  colnames(mydata_transform_work)[ncol(mydata_transform_work)] <-
    paste0(continuous_vars[i], "_quantile")
}

## Drop Variables
mydata_transform_work <-
  select(mydata_transform_work, -c(zhat, dum91, icat, ecat, tfa_he))    # remove unnecessary variables
mydata_transform_work <-
  select(mydata_transform_work, -c(nohs, a1, i1))  # remove first age category, income category and no high school dummy as reference category
# Not necesary: remove age and income dummies used in paper. Currently not done because we can verify if we get similar results using their dummies.
# mydata_transform_work <-
#   select(mydata_transform_work,-paste0("a", seq(2, 5))) # age dummies
# mydata_transform_work <-
#   select(mydata_transform_work,-paste0("i", seq(2, 7))) # income dummies

## Standardize variables.
# ! Hint: need to do all "content-related" variable transformations before this step.
# ! Hint 2: for lasso etc, also binary variables need to be standardized. thats why we do it here
standardized_vars <- scale(mydata_transform_work)
colnames(standardized_vars) <-
  paste0(colnames(mydata_transform_work), "_std")
mydata_transform_work <-
  cbind(mydata_transform_work, standardized_vars)

# add cate variables ######
mydata_transform_work <- mutate(mydata_transform_work, inc_quintile =ntile(inc,5)) 

# rename transform data
mydata_transform <- mydata_transform_work

## Save transformed data
save(mydata_transform, file = paste0(outpath, "mydata_transform.Rdata")) 


#### 3) Define Variable Sets ####
# dependent variables
dependent_vars <-
  c(
    "tw_adjust",
    "net_tfa_adjust",
    "net_nifa",
    "tw",
    "tw_original",
    "tfa",
    "ira",
    "net_tfa",
    "nifa",
    "net_n401",
    "tfa_adjust",
   "tw_adjust_original",
   "net_tfa_adjust_original",
   "tfa_adjust_original"
  )
dependent_vars_std <- paste0(dependent_vars, "_std")
dependent_vars_quantile <- paste0(dependent_vars, "_quantile")
dependent_vars_selection <-
  c("tw_adjust_original", "tw_adjust_quantile")

# cate variables
cate_vars <- c("inc_quintile")

# independent variables
non_used_wealth_variables <- c("a401", "hval", "hmort", "hequity","hval_quantile","a401_quantile","hmort_quantile","hequity_quantile","hval_quantile","other_assets_quantile")
independent_vars_temp <-
  colnames(mydata_transform)[!grepl("_std", colnames(mydata_transform), TRUE)]
independent_vars <-
  independent_vars_temp[!(independent_vars_temp %in% dependent_vars) &
                          !(independent_vars_temp %in% dependent_vars_quantile) & 
                          !(independent_vars_temp %in% dependent_vars_std) & 
                          !(independent_vars_temp %in% non_used_wealth_variables)]
independent_vars_std <-  paste0(independent_vars, "_std")

independent_vars_selection <-
  c(
    "e401",
    "age",
    "inc",
    "db",
    "marr",
    "male",
    "twoearn",
    "pira",
    "hs",
    "smcol",
    "col",
    "hown",
    "withdrawal",
    "age_sq",
    "age_cub",
    "inc_sq" ,
    "inc_cub"
  )
independent_vars_selection_std = paste0(independent_vars_selection, "_std")

dependent_vars_benjamin <-
  c("tw", "tw_original", "tfa", "net_nifa") # do not know if benjamin used logs
dependent_vars_benjamin_std <-
  paste0(dependent_vars_benjamin, "_std")
independent_vars_benjamin <-
  c(
    "e401",
    "p401",
    "fsize",
    "ira",
    "pira",
    "hown",
    "hs",
    "smcol",
    "col",
    "marr",
    "twoearn",
    "db",
    paste0("a", seq(
      from = 2, to = 5, by = 1
    )),
    paste0("i", seq(
      from = 2, to = 7, by = 1
    ))
  ) # do not know if benjamin used logs
independent_vars_benjamin_std <-
  paste0(independent_vars_benjamin, "_std")

# collect sets and save as list
  # first list: for descriptives, including p401
  variable_sets_descriptives <-
    list(
      dependent_vars,
      dependent_vars_std,
      dependent_vars_quantile,
      dependent_vars_selection,
      non_used_wealth_variables,
      independent_vars,
      independent_vars_std,
      independent_vars_selection,
      independent_vars_selection_std,
      dependent_vars_benjamin,
      dependent_vars_benjamin_std,
      independent_vars_benjamin,
      independent_vars_benjamin_std,
      cate_vars
    )
  names(variable_sets_descriptives) <-
    c(
      "dependent_vars",
      "dependent_vars_std",
      "dependent_vars_quantile",
      "dependent_vars_selection",
      "non_used_wealth_variables",
      "independent_vars",
      "independent_vars_std",
      "independent_vars_selection",
      "independent_vars_selection_std",
      "dependent_vars_benjamin",
      "dependent_vars_benjamin_std",
      "independent_vars_benjamin",
      "independent_vars_benjamin_std",
      "cate_vars"
    )
  
  # second list: for models, excluding p401
  variable_sets_modelling <- variable_sets_descriptives
  for (i in 1:length(variable_sets_descriptives)){
    variable_sets_modelling[[i]] =  variable_sets_modelling[[i]][!grepl("p401", variable_sets_modelling[[i]])] # remove p401 variables
  }
  

## save dataset
save(variable_sets_modelling, file = paste0(outpath, "variable_sets_modelling.Rdata")) # save list for modelling
save(variable_sets_descriptives, file = paste0(outpath, "variable_sets_descriptives.Rdata")) # save list for descriptives

#### 4) Descriptives after Transformations ####

summary(mydata_transform)
summary(is.na(mydata_transform))

# Correlations
ggpairs(mydata_transform[, c(dependent_vars_selection, "e401", "p401")]) +
  ggtitle(paste("Correlations Variables - Base Variables - After Trans"))
ggsave(
  file = paste0(
    outpath_des_after_trans,
    "corr_plot_key_vars_after_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

ggpairs(mydata_transform[, c(dependent_vars_benjamin, independent_vars_benjamin)]) +
  ggtitle(paste("Correlations Variables - Benjamin Set Vars - After Trans"))
ggsave(
  file = paste0(
    outpath_des_after_trans,
    "corr_plot_key_vars_after_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

ggcorrplot(cor(mydata_transform[, c(dependent_vars_selection, independent_vars_selection)], method =
                 "pearson"), tl.cex = 6) +
  ggtitle(paste("Correlation Matrix - after trans"))
ggsave(
  file = paste0(
    outpath_des_after_trans,
    "corr_matrix_all_after_trans",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 300
)

# Table:  Means / Medians conditional on eligibility
cond_mean_after_trans <-
  matrix(NA, nrow = ncol(mydata_transform), ncol = 4)
for (i in 1:ncol(mydata_transform)) {
  means <-
    by(data = mydata_transform[, i],
       INDICES = mydata_transform$e401,
       FUN = mean)
  medians <-
    by(data = mydata_transform[, i],
       INDICES = mydata_transform$e401,
       FUN = median)
  cond_mean_after_trans[i, 1] <- means[[1]]
  cond_mean_after_trans[i, 2] <- means[[2]]
  cond_mean_after_trans[i, 3] <- medians[[1]]
  cond_mean_after_trans[i, 4] <- medians[[2]]
}
colnames(cond_mean_after_trans) <-
  c("mean_non_eligible",
    "mean_eligible",
    "median_non_eligible",
    "median_eligible")
rownames(cond_mean_after_trans) <- colnames(mydata_transform)
print(cond_mean_after_trans, digits = 3)
print(xtable(cond_mean_after_trans, title = "Conditional Means (after variable transformations", digits = 3), type="latex",paste0(outpath_des_after_trans, "cond_mean_after_trans.tex"))
save(
  cond_mean_after_trans,
  file = paste0(
    outpath_des_after_trans,
    "cond_means_table_after.Rdata"
  )
)

# Difference in Means? -  Use standardized difference (balance check procedure)
standardized_difference <- as.data.frame(balance_check(mydata_transform,mydata_transform$e401)) %>%
  arrange(desc(abs(standardized_difference)))

save(standardized_difference, file= paste0(outpath_des_after_trans, "standardized_difference.Rdata"))
print(xtable(standardized_difference, title = "Standardized Difference (after variable transformations", digits = 3))

# Histograms
for (i in 1:ncol(mydata_transform)) {
  png(
    file = paste0(
      outpath_des_after_trans,
      "hist_after_trans_",
      colnames(mydata_transform)[i],
      ".png"
    ),
    width = 600,
    height = 350
  )
  hist(mydata_transform[, i], main = paste("Hist for", colnames(mydata_transform)[i]))
  dev.off()
}

# Overlapping Histograms
for (i in 1:ncol(mydata_transform)) {
  ggplot(mydata_transform, aes_string(x = paste0(colnames(mydata_transform)[i]))) +
    geom_histogram(
      data = subset(mydata_transform, e401 == 0),
      fill = "red",
      alpha = 0.2
    ) + #Red eligible not for 401
    geom_histogram(
      data = subset(mydata_transform, e401 == 1),
      fill = "blue",
      alpha = 0.2
    ) + #blue eligible  for 401
    ggsave(
      paste0(
        outpath_des_after_trans,
        "overlap_hist_after_trans_",
        colnames(mydata_transform)[i],
        ".png"
      )
    )
}

# Boxplot
for (i in 1:ncol(mydata_transform)) {
  png(
    file = paste0(
      outpath_des_after_trans,
      "boxplot_after_trans_",
      colnames(mydata_transform)[i],
      ".png"
    ),
    width = 600,
    height = 350
    
  )
  boxplot(
    subset(mydata_transform, e401 == 0)[, i],
    subset(mydata_transform, e401 == 1)[, i],
    main = paste("Boxplot e401 = 0 or 1 and", colnames(mydata_transform)[i]),
    at = c(1, 2),
    names = c("e401 = 0", "e401 = 1"),
    las = 2,
    horizontal = FALSE,
    notch = FALSE
  )
  dev.off()
}

##### 5) Pretty Descriptives for Paper #####
# summary table: descriptives full sample & for eligiblity groups, balance check for key variables.
# table descriptives for paper 
# vars to use
vars_descriptive_table = c("e401","p401",
                           "tw_adjust_original","net_tfa_adjust_original","net_nifa",
                           "age","inc","db","ira", "hequity","educ", "fsize","marr","male","twoearn","withdrawal")
names_vars_descriptive_table = c("Eligibility","Participation",
                                 "Total Wealth","Net Financial Assets","Net Non-401k Financial Assets",
                                 "Age","Income","Defined Benefit Participation","IRA Account","Home Equity","Years Education", "Family Size", "Married","Male","Two Earners","Withdrawal w.o. Cost")

# data for descriptives for tweaks
mydata_transform_descriptive <- mydata_transform[,vars_descriptive_table]
# revert some log transformations for descriptives
  mydata_transform_descriptive$ira <- exp(mydata_transform_descriptive$ira)-1
  mydata_transform_descriptive$hequity <- exp(mydata_transform_descriptive$hequity)-1
  mydata_transform_descriptive$net_nifa <- exp(mydata_transform_descriptive$net_nifa)-1
  mydata_transform_descriptive$inc <- exp(mydata_transform_descriptive$inc)-1

standardized_difference <- as.data.frame(balance_check(mydata_transform_descriptive,mydata_transform_descriptive$e401)) # balance check procedure

table_descriptives_paper <- cbind(apply(mydata_transform_descriptive, 2, mean), 
                                  apply(mydata_transform_descriptive, 2, median),
                                  apply(mydata_transform_descriptive, 2, sd),
                                  apply(mydata_transform_descriptive[mydata_transform_descriptive$e401==0,], 2, mean),
                                  apply(mydata_transform_descriptive[mydata_transform_descriptive$e401==1,], 2, mean),
                                  standardized_difference$standardized_difference) # generate table with: mean, median, sd, group means (e=0, e=1) and std difference
colnames(table_descriptives_paper) <- c("Mean","Median","St.-Dev.","Mean - Non-Eligible","Mean - Eligible","Std. Diff. Means")
rownames(table_descriptives_paper) <- names_vars_descriptive_table


save(table_descriptives_paper, file= paste0(outpath_des_after_trans, "table_descriptives_paper.Rdata"))
print(xtable(table_descriptives_paper,title = "Descriptive Statistics", digits = 2), type="latex",paste0(outpath_des_after_trans, "table_descriptives_paper.tex"))


# Scatter Plots Income / Wealth with color for treatment
mydata_transform_scatter <- mydata_transform_descriptive
#mydata_transform_scatter <- mydata_transform_descriptive

ggplot(mydata_transform_scatter, aes(x=inc/1000, y=tw_adjust_original/1000, z=e401))+
  stat_summary_hex()+
  labs(x = "Income in TUSD")+
  labs(y = "Total Wealth in TUSD")+
  labs(fill = "Share Eligible")+
  theme_bw()+
  theme(legend.text=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))
ggsave(
  paste0(
    outpath_des_paper,
    "3d_scatter_income_wealth_eligibility",
    ".png"
  )
)

# correlation matrix for key variables
# use transformed dataset (to show correlations)
# vars to use
vars_descriptive_corrplot = c("e401","p401",
                           "tw_adjust","net_tfa_adjust","net_nifa",
                           "age","inc","db","pira", "hown","educ", "fsize","marr","male","twoearn","withdrawal")
names_vars_descriptive_corrplot = c("Eligibility","Participation",
                                 "Total Wealth","Net Financial Assets","Net Non-401k Financial Assets",
                                 "Age","Income","Defined Benefit Participation","IRA Participation","Home Owner","Years Education", "Family Size", "Married","Male","Two Earners","Withdrawal w.o. Cost")

mydata_transform_corrplot_paper <- mydata_transform[,vars_descriptive_corrplot]
colnames(mydata_transform_corrplot_paper) <- names_vars_descriptive_corrplot
ggcorrplot(cor(mydata_transform_corrplot_paper, method =
                 "pearson"), tl.cex = 6)
ggsave(
  file = paste0(
    outpath_des_paper,
    "corr_matrix_paper",
    ".png"
  ),
  width = 6,
  height = 4,
  dpi = 1200
)

# Income: overlap plots before and after transformation
number_bins <- 50
# BEFORE transformation
mydata_transform_overlap_paper_before <- mydata %>%
  mutate(e401 = ifelse(e401==1, "Eligible","Non-Eligible"))

ggplot(mydata_transform_overlap_paper_before, aes(x = inc, fill = e401)) +
  geom_histogram(aes(y=..count..),
                 color="black",
                 position="identity",
                 alpha = 0.5, 
                 bins = number_bins)+
  geom_vline(aes(xintercept=mean(inc)), color="black", linetype="dashed", size=1)+
  labs(x = "Income in USD")+
  labs(y = "Frequency")+
  labs(fill = "")+
  theme_bw()+
  theme(legend.position = c(0.8, 0.5), legend.text=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))
ggsave(
  paste0(
    outpath_des_paper,
    "overlap_hist_income_before_paper",
    ".png"
  )
)




# AFTER transformation
mydata_transform_overlap_paper_after <- mydata_transform %>%
  mutate(inc_original = exp(inc)) %>% # maybe display non-logged income
  mutate(e401 = ifelse(e401==1, "Eligible","Non-Eligible"))

ggplot(mydata_transform_overlap_paper_after, aes(x = inc, fill = e401)) +
  geom_histogram(aes(y=..count..),
                 color="black",
                 position="identity",
                 alpha = 0.5, 
                 bins = number_bins)+
  geom_vline(aes(xintercept=mean(inc)), color="black", linetype="dashed", size=1)+
  labs(x = "Income in USD- Logged")+
  labs(y = "Frequency")+
  labs(fill = "")+
  theme_bw()+
  theme(legend.position = c(0.8, 0.5),legend.text=element_text(size=12))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))
ggsave(
  paste0(
    outpath_des_paper,
    "overlap_hist_income_after_paper",
    ".png"
  )
)
