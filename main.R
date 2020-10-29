### Project Pension - Advanced Econometrics Methods, HSG

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
library(nonrandom)

## functions
filter <- dplyr::filter
select <- dplyr::select
ggpairs <- GGally::ggpairs
ggcorrplot <- ggcorrplot::ggcorrplot

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

# setwd("D:/GitHub/advanced-econometrics") # setwd
# setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics") # setwd
# source("functions.R") # functions


outpath <- "./output/" # output
outpath_des_before_trans <- "./output/descriptives/before_trans/"
outpath_des_after_trans <- "./output/descriptives/after_trans/"
datapath <- "./data/" # data files (input datafile from package)

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
xtable(cond_mean_before_trans, title = "Conditional Means (before variable transformations", digits = 3)
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
mydata_transform <- mydata
mydata_transform$tw_original <- mydata_transform$tw

## Remove outlier observations and invalid values
# drop observations with negative income (only 5 obs, same as benjamin (2003))
mydata_transform <- filter(mydata_transform, inc > 0)

# drop observations with negative home equity (only 27 obs) to avoid overfitting to outliers.
mydata_transform <- filter(mydata_transform, hequity >= 0)

#### Total wealth variable Outliers- Important ####
# Issue: negative outliers in tw will have impact on log transformation, interpretability and lead to overfitting of outliers
# Heuristic idea: since all asset values (not net asset values) are 0 / positive: remove obs with total wealth < -50000 + house equity. 50000 could be e.g. consumer loans, tax payments,...
# Result: removed 25 observations
mydata_transform <- filter(mydata_transform, tw > -(-hequity + 50000))

#### Remove Regions without common support ####
# income: no common support for <5000USD income (only e401=0). Lose ~350 observations
mydata_transform <- filter(mydata_transform, inc >7000)

## New variables
# Withdrawal without penalty variable (above age 59.5)
mydata_transform$withdrawal <-
  as.numeric(mydata_transform$age > 59.5)

# other assets as residual variable, includes: other property, vehicles, ...
mydata_transform$other_assets <-
  mydata_transform$tw - mydata_transform$net_tfa - mydata_transform$hequity

## Interaction terms
# Family variable
mydata_transform$busy_couple <-
  as.numeric(mydata_transform$fsize == 2 &
               mydata_transform$twoearn == 1)
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
mydata_transform <-
  mutate(mydata_transform, fsize = case_when(as.double(fsize) > 5 ~ 5, TRUE ~ as.double(fsize))) # transform >5 familiy size to 5
fsize_dummies <-
  fastDummies::dummy_cols(mydata_transform$fsize, remove_first_dummy = T)[, 2:5] # dummies for family size. remove fist column which is original variable
colnames(fsize_dummies) <-
  c(paste0("fsize_", c(2, 3, 4, "5_or_above")))
mydata_transform <-
  cbind(mydata_transform, fsize_dummies)

# Dummies for cutoffs of continuous vars
# Dummies for left cutoff (0)
# check which vars have left cutoff:
# result: hmort, hval
# BUT: with removed negative equity variables _> hown = hval_dummy
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform[, continuous_vars[i]] == min(mydata_transform[, continuous_vars[i]])))
}
mydata_transform <-
  mutate(mydata_transform, hmort_dummy = as.numeric(hmort > 0)) # mortgage dummy


# Dummies for right cutoff
# check which vars have right cutoff:
# result: hmort, hval, hequity
print("To determine left cutoff (apart from hist): number of obs equal to minumum of variable")
for (i in 1:length(continuous_vars)) {
  print(continuous_vars[i])
  print(sum(mydata_transform[, continuous_vars[i]] == max(mydata_transform[, continuous_vars[i]])))
}
mydata_transform <-
  mutate(mydata_transform, hval_dummy_right_censored = as.numeric(max(hval) ==
                                                                    hval)) # house value dummy
mydata_transform <-
  mutate(mydata_transform, hmort_dummy_right_censored = as.numeric(max(hmort) ==
                                                                     hmort)) # mortgage dummy
mydata_transform <-
  mutate(mydata_transform,
         hequity_dummy_right_censored = as.numeric(max(hequity) == hequity)) # hequity dummy

# Wealth variable adjustments due to db following Benjamin (2003) (defined benefit plans. their value is not included in wealth variables)
# 1. replacement of preexisting db-plan: subtract 20% of 401k assets from total wealth & financial asset variables that include 401k
mydata_transform$tfa_adjust <-
  mydata_transform$tfa - mydata_transform$a401 * 0.2
mydata_transform$net_tfa_adjust <-
  mydata_transform$net_tfa - mydata_transform$a401 * 0.2
mydata_transform$tw_adjust <-
  mydata_transform$tw - mydata_transform$a401 * 0.2

# 2. marginal substitution effect: firms could reduce benefits of DB-plan if they instroduce 401k.Subtract income-related correction term from total wealth & financial asset variables that include 401k
wealth_correction_per_dollar_income <- 0.032
mydata_transform$tfa_adjust[mydata_transform$db &
                              mydata_transform$e401] <-
  mydata_transform$tfa_adjust[mydata_transform$db &
                                mydata_transform$e401] - mydata_transform$inc[mydata_transform$db &
                                                                                mydata_transform$e401] * wealth_correction_per_dollar_income
mydata_transform$net_tfa_adjust[mydata_transform$db &
                                  mydata_transform$e401] <-
  mydata_transform$net_tfa_adjust[mydata_transform$db &
                                    mydata_transform$e401] - mydata_transform$inc[mydata_transform$db &
                                                                                    mydata_transform$e401] * wealth_correction_per_dollar_income
mydata_transform$tw_adjust[mydata_transform$db &
                             mydata_transform$e401] <-
  mydata_transform$tw_adjust[mydata_transform$db &
                               mydata_transform$e401] - mydata_transform$inc[mydata_transform$db &
                                                                               mydata_transform$e401] * wealth_correction_per_dollar_income

continuous_vars <-
  c(paste0(c("tfa", "net_tfa", "tw"), "_adjust"), continuous_vars) # add variables to continuous. Add in front (because last variables are excluded for log operation)


# Log monetary variables
# Note: if variable can be negative, we transform it to be positive ("move the minimum value") such that we can use logs. We move the minimum of the series to 1.
# !Attention: for variables with negative minimum (only home equity), the interpretability of the coefficient is killed.)
for (i in 1:(length(continuous_vars) - 3)) {
  # exclude tw_original, educ and age variable in this loop
  if (min(mydata_transform[, continuous_vars[i]]) <= 1) {
    mydata_transform[, continuous_vars[i]] <-
      log(mydata_transform[, continuous_vars[i]] - min(mydata_transform[, continuous_vars[i]]) +
            1)
  }
  else {
    mydata_transform[, continuous_vars[i]] <-
      log(mydata_transform[, continuous_vars[i]])
  }
}


# polynomials: (var-mean(var))^n
# age
mydata_transform$age_sq <-
  (mydata_transform$age - mean(mydata_transform$age)) ^ 2
mydata_transform$age_cub <-
  (mydata_transform$age - mean(mydata_transform$age)) ^ 3

# income
mydata_transform$inc_sq <-
  (mydata_transform$inc - mean(mydata_transform$inc)) ^ 2
mydata_transform$inc_cub <-
  (mydata_transform$inc - mean(mydata_transform$inc)) ^ 3

# wealth outcome variables in quantiles (doesnt matter if before or after standardizing)
for (i in 1:(length(continuous_vars) - 3)) {
  mydata_transform <-
    cbind(mydata_transform,
          ecdf(mydata_transform[, continuous_vars[i]])(mydata_transform[, continuous_vars[i]])) # function for empirical quantile
  colnames(mydata_transform)[ncol(mydata_transform)] <-
    paste0(continuous_vars[i], "_quantile")
}

## Drop Variables
mydata_transform <-
  select(mydata_transform, -c(zhat, dum91, icat, ecat, tfa_he))    # remove unnecessary variables
mydata_transform <-
  select(mydata_transform, -c(nohs, a1, i1))  # remove first age category, income category and no high school dummy as reference category
# Not necesary: remove age and income dummies used in paper. Currently not done because we can verify if we get similar results using their dummies.
# mydata_transform <-
#   select(mydata_transform,-paste0("a", seq(2, 5))) # age dummies
# mydata_transform <-
#   select(mydata_transform,-paste0("i", seq(2, 7))) # income dummies

## Standardize variables.
# ! Hint: need to do all "content-related" variable transformations before this step.
# ! Hint 2: for lasso etc, also binary variables need to be standardized. thats why we do it here
standardized_vars <- scale(mydata_transform)
colnames(standardized_vars) <-
  paste0(colnames(mydata_transform), "_std")
mydata_transform <-
  cbind(mydata_transform, standardized_vars)

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
    "tfa_adjust"
  )
dependent_vars_std <- paste0(dependent_vars, "_std")
dependent_vars_quantile <- paste0(dependent_vars, "_quantile")
dependent_vars_selection <-
  c("tw_adjust", "net_tfa_adjust", "net_nifa", "tw_original")
dependent_vars_selection_std <-
  paste0(dependent_vars_selection, "_std")
dependent_vars_selection_quantile <-
  paste0(dependent_vars_selection[dependent_vars_selection != "tw_original"], "_quantile")

# independent variables
non_used_wealth_variables <- c("a401", "hval", "hmort", "hequity")
independent_vars_temp <-
  colnames(mydata_transform)[!grepl("_std", colnames(mydata_transform), TRUE)]
independent_vars <-
  independent_vars_temp[!(independent_vars_temp %in% dependent_vars) &
                          !(independent_vars_temp %in% non_used_wealth_variables)]
independent_vars_std <-  paste0(independent_vars, "_std")

independent_vars_selection <-
  c(
    "e401",
    "p401",
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
    "fsize_2",
    "fsize_3",
    "fsize_4",
    "fsize_5_or_above",
    "withdrawal",
    "busy_couple",
    "hmort_dummy",
    "hval_dummy_right_censored",
    "hmort_dummy_right_censored",
    "hequity_dummy_right_censored",
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
variable_sets <-
  list(
    dependent_vars,
    dependent_vars_std,
    dependent_vars_selection,
    dependent_vars_selection_std,
    non_used_wealth_variables,
    independent_vars,
    independent_vars_std,
    independent_vars_selection,
    independent_vars_selection_std,
    dependent_vars_benjamin,
    dependent_vars_benjamin_std,
    independent_vars_benjamin,
    independent_vars_benjamin_std
  )
names(variable_sets) <-
  c(
    "dependent_vars",
    "dependent_vars_std",
    "dependent_vars_selection",
    "dependent_vars_selection_std",
    "non_used_wealth_variables",
    "independent_vars",
    "independent_vars_std",
    "independent_vars_selection",
    "independent_vars_selection_std",
    "dependent_vars_benjamin",
    "dependent_vars_benjamin_std",
    "independent_vars_benjamin",
    "independent_vars_benjamin_std"
  )

## save dataset
save(variable_sets, file = paste0(outpath, "variable_sets.Rdata")) # save list

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
xtable(cond_mean_after_trans, title = "Conditional Means (after variable transformations", digits = 3)
save(
  cond_mean_after_trans,
  file = paste0(
    outpath_des_after_trans,
    "cond_means_table_after.Rdata"
  )
)

# Standardized Difference Table 
standardized_difference <- as.data.frame(balance_check(mydata_transform,mydata_transform$e401)) %>%
  arrange(desc(abs(standardized_difference)))

save(standardized_difference, file= paste0(outpath_des_after_trans, "standardized_difference.Rdata"))
xtable(standardized_difference, title = "Standardized Difference (after variable transformations", digits = 3)

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
