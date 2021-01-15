# Does 401(k) eligibility impact household wealth?

## Info
This project was written for an advanced econometrics class at the University of St. Gallen.
We use data of the data from the Survey of Income and Program Participation (SIPP) available from the R-package hdm.
We estimate average treatment effects of and treatment effects conditional on household income

## Contributers
Supervisor: Prof. Petyo Bonev
Students: Johannes Cordier, Liudmila Gorkun-Voevoda, Davia Kündig, Erik Senn, Jeremia Stalder

## Abstract
401(k) plans have been introduced in 1980 with the goal to ameliorate the financial outlook after retirement. While the introduction dates 30 years back, the effect on household wealth is still ambiguous today. 
By comparing 401(k) eligible and ineligible households’ wealth, this paper seeks to determine whether or not there is an effect on wealth. To answer this question, different estimators have been applied, using parametric, semi-parametric and non-parametric methods.
Our results suggest a positive average effect of eligibility on wealth between 2.5K and 4.8K US-dollars. CATE estimations further show effect heterogeneity for households of different income quintiles. Households with incomes from the top and bottom 20\% quintile are likely to benefit most from the program.
In general, we conclude that 401(k) plans are an effective policy tool to increase retirement savings as well as wealth, especially for low and high income groups.

## Estimation Methods
Simple Mean Comparison
OLS / Conditional Means
Doubly Robust Parametric Model
Non-linear Least Squares
Non-Parametric kernel regression
Nadaraya Watson estimator
Semiparametric regression
Lasso 
Double Selection Lasso 
Causal random forest

## Code
main_procedure.R - Guide through project. Calls all required code files and produces results. 

## Relevant Datasets
mydata_transform.Rdata - contains the transformed cleaned data for effect estimation
variable_sets_modelling.Rdata - contains lists of variables used as dependent and independet variables. As independent variables, we use independent_vars_selection apart from lasso, where independent_vars is used. As dependent variable, we use tw_adjust_total.

