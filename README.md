# Advanced Econometric Methods Project

1. Who is the reference person? How is wealth measured (per person or per household)? 
  - Answer: reference person is the one, in whose name the house is written
2. With two earners, what does the eligibility dummy represent? (should be for the reference person)
  - How the income is shared between the people in one household?
		E-Answer - we cannot infer. Or did anyone find sth? 
3.	Check plausibility:
  - Home ownership, home value, mortgage
		Answer: hown = 1 if hval>0
		hequity = hval - hmort (can be negative)
  - family size =1 and two earners=1 
		checked, does not exist.
  - Financial assets, net fin assets, assets 401k – if they coincide => they are on household level
  - eligibility and participation: cannot participate without eligibility 
		checked, does not exist.
  - education dummies: sum up to 1 - checked (removed nohs -> nohs as reference category)
4.	Check for data errors (like negative income)- 
		Checked summary stats all variables, reasonable. For income, 2 obs are negative. Need to remove them? 
		
5.	Check criteria of eligibility for 401k/IRA/DB

## Variable transformation
1.	Age! Try different forms of polynomials, different interaction terms. 
2.	Education (maybe only use dummies)!
		Dropped years of education, kept dummies (Dummies are directly coded from years of education.) 
12.	Check dummy variables – whether we need to kick out the reference category
		Education Dummies: nohs as reference category (no high school degree)
3.	Dummy for household size = 2 and 2 earners, more family variables if possible
		Answer: created "busy_couple" variable (size = 2 and 2 earners) - 1 for ~1000 obs
4.  Dummys for: Mortgage, home equity (positive & negative, because 4000 obs have eq=0). NOT for a401, hval, ira (because p401, hown, pira exist already)
		Answer: Done
8.	Check male dummy (how is it coded) because we have higher share of 0 which is weird
		E: No paper mentioned this or included this as confounder. Assume 0 is male? 
9.	Zhat is IV variable? 
		dropped, we dont do IV
10.	Normalize continuous variables
		Done. For new continuous variables, add them to the vector to standardize
11.	Log transformation for wealth vars, income. 
		E:stupid question - how do we log a variable that has negative values (such as home equity)? 

	
13.	What should we do with all the wealth variables? 

New: 14: E - what is tfa_he


## Estimators
1.	Lasso (postlasso)
2.	Nonparametric (kernel? Reduce dimensionality – pca/tsne)
3.	Semiparametric ??? – ask to cover
4.	NLS
5.	Simple linear (TSLS), propensity score (matching?), doubly robust. 
6.	Causal random forest (package)
7.	Other estimators Erik can think of and which Jere can code. Idea Erik: Simple Means Difference. I believe in Jere! 

**Davia:** research on eligibility for pension programs, paper structure, overleaf, a)-d)  
**Erik:** combination of methods with IV, relevance of complier population, a)-d)  
**Jere:** github, think about data, semiparametric est  
**Johannes:** plausibility checks, variable transformations + descriptives  
**Mila:** nonparametric estimators, think about data  

## Questions for Petyo:
Do we need to use IV? - No IV, he wants effect of eligibility. 
Any way to test for reverse causality for cross section (no TS data)? We dont know any. - NO. Just focus on fulfilling CIA,. Reverse Causality can be mentioned but igonored.
Can we use additional papers as source? (many that also worked on the same dataset) - Sure we can. 

## Research Question
What is the (conditional) average treatment effect of 401k eligibility on wealth? (No IV)
Are we interested in (C)ATET or (C)CATE? CATET would have weaker assumptions: Common support only for D=1 -> how to we check for this (overlap not necessary anymore)? , CIA only for Y1 (doesnt really matter). 

## Information pension plans
DB - defined benefit plan, mostly payed by employer, introduced before 401k.
Withdrawal without penalty: generally above age 59.5 (in some cases 55).
Nice to know for fancy intro: which states offered 401k in 1990 and now, share of firms offering 401k, # americans in 401k...

# Method News
E: Question - reverse causality issue with eligibility? (if yes need to mention)
E: best to get CATE-estimates with all methods (not only ATE). Better for policy implications / targeting. 
	Which possible variables for conditioning? Discuss:  income (as in paper), family size,IRA and/or DB ppl vs no pension plan before, ...? Do we only want to condition on dummies or also on continous variables? (E.g. in paper they use income dummies, for OLS we could then estimate different models).
	Question: statistical tests, if cates are different from each other possible?

## Possibly good descriptives for our data
1. correlation matrix (done)
2. boxplots / hists for continuous variables (before and after transformation, univariate)
3. boxplots / hists for all variables grouped by eligiblilty (shows difference between treated & non-treated in covariates -> identify confounders + test common support assumption)
4. descriptive table for the same (all vars grouped by eligibilty) as in benjamin, table 1. Include t-test for difference in means AND / OR Balance Check procedure (see screenshot from DA II). Package BalanceCheck in R does sth more complicated, can also test this.
5. cross tabulation for eligibility and other binary variables (to see if each group has sufficient obs + can test for independence of variable and treatment via chi-square-test -> if independent then variable is no true confounder)
6. classic boring descriptives table for all variables 

## Identification assumption
Eligibility is exogenous given confounders. 

## Literature insights
Benjamin: 
	Uses eligibility as treatment with propensity score subsclassification 
	Outcome: total assets to avoid reshuffeling effects. 
	DB-variable: Problem: DB-wealth is not included in total assets. -> correct financial assets & total wealth variable
	
