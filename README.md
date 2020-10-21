# Advanced Econometric Methods Project

1. Who is the reference person? How is wealth measured (per person or per household)? 
  - Answer: reference person is the one, in whose name the house is written
2. With two earners, what does the eligibility dummy represent? (should be for the reference person)
  - How the income is shared between the people in one household?
3.	Check plausibility:
  - Home ownership, home value, mortgage
  - Financial assets, net fin assets, assets 401k – if they coincide => they are on household level
  - Check interactions for dummies (common support) with DAII chart
4.	Check for data errors (like negative income)
5.	Check criteria of eligibility for 401k/IRA/DB

## Variable transformation
1.	Age! Try different forms of polynomials, different interaction terms. 
2.	Education (maybe only use dummies)!
3.	Dummy for household size = 2 and 2 earners, more family variables if possible
4.	Mortgage dummy
5.	Dummy for negative home equity
6.	Dummy for variables that have a cutoff (home value, mortgage)
7.	Dummy for 0 IRA
8.	Check male dummy (how is it coded) because we have higher share of 0 which is weird
9.	Zhat is IV variable? Check if we get the same
10.	Normalize everything
11.	Log transformation for wealth vars
12.	Check dummy variables – whether we need to kick out the reference category
13.	What should we do with all the wealth variables? 
New E: 14. Could income lead to reverse causality if it includes income from assets (interest, capital gains,...). If yes: can we find an IV? (probably not). State in paper. 

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
Do we need to use IV?
Any way to test for reverse causality for cross section (no TS data)? We dont know any. 
Can we use additional papers as source? (many that also worked on the same dataset) 

## Research Question
Need to decide whats more interesting:
	Effect of 401k participation on wealth? (would need IV)
	Effect of 401k eligibility on wealth (no IV)

## Information pension plans
DB - defined benefit plan, mostly payed by employer, introduced before 401k.
Withdrawal without penalty: generally above age 59.5 (in some cases 55).
Nice to know for fancy intro: which states offered 401k in 1990 and now, share of firms offering 401k, # americans in 401k...

# Method News
E: IV necessary because 1) unobserved saving preference of ppl AND (2) self selection into participating states is correlated with participation & wealth
E: best to get CATE-estimates with all methods (not only ATE). Better for policy implications / targeting. 
	Which possible variables for conditioning? Discuss:  income (as in paper), family size,IRA and/or DB ppl vs no pension plan before, ...? Do we only want to condition on dummies or also on continous variables? (E.g. in paper they use income dummies, for OLS we could then estimate different models).
	Question: statistical tests, if cates are different from each other possible?

## Possibly good descriptives for our data
1. correlation matrix (done)
2. boxplots / hists for continuous variables (before and after transformation, univariate)
3. boxplots / hists and descriptive tables for all variables grouped by (1) participation OR (2) eligiblilty (shows difference between treated & non-treated in covariates -> identify confounders
4. cross tabulation for participation, eligibility and other binary variables (to see if each group has sufficient obs + can test for independence of variable and treatment via chi-square-test -> if independent then variable is no true confounder)
5. classic boring descriptives table for all variables 

## Identification assumption
Eligibility is exogenous given income and other covariates. 

## Literature insights
Benjamin: 
	Same IV idea (with propensity score subsclassification (what is this? ;)) 
	Outcome: total assets to avoid reshuffeling effects. 
	DB-variable: Problem: DB-wealth is not included in total assets. 
	
