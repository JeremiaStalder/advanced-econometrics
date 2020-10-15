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

## Estimators
1.	Lasso (postlasso)
2.	Nonparametric (kernel? Reduce dimensionality – pca/tsne)
3.	Semiparametric ??? – ask to cover
4.	NLS
5.	Simple linear (TSLS), propensity score, double robustness
6.	Causal random forest (package)
7.	Other estimators Erik can think of and which Jere can code

Note: Ask whether we actually need to use IV.


**Davia:** research on eligibility for pension programs, paper structure, overleaf, a)-d)  
**Erik:** combination of methods with IV, relevance of complier population, a)-d)  
**Jere:** github, think about data, semiparametric est  
**Johannes:** plausibility checks, variable transformations + descriptives  
**Mila:** nonparametric estimators, think about data  


