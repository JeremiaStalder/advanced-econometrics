# Advanced Econometric Methods Project

## overleaf
https://www.overleaf.com/project/5f8301e2d430cc0001f661f7

## Research Question
What is the (conditional) average treatment effect of 401k eligibility on wealth? (No IV)

## Finished Code Files for Jere
parametric.R
parametric_functions.R

## Estimates to Report
Simple Means
OLS / Conditional Means
Doubly_Robust_Base
NLS
Non-Parametric
Semiparametric
Conditional Means Lasso
Double Selection Lasso
Forest


## Questions for Petyo:
Deadline Paper & Exam Date
how to present results? need sds/conf intervals etc - yes, get SDs and try to get confidence intervals
Figures: ok to use color? (e.g. correlation plots, overlap histograms work better)  - yes we can
What type of assumption do you mean for e.g. nonparametric estimation (math assumptions or content)? (apart from the classic Sel on Obs Ass)
Space mgmt: only 7pagesfor 5ppl? -> we are free to write as much as we want, he will read it :P
	- detailed description of estimators
	- figures appendix
Cod File orga: what to do with code that runs a long time?
				can we call the seperate scripts from one main file -> mark it. Can use separate files with one main file. 
	
## Proofreading List
Every main section needs min 2 proofreaders that did not work on the section
Every estimation section needs min 1 proofreader that did not work on the section
1. Intro. Written by:
2. Descriptives. Written by: Erik. Proofread by: Davia,
3. Empirical strategy. 
3.1: ATE: Written by: Davia & Erik. Proofread by:
3.2: Diff-In-Means: Written by: Davia & Erik. Proofread by:
3.3: CIA-Assumptions: Written by: Davia & Erik. Proofread by:
3.4: Parametric Model: Written by: Davia & Erik. Proofread by:
3.5: NLS: Written by: Proofread by:
3.6: Non-Parametric / Semiparametric: Written by: Proofread by: Erik (state 12.01, added notes)
3.7: Lasso: Written by: Johannes, Erik Proofread by: Erik (state 12.01)
3.8: Causal Random Forest: Written by: Jere. Proofread by: Erik (state 12.01)
4: Results: Written by:  Proofread by: 
5: Conclusion Written by:  Proofread by:
A: Appendix. Proofread by: 


## Meeting 18.11.2020
0. General Questions
1. Compare estimation results	
	Report: ATE, CATE (5 Quintiles)
	If possible: Standard errors ATE (and CATEs)
	If possible: confidence intervals ATE (and CATEs)
	relative ATE = ATE / mean(wealth), relative CATE = CATE / mean(wealth in that group)
2. Decide on 
	variables used (similar for most estimators)
	cate possible with all methods? (E: could do income quintiles cate- Q: do we then still include income as regressor?)
	which outcome variable to use? Multiple ones?
	estimates to do / report (ATE, CATE. With SDs?) -> can we get sds for all estimators?
3. Other Topics
	Desciptive Section: other ideas for charts? Things to mention?
	Question List open questions: to through and see if still relevant
4. Writing: Devide work
	Descriptives - Erik
	Someone for each model to describe + proofread
	Proofread task a-d
	s.o. for result section: collect all estimates in tables etc, write comparison. Compare with results of other papers? + proofread
	Intro & conclusion
5. Code: s.o. to collect code & clean (after estimations are done)

## TODO 
Final Dataset - Erik. STATUS: main part done, dataset is ready to work with. Need proofread + someone to look at descriptives after transform for check. 
	Check CS after all transformations. Drop Vars w/o CS. - DONE (dropped ~350 obs with low income because they all had e401==0)
	Fancy Descriptives for Paper - Partly done. Any more Ideas?
Text 
	Set up paper - Davia
	(a)to(d)  - Davia, Erik.  (bullet points, compare, then devide writing part)
	Desciptive Section - Erik, Johannes as proofread. 
	Assumptions for Estimators: everyone who codes the estimator
	Result Section: setup useful comparision tables - NOONE so far.
		Every estimator "delivers" estimates
		
## To DO - Estimators
First Person is responsible for the writing & documentation.
1.	Lasso (postlasso) - Johannes, Mila
2.	Nonparametric (kernel? Reduce dimensionality – pca/tsne) - Mila, Johannes
3.	Semiparametric - Jere, Johannes
4.	NLS - Jere (check what is it)
5.	Difference in Means + Simple linear, propensity score (matching?), doubly robust. - Davia, Erik- - ATE done, CATE not
6.	Causal random forest (package). Double ML package Knauss useful? - Erik, Jere - So far only quick and dirty without understanding. Used grf package, augmented ipw for ate/cate 
		Quests: how does the average_treatment_effect fct get standard errors? - look up 
		Look up what grf package does for description
7.	Other estimators Erik can think of and which Jere can code. 

## Open Questions / Todos
E: For blabla in paper: Heterogeneity across states. States have different tax levels. self-selection effect of ppl into states
	Should we use (wealth) variables before or after tax? 
E: Might need more INTERACTION TERMS etc. (especially for Lasso). Feel free to
	best option: add variables in preprocessing file, and add them to variable sets
	or (if really only useful for one estimator): add in estimator code
	E: IMPORTANT: Outcome Total wealth is transformed as log by shifting (due to ~1000 negative values). Any possible adjustments? Issue: we cannot interpret effect properly at the moment!
		"Solution": also created quantile total wealth (and other outcomes): test sensitiviy of results towards variable transformations.
E: Ideas for cool descriptives in paper? 
E: Variable transformations: 
	Outcome variable to use: quantile or log? (since tw has negative values)
	Confounder: no confounder has negative values initially, so log transformation would be fine. Quantiles would maybe be more consistent with outcome variable. Which do we use? 
E: ATE or CATE. 
	Petyo wants ATE and thinks CATE would be great. 
	possible variables for conditioning? Discuss:  income (as in paper), family size,IRA and/or DB ppl vs no pension plan before, ...? Do we only want to condition on dummies or also on continous variables? (E.g. in paper they use income dummies, for OLS we could then estimate different models).
	Question: statistical tests, if cates are different from each other?
	CATE could be done for the 5 income quintiles. Is income as variable then removed? (because its probably still confounding. But can have overfitting with polynomials, maybe remove them? 
E: Which variable sets to use?
	outcome: test all dependent_vars_selection & dependent_vars_selection_quantiles 
	confounders: independent_vars_selection
	for some methods (e.g. lasso) we can ofc deviate. Then state which vars are used. 
E: Effect Estimates: which results do we present?	
	Effect Estimate. 
	SD, T-Val, P-val -> for some methods hard / impossible to get?
	For all outcome measures
	Someone that collects all measures
	Someone to write result section
E: How to get SD for IPW, Doubly-Robust? 
E/Davia: Cut obs for IPW

	
## Information pension plans
DB - defined benefit plan, mostly payed by employer, introduced before 401k.
Withdrawal without penalty: generally above age XX (look up what it was in 1991)
Important to know for fancy intro: which states offered 401k in 1990 and now, share of firms offering 401k, # americans in 401k...

## Information Variables
Outcome variables: 
	SUGGESTION for outcome variable to use:  tw_adjust_quantile, others in dependent_variable_selection (tw has 1000 negative values, therefore logtransmation with shift kills interpretability)
	Transformation forms: logged, quantiles (both also standardized), for tw: also original
	Variables
		tw_adjust - Total wealth (includes all substitution effect between different sources) 
		net_niNet Financial Assets (net_tfa)
		Net non401k Financial Assets (effect of 401k to non-401 savings) 
	ISSUE with all measures: (Note this in descriptive section)
		401k & IRA are pre-tax values (because they are taxed at withdrawal) while other assets are not. 
		Therefore, they are overvalued. Could lead to overestimation of an effect. 
		Benjamin (2003) suggests to correct for this using the marginal tax rates. However, we ommit this also because we do not know the state of residence for state tax rates. 
Standardization: every variable has a version "_std" in case you need the standardized variable. This also holds for binary (for e.g. Lasso, also binaries should be standardized)
VARIABLE SETS 
	Sets: dependent / independent variables (all, suggested selection, benjamin (vars from paper))
	Where: saved in Rfile in output
	Which to use: most important - selection. Test different outcomes,  quantile and log transformation (thats the "normal" variable set without suffix)
	"_std" - standardized versions
	"_quantile" - quantile versions
	FEEL FREE to change sets / create new sets in main file
	see balance check table in text to decide on relevance of confounders. 
	
## Data transformation
FINAL DATASET
		mydata_transform.Rdata in output
### Dropped Observations
	due to Income: ~350 observations with low income due to no common support- Grund: inc is important confounder
	due to Wealth: ~20 left outliers to reduce issue of log transformation, avoid overfitting to outliers
	due to hequity: ~20 observations with negative equity - outliers (eventhough it is possible)
### Variable Transformations
1.	Age! Try different forms of polynomials, different interaction terms. 
		Age^2 & age^3 after standardizing
2.	Education (maybe only use dummies)!
		Dropped years of education, kept dummies (Dummies are directly coded from years of education.) 
12.	Check dummy variables – whether we need to kick out the reference category
		Education Dummies: nohs as reference category (no high school degree)
3.	Dummy for household size = 2 and 2 earners, more family variables if possible
		Answer: created "busy_couple" variable (size = 2 and 2 earners) - 1 for ~1000 obs
4.  Dummys for: Mortgage, home equity (positive & negative, because 4000 obs have eq=0). NOT for a401, hval, ira (because p401, hown, pira exist already)
		Answer: Done
8.	Check male dummy (how is it coded) because we have higher share of 0 which is weird
		E: No paper mentioned this or included this as confounder. We assume 0 is male.
9.	Zhat is IV variable? 
		dropped, we dont do IV
10.	Standardize ALL variables
		Done. For new continuous variables, add them to the vector to standardize
11.	Log transformation for wealth vars, income. 
		E:stupid question - how do we log a variable that has negative values (such as home equity)? 
		AW: We transformed via: x =  if min(x)<1 then x = x-min(x)+1 else x = x. (then all x>=1, can take logs. 
		Issue if transformed: coefficient cannot be interpreted
12. Family Size
		Dummified. Reference category: 1 person household. Combined family size >=5
14. add dummy for withdrawal
		Done
14: What is tfa_he?
		Financial Assets and house equity (cannot fully replicate). Dropped
15. All outcomes: logged, quantiles as transformation
16. Total Wealth transformation according to benjamin (2003)
		Problem: Value of DB-pension plan not measured
		Corrections:
			1. replacement of preexisting db-plan: subtract 20% of 401k assets from from tw, tfa, net_tfa
			2. marginal substitution effect: employers shift support from DB to 401k plan. Larger effeect for high income workers which are more likely to be eligible. subtract 0.032$ from tw, tfa, net_tfa per $of income 
				

## Possibly good descriptives for our data
1. correlation matrix (done)
2. boxplots / hists for continuous variables (before and after transformation, univariate)
3. boxplots / hists for all variables grouped by eligiblilty (shows difference between treated & non-treated in covariates -> identify confounders + test common support assumption)
4. descriptive table for the same (all vars grouped by eligibilty) as in benjamin, table 1. Include t-test for difference in means AND / OR Balance Check procedure (see screenshot from DA II). Package BalanceCheck in R does sth more complicated, can also test this.
5. cross tabulation for eligibility and other binary variables (to see if each group has sufficient obs + can test for independence of variable and treatment via chi-square-test -> if independent then variable is no true confounder)
6. classic boring descriptives table for all variables 

## Identification assumption
Eligibility is exogenous given confounders. 

## Literature
Benjamin: 
	Uses eligibility as treatment with propensity score subsclassification 
	Outcome: total assets to avoid reshuffeling effects. 
	DB-variable: Problem: DB-wealth is not included in total assets. -> correct financial assets & total wealth variable
	
## Questions from first Meeting
1. Who is the reference person? How is wealth measured (per person or per household)? 
  - Answer: reference person is the one, in whose name the house is written
2. With two earners, what does the eligibility dummy represent? (should be for the reference person)
  - How the income is shared between the people in one household?
		E-Answer - we cannot infer. Or did anyone find sth? 
3.	Check plausibility:
  - Home ownership, home value, mortgage
		Answer: hown = 1 if hval>0
		hequity = hval - hmort (can be negative)
		home value and mortage are right censored ->home equity too. Seems like wealth variables use real unbounded value, but cannot proof this (tested tfa - tfa_he)
  - family size =1 and two earners=1 
		checked, does not exist.
  - wealth variables
		Total Wealth: 
			cannot fully replicate because value of other property, vehicles (other_assets) etc not included as variable. 
			created variable other_assets is mostly positive and right skewed. plausible.
		Financial assets: a401 + ira + nifa (works apart from 85 cases)
		Net Fin Assets: non 401 assets + a401 +ira (works for cases with no ira apart from 30 cases. IRA is probably somehow taxed).
  - eligibility and participation: cannot participate without eligibility 
		checked, does not exist.
  - education dummies: sum up to 1 - checked (removed nohs -> nohs as reference category)
4.	Check for data errors (like negative income)
		Checked summary stats all variables, reasonable. For income, 5 obs are negative or 0. Removed
5.	Check criteria of eligibility for 401k/IRA/DB
		401k: eligible: employee of an employer who offers it. What is it: Tax-exempt pension plan with employee and employer contribution, privatly organized fonds (tax payment at withdrawal)
		IRA: eligible: possible for everyone (also self employed). What is it: 
		DB: eligible: employee of an employer who offers it. What is it: other pension plan. usually only played by employer (except for public institutions).
