# Advanced Econometric Methods Project

## Research Question
What is the (conditional) average treatment effect of 401k eligibility on wealth? (No IV)

## TODO 
Final Dataset - Erik. STATUS: main part done, dataset is ready to work with. Need proofread + someone to look at descriptives after transform for check. 
	Check CS after all transformations. Drop Vars w/o CS. - DONE (dropped ~350 obs with low income because they all had e401==0)
	Redo descriptives - Done
	Balance Check & difference in means test - DONE. Add to Descriptive Table and explain. (indication for relevant confounders)
	Remainig vars: Standardize cont vars + dummys  - DONE
	Documentation on all variables - Done. Will improve if questions occur
	Variable transformation overview + table - Done. 
	Fancy Descriptives for Paper - NOT DONE
Text 
	Set up paper - Davia
	(a)to(d)  - Davia, Erik.  (bullet points, compare, then devide writing part)
	Desciptive Section - Erik, Johannes as proofread
	Assumptions for Estimators: everyone who codes the estimator
	Result Section: setup useful comparision tables - NOONE so far.
		Every estimator "delivers" estimates
	Intro / Conclusion

## To DO - Estimators
First Person is responsible for the writing & documentation.
1.	Lasso (postlasso) - Johannes, Mila
2.	Nonparametric (kernel? Reduce dimensionality – pca/tsne) - Mila, Johannes
3.	Semiparametric - Jere, Johannes
4.	NLS - Jere (check what is it)
5.	Difference in Means + Simple linear, propensity score (matching?), doubly robust. - Davia, Erik
6.	Causal random forest (package). Double ML package Knauss useful? - Erik, Jere
7.	Other estimators Erik can think of and which Jere can code. 

## Open Questions / Todos
E: Reverse causality issue with eligibility? (if yes need to mention)
E: ATE or CATE. 
	Petyo wants ATE and thinks CATE would be great. 
	possible variables for conditioning? Discuss:  income (as in paper), family size,IRA and/or DB ppl vs no pension plan before, ...? Do we only want to condition on dummies or also on continous variables? (E.g. in paper they use income dummies, for OLS we could then estimate different models).
	Question: statistical tests, if cates are different from each other possible?
E: Heterogeneity across states. States have different tax levels. self-selection effect of ppl into states
		Should we use (wealth) variables before or after tax? 
E: Might need more INTERACTION TERMS etc. (especially for Lasso). Feel free to
	best option: add variables in preprocessing file, and add them to variable sets
	or (if really only useful for one estimator): add in estimator code
	E: IMPORTANT: Outcome Total wealth is transformed as log by shifting (due to ~1000 negative values). Any possible adjustments? Issue: we cannot interpret effect properly at the moment!
		"Solution": also created quantile total wealth (and other outcomes): test sensitiviy of results towards variable transformations.
E: Ideas for cool descriptives in paper? 
	

## Information pension plans
DB - defined benefit plan, mostly payed by employer, introduced before 401k.
Withdrawal without penalty: generally above age XX (look up what it was in 1991)
Important to know for fancy intro: which states offered 401k in 1990 and now, share of firms offering 401k, # americans in 401k...

## Information Variables
Outcome variables: 
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
				

## Questions for Petyo:
Do we need to use IV? - No IV, he wants effect of eligibility. 
Any way to test for reverse causality for cross section (no TS data)? We dont know any. - NO. Just focus on fulfilling CIA,. Reverse Causality can be mentioned but igonored.
Can we use additional papers as source? (many that also worked on the same dataset) - Sure we can. 

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
