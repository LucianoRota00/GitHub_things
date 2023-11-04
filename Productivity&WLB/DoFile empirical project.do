clear all
set more off

*****************
***DATA IMPORT***
***************** 

import excel "C:\Users\lucia\Desktop\GitHub repository\Productivity&WLB\dataset.xlsx", sheet("Sheet1") cellrange(A2:AB130) firstrow

************************
***SUMMARY STATISTICS***
************************
*** for the dependent variable (prod) and variables used for the WLB index
table (var) (T), ///
statistic(mean prod unrate unrate_f pt_share selfempl worklife_duration ///
homeworking avg_weekworkh lenpaid_fathleave lenpaid_mothleave mothleave_prot ///
tot_lenpaid_leave soc_ben socsupp avg_holidays avg_wh_formalcare02 ///
avg_wh_formalcare3 households_2children households_3children noleis) ///
statistic(sd prod unrate unrate_f pt_share selfempl worklife_duration ///
homeworking avg_weekworkh lenpaid_fathleave lenpaid_mothleave mothleave_prot ///
tot_lenpaid_leave soc_ben socsupp avg_holidays avg_wh_formalcare02 ///
avg_wh_formalcare3 households_2children households_3children noleis) ///
sformat("(%s)" sd) style(table-1) name(summarytable2) replace

collect export summarytable2.docx, name(summarytable2) replace

***for variables used as control afterwards
table (var) (T), ///
statistic(mean GDP_percapita median_age suicide_rate life_exp) ///
statistic(sd GDP_percapita median_age suicide_rate life_exp) ///
sformat("(%s)" sd) style(table-1) name(summarytable3) replace

collect export summarytable3.docx, name(summarytable3) replace

*******************
***NORMALIZATION***
*******************
***DEFINE A GLOBAL IN ORDER TO STORE ALL THE VARIABLE NAMES
global variables unrate unrate_f pt_share selfempl worklife_duration ///
homeworking avg_weekworkh longweekworkh lenpaid_fathleave lenpaid_mothleave soc_ben ///
mothleave_prot tot_lenpaid_leave socsupp avg_holidays ///
avg_wh_formalcare02 noleis households_3children households_2children ///
avg_wh_formalcare3 

***NORMALIZING THE VARIABLES (MIN-MAX NORMALIZATION)
foreach x of global variables {
	
	 forvalues t=2014/2019 { /*normalizing each variable in each year t*/
		sum `x' if T==`t', d /*in order to store r(min) and r(max) of the variable*/
		gen nrm_`x'_`t' = (`x'-r(min))/(r(max)-r(min)) if T==`t' /*generate the normalized values of the variable in year t*/
	}
	
	egen nrm_`x' = rowmean(nrm_`x'_*) /*since for each variable we get T columns (with T = 2014,..., 2019), 
										by doing so we condensate them on the same column*/
	drop `x' nrm_`x'_* /*just to give to the normalized variable the same name as before*/
	rename nrm_`x' `x' /*rename the normalized variable with the original name*/
}

***POLARITY SWITCH
**Define a global to store all the variables affecting negatively WLB
global negative unrate unrate_f worklife_duration ///
avg_weekworkh longweekworkh ///
noleis households_3children households_2children ///

foreach x of global negative {
	gen switched_`x' = 1-`x'
	drop `x' /*just to give to the normalized variable the same name as before*/
	rename switched_`x' `x'
}

*********************
***WLB COMPUTATION***
*********************
***PRINCIPAL COMPONENT ANALYSIS FOR EACH YEAR
forvalues t=2014/2019 {
	factor $variables if T==`t', pcf mineigen(0)
	predict pc`t' if T==`t' /*take the first principal component: this is actually the WLB index in year t*/
}

egen wlb = rowmean(pc*) /*since the wlb indexes are reported on different columns,
							by doing so we condensate them on the same column
						!!!	This creates the column with the WLB index, our main regressor*/
drop pc*

**************
***ANALYSIS***
**************
***SET DATA AS PANEL
xtset N T
gen ln_prod = ln(prod)

***REGRESSIONS
*Parsimonious regression (no fixed effects)
reg ln_prod wlb, vce(cluster country)
estimates store reg1

outreg2 reg1 using table, word replace dec(3) stats(coef se) ///
alpha(0.01, 0.05, 0.10) nodepvar drop(2015.T 2016.T 2017.T 2018.T 2019.T) ///
addstat(F-test, e(F), Adj. R2, e(r2_a)) addtext(State FE, NO, Year FE, NO)

*Fixed effects regression
xtreg ln_prod wlb i.T, fe vce(cluster country)
estimates store reg2

outreg2 reg2 using table, word append dec(3) stats(coef se) ///
alpha(0.01, 0.05, 0.10) nodepvar drop(2015.T 2016.T 2017.T 2018.T 2019.T) ///
addstat(F-test, e(F), Adj. R2, e(r2_a)) addtext(State FE, YES, Year FE, YES)

*Adding GDP per capita as control
xtreg ln_prod wlb GDP* i.T, fe vce(cluster country)
estimates store reg3

outreg2 reg3 using table, word append dec(3) stats(coef se) ///
alpha(0.01, 0.05, 0.10) nodepvar drop(2015.T 2016.T 2017.T 2018.T 2019.T) ///
addstat(F-test, e(F), Adj. R2, e(r2_a)) addtext(State FE, YES, Year FE, YES)

*Adding multiple controls but GDP per capita
xtreg ln_prod wlb median_age life_exp suicide_rate i.T, fe vce(cluster country)
estimates store reg4

outreg2 reg4 using table, word append dec(3) stats(coef se) ///
alpha(0.01, 0.05, 0.10) nodepvar drop(2015.T 2016.T 2017.T 2018.T 2019.T) ///
addstat(F-test, e(F), Adj. R2, e(r2_a)) addtext(State FE, YES, Year FE, YES)

*Full regression
xtreg ln_prod wlb GDP* median_age life_exp suicide_rate i.T, fe vce(cluster country)
estimates store reg5

outreg2 reg5 using table, word append dec(3) stats(coef se) ///
alpha(0.01, 0.05, 0.10) nodepvar drop(2015.T 2016.T 2017.T 2018.T 2019.T) ///
addstat(F-test, e(F), Adj. R2, e(r2_a)) addtext(State FE, YES, Year FE, YES) ///
sortvar(wlb GDP* median_age life_exp suicide_rate) title("Regression table")











