/* =============================================================================

Poverty Measurement Course July 2022
Poverty and Equity Global Practice
The World Bank

Stata Exercises: Construction of Consumption Aggregate for 
                 2013 Ethiopia Socioeconomic Survey

By: Meliz Tyurkileri
Email: melizturkileri@sabanciuniv.edu

============================================================================= */


clear

/* -------------------------------------------------------------------------- */
/*          A. Preliminaries                                                  */
/* -------------------------------------------------------------------------- */

/* ---- 1.  Construct household size to look at expenditure per capita ------ */

use "${raw}sect1_hh_w2.dta"
drop if hh_s1q04c == 2 // drop people no longer in the hh
assert rural != . // just pick a random, never missing numeric variable to count when collapsing
collapse (count) hhsize = rural, by(household_id2)
save "${temp}hhsize.dta", replace


/* -------------------------------------------------------------------------- */
/*          B. Section 6A: One Month                                          */
/* -------------------------------------------------------------------------- */

use "${raw}sect6a_hh_w2.dta", clear
rename hh_s6a* * // this is optional, I just prefer to work with shorter variable names


/* ---- 0. Preliminaries ---------------------------------------------------- */

//  a. merge in household size
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match)

//  b. drop items not included in consumption aggregate
// STEP 1: 

drop if q00==12 //  we deal with house rent seperately.

/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. missing values of q01 (did the hh purchase?)
count if q01 == .

//  b. missing values of q02 (amount spent) when the hh purchased

count if q02==. & q01==1

//  c. look at per capita expenditure by item and value of rural
gen pc_q02 = q02 / hhsize
tabstat pc_q02 [aw = pw2], by(q00) s(min p5 p50 p95 p99 max)
bys rural: tabstat pc_q02 [aw = pw2], by(q00) s(min p5 p50 p95 p99 max)


/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. construct median and 99th pctile values of per capita expenditure by item and rural
//     and whether more than 50% of households in each value of rural bought the item
egen pc_upper = wpctile(pc_q02), w(pw2) p(99) by(rural q00)
egen pc_med   = wpctile(pc_q02), w(pw2) p(50) by(rural q00)
egen mbuy = wpctile(q01), w(pw2) p(50) by(rural q00) // if mbuy = 1, more than 50% bought

//  b. initialize expenditure variable 
//     we will make corrections / impuations to this variable rather than q02 directly
gen expenditure = q02
replace expenditure = 0 if q01 == 2

//  c. address any missing values of q01
gen buy = q01 == 1 if q01 < .
replace buy = 1 if q01 == . & mbuy == 1 // if more than 50% of hhs bought, assume hh bought if q01 is missing
replace buy = 0 if q01 == . & mbuy == 2 // if less than 50% of hhs bought, assume hh didn't buy if q01 is missing

//  d. address any missing values of q02

count if q02==. & buy==1 // check missing values of q02 (amount spent) when the hh purchased 
replace expenditure=pc_med*hhsize if q02 == . & buy == 1

// I used percentile method for adressing outliers. How can I decide whether I need to use Z-score or percentile method.
//  e. address any upper outliers in q02

tab q00 [fw = round(pw2)] if pc_q02 > 2 * pc_upper & pc_q02 < .
replace expenditure=2 * pc_upper * hhsize if pc_q02 > 2 * pc_upper & pc_q02 < . // winsorize upper outliers 

//  f. address any lower outliers (including inappropriate values of 0) in q02
//construct median and 1st pctile values of total expenditure by item and rural
egen tot_lower = wpctile(q02),    w(pw2) p(1)  by(rural q00) 
egen tot_med = wpctile(q02),    w(pw2) p(50)  by(rural q00) 

tab q00  [fw = round(pw2)] if q02< tot_lower / 2
tab q02 [fw = round(pw2)] if q02 < tot_lower / 2 
count if q02==0 & buy==1

replace expenditure=tot_med if  q02==0 & buy==1 // treat inappropriate values of 0 as missing, and impute them.

/* ---- 3. Construct annual nonfood consumption ----------------------------- */ 

//  a. annualize expenditure
//  STEP 1: 
gen expenditure_ann=12*expenditure


//  b. collapse to household-level data with one variable (called nonfood1) for total hh expenditure (also keep ea and weight variables)
//  STEP 1: 
collapse (sum) nonfood1=expenditure_ann (mean) pw2 (sum) q02 (firstnm) ea_id2, by (household_id2)

//  c. check distribution
gen log1 = log(nonfood1)
*histogram log1 [fw = round(pw2)]
list if log1 > 2.5 & log1 < 3.5 // suspicious... I tried to identify lower outliers but excluding them did not solve the problem. 
drop log1

/* ---- 4. Save ------------------------------------------------------------- */

// should just have three variables: ea_id2, household_id2 and nonfood1: total annual nonfood expenditure on the items from this section
isid household_id2
tempfile nonfood1
save `nonfood1'



/* -------------------------------------------------------------------------- */
/*          C. Section 6B: Twelve Months                                      */
/* -------------------------------------------------------------------------- */

use "${raw}sect6b_hh_w2.dta", clear
rename hh_s6b* *


/* ---- 0. Preliminaries ---------------------------------------------------- */

//  a. merge in household size
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match)

//  b. drop items not included in consumption aggregate
// STEP 1:
 drop if inrange(q00,9,12) | q00==7 //  Furniture will be recorded as durable. Exclude ceremonial expenses, Contributions to IDDIR, Donations to the church, taxes and levies.

/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. missing values of q03 (did the hh purchase?)
count if q03 == .

//  b. missing values of q04 (amount spent) when the hh purchased
count if q04==. & q03==1

//  c. look at per capita expenditure by item and values of rural
gen pc_q04 = q04 / hhsize
tabstat pc_q04 [aw = pw2], by(q00) s(min p5 p50 p95 p99 max)
bys rural: tabstat pc_q04 [aw = pw2], by(q00) s(min p5 p50 p95 p99 max)


/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. construct median and 99th pctile values of per capita expenditure by item and values of rural
//     and whether more than 50% of households in each value of rural bought the item

egen pc_upper = wpctile(pc_q04), w(pw2) p(99) by(rural q00)
egen pc_med   = wpctile(pc_q04), w(pw2) p(50) by(rural q00)
egen mbuy = wpctile(q03), w(pw2) p(50) by(rural q00) // if mbuy = 1, more than 50% bought


//  b. initialize expenditure variable 
//     we will make corrections / impuations to this variable rather than q04 directly
gen expenditure = q04

//  c. address any missing values of q03
gen buy = q03 == 1 if q03 < .
replace buy = 1 if q03 == . & mbuy == 1 // if more than 50% of hhs bought, assume hh bought if q01 is missing
replace buy = 0 if q03 == . & mbuy == 2 // if less than 50% of hhs bought, assume hh didn't buy if q01 is missing

//  d. address any missing values of q04
count if q04==. & buy==1 // check missing values of q04 (amount spent) when the hh purchased 
replace expenditure=pc_med*hhsize if q04 == . & buy == 1

//  e. address any upper outliers in q04


tab q00 [fw = round(pw2)] if pc_q04 > 2 * pc_upper & pc_q04 < .
replace expenditure=2 * pc_upper * hhsize if pc_q04 > 2 * pc_upper & pc_q04 < . 

//  f. address any lower outliers (including inappropriate values of 0) in q04
egen tot_lower = wpctile(q04),    w(pw2) p(1)  by(rural q00) 
egen tot_med = wpctile(q04),    w(pw2) p(50)  by(rural q00) 

tab q00  [fw = round(pw2)] if q04< tot_lower / 2
tab q04 [fw = round(pw2)] if q04< tot_lower / 2 
count if q04==0 & buy== 1 

*replace expenditure=tot_med if  q04==0 & buy==1 // There is not any inappropriate values.


/* ---- 3. Construct annual nonfood consumption ----------------------------- */ 

//  a. annualize expenditure
//  STEP 1: 

gen expenditure_ann=expenditure // (it is annual )

//  b. collapse to household-level data with one variable (called nonfood2) for total hh expenditure (also keep ea and weight variables)
//  STEP 1: 

collapse (sum) nonfood2=expenditure_ann (mean) pw2 (firstnm) ea_id2, by (household_id2)

//  c. check distribution

gen log1 = log(nonfood2)
*histogram log1 [fw = round(pw2)]
drop log1

/* ---- 4. Save ------------------------------------------------------------- */

// should just have three variables: ea_id2, household_id2 and nonfood2: total annual nonfood expenditure on the items from this section
isid household_id2
tempfile nonfood2
save `nonfood2'



/* -------------------------------------------------------------------------- */
/*          D. Section 9: Utilities                                           */
/* -------------------------------------------------------------------------- */

use "${raw}sect9_hh_w2.dta", clear
rename hh_s9* *


/* ---- 0. Preliminaries ---------------------------------------------------- */

//  a. merge in household size
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match)


/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. missing values in q15_a q19_c q23
count if q15_a == .
count if q19_c == .
count if q23   == .

//  b. outlier values in q15_a, q19_c and q23
foreach var of varlist q15_a q19_c q23 {
        gen pc_`var' = `var' / hhsize
}
tabstat pc_* [aw = pw2], s(min p5 p50 p95 p99 max)
bys rural: tabstat pc_* [aw = pw2], s(min p5 p50 p95 p99 max)


/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. construct median and 99th pctile values of per capita expenditure by item and values of rural
//      
foreach var of varlist pc_q15_a pc_q19_c pc_q23 {
        egen upper_`var' = wpctile(`var'), w(pw2) p(99) by(rural)
		egen med_`var'   = wpctile(`var'), w(pw2) p(50) by(rural)	
}

// There is a filter question for q23, check whether more than 50% of households in each value of rural access to the service
    egen have_q22 = wpctile(q22), w(pw2) p(50) by(rural) // if have = 1, more than 50% bought

//  b. initialize expenditure variables
gen exp_q15_a = q15_a
gen exp_q19_c = q19_c
gen exp_q23   = q23

//  c_1. address any missing values of q15_a, q19_c 

foreach var of varlist q15_a q19_c  {
          replace exp_`var'= med_pc_`var' * hhsize if exp_`var'==. 
}

//  c_2. address any missing values of q22 and q23 (filter question)
gen have = q22 == 1 if q22 < .
replace have = 1 if q22 == . & have_q22 == 1 // if more than 50% of hhs have, assume hh have if q22 is missing
replace have = 0 if q22 == . & have_q22 == 2 // if less than 50% of hhs have, assume hh didn't have if q22 is missing

count if q23==. & have==1 // check missing values of q23 (amount spent) when at least one member of hh have cell phone or landline phone
replace exp_q23=med_pc_q23*hhsize if q23 ==. & have == 1

//  d. address any upper outliers of q15_a, q19_c and q23

count if pc_q15_a > 2 * upper_pc_q15_a & pc_q15_a < .
count if pc_q19_c > 2 * upper_pc_q19_c & pc_q19_c < .
count if pc_q23 > 2 * upper_pc_q23 & pc_q23 < .


*histogram pc_q15_a [fw = round(pw2)] , by(rural) // check distribution for water
*histogram pc_q19_c [fw = round(pw2)], by(rural) // check distribution for electricity
*histogram pc_q23 [fw = round(pw2)], by(rural) // check distribution for phone utilization

foreach var of varlist q15_a q19_c q23 {
          replace exp_`var'= 2 * upper_pc_`var' * hhsize if pc_`var' > 2 * upper_pc_`var' & pc_`var' < . 
}


//  e. address any lower outliers of q15_a, q19_c and q23
foreach var of varlist q15_a q19_c q23 {
        egen tot_lower_`var' = wpctile(`var'),    w(pw2) p(1)  by(rural) 
        egen tot_med_`var' = wpctile(`var'),    w(pw2) p(50)  by(rural ) 
}

foreach var of varlist q15_a q19_c q23 {
          count if `var' < tot_lower_`var' / 2 
}

count if q23==0 & have== 1 
replace exp_q23=med_pc_q23*hhsize if  q23==0 & have==1 // deal with inappropriate values of 0


/* ---- 3. Construct annual nonfood consumption ----------------------------- */ 

//  STEP 1:
// should just have FOUR variables: ea_id2, household_id2, rural and nonfood3: total annual nonfood expenditure on the items from this section
// will keep rural for analysis later

foreach var of varlist exp_q15_a exp_q19_c exp_q23 {
        gen ann_`var' = `var'*12
}

egen nonfood3=rowtotal(ann_exp_q15_a ann_exp_q19_c ann_exp_q23)
collapse (sum) nonfood3 (mean) rural pw2 (firstnm) ea_id2, by (household_id2)

isid household_id2


/* -------------------------------------------------------------------------- */
/*          E. Merge and Check                                                */
/* -------------------------------------------------------------------------- */

/* ---- 1. Merge ------------------------------------------------------------ */

merge 1:1 household_id2 using `nonfood1', nogen
merge 1:1 household_id2 using `nonfood2', nogen


/* ---- 2. Sum values from three datasets ----------------------------------- */

//  STEP 1: construct the variable nonfood, the sum of nonfood expenditure from all three datasets 
egen nonfood=rowtotal(nonfood1 nonfood2 nonfood3)

/* ----- 3. Check overall distribution -------------------------------------- */

//  a. overall
gen lognonfood = log(nonfood) // note 30 hhs not consuming any of these
*histogram lognonfood // small cluster around 3.5 ish worth investigating 



/* --- 4. Save -------------------------------------------------------------- */

keep ea_id2 household_id2 nonfood
lab var nonfood "expenditure on non-food items/serivces, annual"
save "${out}Cons_nonfood.dta", replace


*_______________________________________________________________________________
***************************- END of do-file  -**********************************
