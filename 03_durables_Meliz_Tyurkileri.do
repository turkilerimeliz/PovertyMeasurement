/* =============================================================================

Poverty Measurement Course July 2022
Poverty and Equity Global Practice
The World Bank

Stata Exercises: Construction of Consumption Aggregate for 
                 2013 Ethiopia Socioeconomic Survey

By: Meliz Tyurkileri
Email: melizturkileri@sabanciuniv.edu

============================================================================= */


pause on
clear

/* -------------------------------------------------------------------------- */
/*          A. Section 10: Durables                                           */
/* -------------------------------------------------------------------------- */

use "${raw}sect10_hh_w2_CONSTRUCTED.dta", clear
rename hh_s10* *

merge m:1 household_id2 using "${temp}hhsize.dta", assert(match) nogen // merge in hh size

/* ---- 0. Preliminaries ---------------------------------------------------- */

//  a. drop durables that shouldn't be included in consumption aggregate
//  STEP 1: 

drop if inrange(q00,16,19) | q00==24 | inrange(q00,30,34) | q00==36 // drop cart, sewing machine, weaving equip., gold, silver, sickle, axe, pick axe, and plough

//  b. reshape

// check for missing values or unreasonable observations
count if q01==.
count if q01==. & (q03_1!=. | q03_2!=. | q04_1!=. |q04_2!=.)

//reshape
drop q02_a q02_b
reshape long q03_ q04_ q05_, i(household_id2 q00) j(instance)
lab var q03_ "For how much did you purchase the item"
lab var q04_ "How many years ago did you purchase the item"
lab var q05_ "If you sold the item today, for how much could you sell it"


/* ---- 1. Investigate missing and outlier values --------------------------- */

//  ADD CODE HERE

// we will NOT address these issues here, but rather exclude any outliers from the calculation of use value
// once use value is constructed, we will use this to fill in any missing observations

// a. investigate missing values and unreasonable observations

// identify observations where all variables are missing
gen report_flag=(q03!=. | q04!=. | q05!=.)

// construct number of unmissing observations by hhid and item
bys household_id2 q00: egen total_number_item=sum(report_flag)

// check whether number of unmissing observations is higher than reported number of item in q01
count if total_number_item>q01

// check whether number of unmissing observation is 0 when a given hh seems to have corresponding item.
count if total_number_item==0 & q01!=. & q01!=0

// check whether a given hh seems to have corresponding item when number of unmissing observation is 0.
count if q01>0  & total_number_item==0

// drop obervations where all the variables are missing and a given hh seems to have only one number of corresponding item. These are the observations with missing values generated becaue we reshaped the data.

drop if q01==1 & report_flag==0
drop report_flag 

//  a. tag extreme values
// check for unreasonable observations q03_
 ** I did not try to identify outliers for q03 since there is a huge variation across in time item purchased. Purchasing prices may be significantly different because of the inflation.

// check for unreasonable observations q04_

gen log_q04= log(q04_)  
egen log_q04_med = wpctile(log_q04), w(pw2) p(50) by(rural q00 q04)
mads log_q04 [aw = pw2], by(rural q00 q04) m(A)
egen groupnum_q04 = group(rural q00 q04)
gen Zscore_q04 = (log_q04-log_q04_med)/A[groupnum_q04, 5]

count if Zscore_q04 < -3 & Zscore_q04 != . // couldn't identify any outlier
count if Zscore_q04 > 3 & Zscore_q04 != . // couldn't identify any outlier

// check for unreasonable observations q05_
bys q00 rural  : tabstat q05_ [aw = pw2], s(min p5 p50 p95 p99 max) // 

gen log_q05= log(q05_)  
egen log_q05_med = wpctile(log_q05), w(pw2) p(50) by(rural q00)
mads log_q05 [aw = pw2], by(rural q00) m(A)
egen groupnum_q05 = group(rural q00)
gen Zscore_q05 = (log_q05-log_q05_med)/A[groupnum_q05, 5]

count if Zscore_q05 < -3 & Zscore_q05 != .
count if Zscore_q05 > 3 & Zscore_q05 != .

//flag unreasonable observations q05_

gen flag = 0 // edit to construct a variable equal to 1 for those observations with questionnable values you want to exclude from the calculation of use value
	   
replace flag =1 if Zscore_q05 < -3 // flagged lower outliers 
replace flag =1 if Zscore_q05 > 3 & Zscore_q05 < .	// flagged upper outliers 	


/* ---- 2. Construct depreciation rates ------------------------------------- */

//  a. construct depreciation rate for each item
//  STEP 1: ADD CODE HERE
// only for non-flagged observations

clonevar item=q00
gen current_val=q05 if flag==0 // excluded flagged obs
gen purch_price=q03 if flag==0 // excluded flagged obs
gen agey=q04 if flag==0 // excluded flagged obs

tab q04 if flag==0

// deal with obs where the item bought in the reference year. I am not sure about the method that we need to use(?) 
 clonevar agey_v1=agey
     replace agey_v1=0.5 if agey==0

/*
// Can we randomly assign the number in range (0,1) for obs where the item bought in the reference year?
// I added this section for sensitivity check. I did not use it.
 clonevar agey_v2=agey
      gen random_age=runiform() if q04==0
      replace agey_v2=random_age if agey_v2==0
*/

//estimate depretiation rate for each obs.

gen depre = 1 - (current_val/purch_price)^(1/agey_v1) 
*gen depre_v2 = 1 - (current_val/purch_price)^(1/agey_v2) // I added this section for sensitivity check. I did not use it.


//  b. construct median depreciation rate for each type of item
//  STEP 1:

bys item: egen depre_p50 = pctile(depre) if depre!=., p(50)
*bys item: egen depre_p50_v2 = pctile(depre_v2) if depre_v2!=., p(50) // I added this section for sensitivity check. I did not use it.


/* ---- 3. Construct use value ---------------------------------------------- */

//  a. construct use value
//  STEP 1: 
// only for non-flagged observations
local real_interest = 0.1168 // annual average deposit rate -2013
gen usevalue = current_val*((`real_interest') + depre_p50) 
*gen usevalue_v2 = current_val*((`real_interest') + depre_p50_v2) // I added this section for sensitivity check. I did not use it.

//  b. look at distribution
gen log_usevalue = log(usevalue) // 
*gen log_usevalue_v2 = log(usevalue_v2) // I added this section for sensitivity check.
*histogram log_usevalue_v1, normal // 
*histogram log_usevalue_v2, normal // 


/* ---- 4. Address missing and outlier values ------------------------------- */

//  a. look at estimated usevalue by rural 
bys rural: tabstat usevalue [aw = pw2], s(min p5 p50 p95 p99 max) by(item) // 

//  b. construct median of the estimated usevalues by item and rural
egen lower_usevalue   = wpctile(usevalue), w(pw2) p(1) by(item rural)
egen med_usevalue   = wpctile(usevalue), w(pw2) p(50) by(item rural)
egen upper_usevalue   = wpctile(usevalue), w(pw2) p(99) by(item rural)

//  c. address any missing values or flagged obs 
count if usevalue==. & q01!=0
replace usevalue=med_usevalue if q01!=0 &  usevalue==.

//  d. address outliers

// winsorize lower outliers
replace usevalue=lower_usevalue if usevalue<lower_usevalue/2 & usevalue!=.

// winsorize upper outliers
replace usevalue=upper_usevalue if usevalue>upper_usevalue*2 & usevalue!=.

//  e. recode usevalues as 0 when the hh do not have the corresponding item.
replace usevalue=0 if q01==0


/* ---- 5. Consutruct annual use value of durables -------------------------- */

//  a. annualize
//  STEP 1: 
* We do not need to make further adjustments. Use value refers to the cost the household is willing to pay to use the durable for the year. 

//  b. sum over household
//  may want to keep extra variables for distribution checks
//  STEP 1: 
collapse (sum) durables=usevalue,  by (household_id2 ea_id2 pw2 rural saq01)
isid household_id2

//  c. check distribution

gen log_durables = log(durables) // 
*histogram log_durables, normal // 


/* ---- 8. Save ------------------------------------------------------------- */

keep household_id2 ea_id2 durables
isid household_id2
lab var durables "use value of consumer durables owned by hh, annual"
save "${out}Cons_durables.dta", replace

*_______________________________________________________________________________
***************************- END of do-file  -**********************************
