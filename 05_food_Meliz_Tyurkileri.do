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
set more off
pause on


/* -------------------------------------------------------------------------- */
/*          A. Preliminaries                                                  */
/* -------------------------------------------------------------------------- */

use "${raw}sect5a_hh_w2.dta", clear
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match) nogen // merge in hh size
rename hh_s5a* *
numlabel HH_S5AQ00, add


/* ---- 1. Binaries for whether food is consumed from each source ----------- */

//  a. did the household consume anything?
assert q01 < .
egen nvars = rownonmiss(q02_a-q06_b), strok
tab nvars q01
gen consume = q01 == 1

//  b. consumption from various different sources

gen get3 = (q03_a > 0 & q03_a < .) | (q04 > 0 & q04 < .)
gen get5 = (q05_a > 0 & q05_a < .)
gen get6 = (q06_a > 0 & q06_a < .)
egen sources = rowtotal(get3 get5 get6)
tab sources q01, m
tab sources if nvars > 0 & q01 == 2

//  c. correct consumption binaries: filter question is no but consumption from some source given
list q00 q02_a q02_b q03_a q03_b q04 q05_a q05_b q06_a q06_b if source == 1 & q01 == 2 // all look like purchases
replace get3 = 1 if source == 1 & q01 == 2
replace consume = 1 if source == 1 & q01 == 2

//  d. correct consumption binaries: filter question is yes, but no consumption from any source given
list q00 q02_a q02_b q03_a q03_b q04 q05_a q05_b q06_a q06_b get* if source == 0 & q01 == 1
// if there's a unit given, assume some consumption from that source
replace get3 = 1 if sources == 0 & q01 == 1 & q03_b < . 
replace get5 = 1 if sources == 0 & q01 == 1 & q05_b < .
replace get6 = 1 if sources == 0 & q01 == 1 & q06_b < .

// otherwise, pick the most likely source for the item in that level of rural
*table q00 rural if consume, c(mean get3 mean get5 mean get6)
replace get3 = 1 if sources == 0 & q01 == 1 & inlist(rural, 2, 3) // purchase most common for all items in small town and large town
replace get3 = 1 if sources == 0 & q01 == 1 & rural == 1 & inlist(q00, 7, 8, 9, 10, 13, 14, 15, 16, 18, 22, 23, 24, 25)
replace get5 = 1 if sources == 0 & q01 == 1 & rural == 1 & inlist(q00, 1, 2, 3, 4, 5, 6, 11, 12, 17, 19, 20, 21, 26)

//  e. correct consumption binaries: quantity from purchases given but no expenditure
count if q04 == 0 & get3
tab get5 if q04 == 0 & get3
tab get6 if q04 == 0 & get3
list q02_a q02_b q03_a q03_b q04 q05_a q05_b q06_a q06_b if q04 == 0 & get3 & get5
replace get3 = 0 if q04 == 0 & (get5 & q05_a == q02_a & q05_b == q02_b)
list q02_a q02_b q03_a q03_b q04 q05_a q05_b q06_a q06_b if q04 == 0 & get3 & get6
replace get3 = 0 if q04 == 0 & (get6 & q06_a == q02_a & q06_b == q02_b)

//  f. check
drop sources
egen sources = rowtotal(get3 get5 get6)
tab sources consume, m // clean


gen get2=(consume) // define dummy shows consumption is recorded
/* ---- 2. Conversion of units ---------------------------------------------- */

numlabel HH_S5AQ02_B HH_S5AQ00, add
tab q00 q02_b

// STEP 1: construct variables for the quantity consumed from each source (and overall) in standardized units
//         (the prices I constructed are per 100g, so it's easiest if you use that here as well)
//         (for example, quant_purch or quant3 could be the quantity purchased in 100 g)

//  a. construct quantity quantity consumed from each source (and overall) in standardized units

local source_code 2 3 5 6
foreach i of local source_code {
       gen quant`i'=10*q0`i'_a if q0`i'_b==20
         replace quant`i'=0.01*q0`i'_a if q0`i'_b==1
         replace quant`i'=0.01032*q0`i'_a  if q0`i'_b==3 & q00==19
         replace quant`i'=0.01032*q0`i'_a  if q0`i'_b==3 & q00==19
         replace quant`i'=0.5*q0`i'_a  if q0`i'_b==4 & q00==21
         replace quant`i'=1.27*q0`i'_a  if q0`i'_b==4 & q00==15
         replace quant`i'=2.4*q0`i'_a  if q0`i'_b==31 & q00==19
         replace quant`i'=2.45*q0`i'_a  if q0`i'_b==31 & q00==20
         replace quant`i'=2.25*q0`i'_a  if q0`i'_b==31 & q00==24
         replace quant`i'=10.32*q0`i'_a  if q0`i'_b==32 & q00==19
         replace quant`i'=10.4*q0`i'_a  if q0`i'_b==32 & q00==20
		 replace quant`i'=. if get`i'==0
}

//  b. check missing values of quantity consumed from each source where it appears household consumed from given source. 

local source_code 2 3 5 6
foreach i of local source_code {
         count if get`i'
		 count if quant`i'<.	 
}

//  c. check unit variables if quantity from purchased is undefined where it appears household consumed from purchased. ( Repeat it for all source of consumption)

local source_code 2 3 5 6
foreach i of local source_code {
         tab q0`i'_b if get`i'==1 & quant`i'==., m	 
}


//  ADD CODE HERE for further cleaning of quantities, such as replace amount from 
//         own production with total quantity if quantity from own production is missing
//         and it appears the household only consumed from own production


//  d. replace amount from purchased with total quantity if quantity from purchased is missing
//         and it appears the household only consumed from purchased

// flag corresponding observations
gen flag2=. 
local source_code  3 5 6
foreach i of local source_code {
         replace flag2=1 if get2==1 & get`i'==1 & source==1	 & quant`i'==. & quant2!=.
}

// replace corresponding observations
local source_code 3 5 6
foreach i of local source_code {
         replace quant`i'=quant2 if get2==1 & get`i'==1 & source==1	 & quant`i'==.
}

//  e. construct a variable identified adjusted total quantities based on quant3, quant5 and quant6

egen quant_total=rsum(quant3 quant5 quant6)
replace quant_total=. if quant3==. & quant5==. & quant6==.  // do not define it when at least one of the quant`i' is missing.
gen flag=(quant_total != quant2)  // flag observations where there is mismatch between self-reported and constructed total quantity.


//  f. replace adjusted total quantitiy reported with adjusted amount from a given source if adjusted total quantitiy is missing, unadjusted quantities for total consumption (reported) and amount from given source are equal and it appears the household only consumed from a given source

local source_code 3 5 6
foreach i of local source_code {
         replace quant2=quant`i' if q02_a==q0`i'_a & get2==1 & get`i'==1 & source==1 & quant`i'!=. & quant2==.
}

*browse if quant2==. & get2==1 & flag==1

//  g. replace adjusted total quantitiy with adjusted amount from a given source if adjusted total quantitiy is missing, adjusted total quntities reported are not equal to adjusted total quantities constructed and it appears the household only consumed from a given source

local source_code 3 5 6
foreach i of local source_code {
         replace quant2=quant`i' if  get`i'==1 & quant`i'!=. & get2==1  & flag==1  & quant2==. & source==1
}

*browse if quant2==. & get2==1 & flag==1 // unadressed observations - 4 obs

//  h. reconstruct a variable identified adjusted total quantities based on quant3, quant5 and quant6

drop flag quant_total
egen quant_total=rsum(quant3 quant5 quant6)
replace quant_total=. if quant3==. & quant5==. & quant6==.  // do not define it when at least one of the quant`i' is missing.
gen flag=(quant_total != quant2)  // flag observations where there is mismatch between self-reported and constructed total quantity.

tab sources if flag==1

// i. adress mismatch between total quantity unit (reported) and  unit (reported) from a given source if household only consumed from a given source

// flag observations
gen unit_mismatch=.
local source_code 3 5 6
foreach i of local source_code {
      replace unit_mismatch=1 if q02_b!=q0`i'_b & get`i'==1 & source==1 & q02_b!=. & q0`i'_b!=.
}

// check observations
*browse if flag==1 & unit_mismatch==. // check cases where the unmatch between reported total quantity (adj) and  constructed total quantity is because of possible mismatch between units. 
*browse if  quant2!=. & quant_total==. // 4 observations where reported quantity (adj) is define while the constructed quantity(adj) is unavailable 
*browse if quant2!=. & quant_total==. // 5 observations where constructed quantity (adj) is define while the reported quantity(adj)  is unavailable 

// flag  observations where unit for consumption from given source are recorded as kg rather than gram
gen unplausible=.
local source_code 3 5 6
foreach i of local source_code {
         replace  unplausible=1 if (quant_total==10*quant2 | quant_total==100*quant2 | quant_total==1000*quant2) & flag==1 & unit_mismatch==1 & source==1
}

// replace amount of consumption from given source with total quantity if the unit for consumption from given source are recorded as kg rather than gram
local source_code 3 5 6
foreach i of local source_code {
         replace  quant`i'=quant2 if unplausible==1 & q0`i'_b==20 & get`i'==1
}

// flag  observations where total consumption are recorded as kg rather than gram
gen unplausible2=.
local source_code 3 5 6
foreach i of local source_code {
         replace  unplausible2=1 if (quant2==10*quant_total | quant2==100*quant_total | quant2==1000*quant_total) & flag==1 & unit_mismatch==1 & source==1
}

// replace total quantity  with amount of consumption from given source if the total quantity are recorded as kg rather than gram
local source_code 3 5 6
foreach i of local source_code {
         replace quant2 = quant`i' if unplausible2==1 & q02_b==20 & get`i'==1
}

//  j. reconstruct a variable identified adjusted total quantities based on quant3, quant5 and quant6

drop flag quant_total
egen quant_total=rsum(quant3 quant5 quant6)
replace quant_total=. if quant3==. & quant5==. & quant6==.  // do not define it when at least one of the quant`i' is missing.
gen flag=(quant_total != quant2)  // flag observations where there is mismatch between self-reported and constructed total quantity.

tab sources if flag==1

// flag  observations where it seems that the unconsumed quantities are recorded under different sources. //124 observations
gen unplausible_doubled=. 
local source_code 2 3
foreach i of local source_code {
         replace  unplausible_doubled=1 if quant_total==`i'*quant2 & flag==1 & source==`i'
}


/* ---- 3. Deal with outliers and missing in quant2 ------------------------- */
// I wrote this with quant2 = total quantity consumed in 100g
// if this is not what you constructed in section A2 above, adjust the code to reflect that

count if quant2 == 0 & consume
count if quant2 == . & consume
count if quant2 > 0 & quant2 < . & !consume // this is all how we want to begin
count if quant2 > 0 & quant2 < . & !consume // this is all how we want to begin
count if quant2 > 0 & quant2 < . & consume // this is all how we want to begin


//  a. global lower bounds on total quantity
tabstat quant2 [aw = pw2] if consume, s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (1/11 17 26 = 1) (12 13 22 23 24 25 = 0.1) (14 15 16 18 19 20 21 = 0.5), gen(lower) // global total hh lower bound for quantity

//  b. global upper bound for per capita quantity
gen pc_q2 = quant2/hhsize
tabstat pc_q2   [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
*table q00 rural [aw = pw2], c(p99 pc_q2) f(%10.0fc)
*table q00 rural [aw = pw2], c(p95 pc_q2) f(%10.0fc)
recode q00 (1/5 16 17 26 19 25 = 100) (6/11 14 15 20 = 50) (12/13 22 = 20) (21 = 30) (23 24 = 10), gen(upper) // global per capita upper bound for quantity

//  c. median 
bys q00 rural: egen med_pc_q2 = wpctile(pc_q2), p(50) w(pw2)

//  d. lower bound
bys q00 rural: egen low_q2 = wpctile(quant2), p( 1) w(pw2)
replace low_q2 = max(lower, low_q2/2) // set lower bound at max of global lower limit and half the 1st percentile
replace quant2 = med_pc_q2 * hhsize if quant2 < low_q2 & quant2 < . & consume  


//  e. upper bound
bys q00 rural: egen up_pc_q2 = wpctile(pc_q2), p(99) w(pw2) 
replace up_pc_q2 = min(upper, 2*up_pc_q2) // set upper bound at min of global upper limit and twice the 99th percentile
replace quant2 = med_pc_q2 * hhsize if pc_q2 > up_pc_q2 & quant2 < . & consume 

//  f. missing values (no zero values, checked above)
replace quant2 = med_pc_q2 * hhsize if quant2 == . & consume
tabstat quant2 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
gen a = 0 if !consume  // fill in quant2 with 0 when no consumption

/* -------------------------------------------------------------------------- */
/*          B. Food Purchases                                                 */
/* -------------------------------------------------------------------------- */

/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. missing and 0 values
count if q04 == . & get3
count if q04 == 0 & get3
replace q04 = . if q04 == 0 & get3
count if q04 > 0 & q04 < . & !get3
count if q04 == 0 & !get3
replace q04 = . if !get3
// don't make any real changes to the original variable q04 in this section, could mess up the construction of prices
clonevar value_purch=q04 // clone q04 and made corrections on this variable.
assert value_purch==q04
 
//  b. global lower bounds on values
tabstat value_purch [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (3 5 8 9 12/17 20 22/23 =1) (2 4 6 7 10 11 19 21 24 26 = 2) (1 18 25 = 5), gen(lower_value_purch) // global total hh lower bound for values

//  c. global upper bound for per capita values
gen pc_value_purch = value_purch/hhsize
tabstat pc_value_purch  [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00  (18 = 250) (1 25 = 200) (2 5 17 19 = 100) (3/4 6/12 15 20/22 24 26 = 50) (13/14 16 = 20) (23 = 10), gen(upper_value_purch) // global per capita upper bound for values

//  d. median 
bys q00 rural: egen med_pc_value_purch = wpctile(pc_value_purch), p(50) w(pw2)

/* ---- 2. Address missing and outlier values ------------------------------- */
//  a. lower bound and lower outliers
bys q00 rural: egen low_bar_purch = wpctile(value_purch), p( 1) w(pw2)
replace low_bar_purch = max(lower_value_purch, low_bar_purch/2) // set lower bound at max of global lower limit and half the 1st percentile
replace value_purch = med_pc_value_purch * hhsize if value_purch < low_bar_purch  & value_purch < . & get3==1

//  b. upper bound and upper outliers
bys q00 rural: egen high_bar_purch = wpctile(pc_value_purch), p(99) w(pw2) 
replace high_bar_purch = min(upper_value_purch, 2*high_bar_purch) // set upper bound at min of global upper limit and twice the 99th percentile
replace value_purch = med_pc_value_purch * hhsize if pc_value_purch > high_bar_purch & value_purch < . & get3==1

//  c. missing values (no zero values, checked above)
replace value_purch = med_pc_value_purch * hhsize if value_purch == . & get3==1
tabstat value_purch [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)

//  d. check final distribution
tabstat value_purch [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
gen lfp = log(value_purch)
*graph hbox lfp [aw = pw2], over(q00)
replace value_purch = 0 if !get3

rename value_purch food_purch
/* ---- 3. Deal with outliers and missing in quant3 ------------------------- */

count if quant3 == 0 & get3 // 6 obs
list if quant3 == 0 & get3
replace quant3=. if quant3 == 0 & get3 // observations where quant recorded as 0 while the hh claim non-zero spending.
count if quant3 == . & get3 // 1751 obs
count if quant3 > 0 & quant3 < . & !get3 // no observation
count if quant3 == 0  & !get3 // no observation

//  a. global lower bounds on quantity from purchased
tabstat quant3 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (1/11 17 26 = 1) (12 13 22 23 24 25 = 0.1) (14 15 16 18 19 20 21 = 0.5), gen(lower_q3) // global total hh lower bound for quantity from purchased

//  b. global upper bound for per capita from purchased
gen pc_q3 = quant3/hhsize
tabstat pc_q3   [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (1/5 16 17 26 19 25 = 100) (6/11 14 15 18 20 = 50) (12/13 22 = 20) (21 = 30) (23 24 = 10), gen(upper_q3) // global per capita upper bound for quantity from purchased

//  c. median 
bys q00 rural: egen med_pc_q3 = wpctile(pc_q3), p(50) w(pw2)

//  d. lower bound
bys q00 rural: egen low_q3 = wpctile(quant3), p( 1) w(pw2)
replace low_q3 = max(lower_q3, low_q3/2) // set lower bound at max of global lower limit and half the 1st percentile
replace quant3 = med_pc_q3 * hhsize if quant3 < low_q3 & quant3 < . & get3

//  e. upper bound
bys q00 rural: egen up_pc_q3 = wpctile(pc_q3), p(99) w(pw2) 
replace up_pc_q3 = min(upper_q3, 2*up_pc_q3) // set upper bound at min of global upper limit and twice the 99th percentile
replace quant3 = med_pc_q3 * hhsize if pc_q3 > up_pc_q3 & quant3 < . & get3

//  f. missing values (no zero values, checked above)
replace quant3 = med_pc_q3 * hhsize if quant3 == . & get3
tabstat quant3 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
replace quant3 = 0 if !get3  // fill in quant3 with 0 when no consumption


/* -------------------------------------------------------------------------- */
/*          C. Construction of Prices                                         */
/* -------------------------------------------------------------------------- */

//  STEP 3: 
//  a. construct unit costs based on food_purch and quant3 for each household claimed expenditure

gen unit_cost= food_purch/quant3 if get3==1 //
  tab get3
  count if unit_cost<.

clonevar item = q00 // define item 

//  b. make sure hierarchy variables are fully coded
count if saq07<.
count if saq06<.
count if saq02<.
count if saq01<.
count if rural<.

//  c. construct median unit prices for each hierarchy

bys item:       egen price_national  = wpctile(unit_cost), p(50) w(pw2)
bys item rural: egen price_rural     = wpctile(unit_cost), p(50) w(pw2)
bys item saq01: egen price_region    = wpctile(unit_cost), p(50) w(pw2)
bys item saq02: egen price_zone      = wpctile(unit_cost), p(50) w(pw2)
bys item saq06: egen price_kebele    = wpctile(unit_cost), p(50) w(pw2)


//  d. identify number of observations used to construct median levels across hierarchy
bys item rural:  egen n_rural      = count(unit_cost)
bys item saq01:  egen n_region     = count(unit_cost)
bys item saq02:  egen n_zone       = count(unit_cost)
bys item saq06:  egen n_kebele     = count(unit_cost)


//  e. check number of observations used to construct median levels across hierarchy
*table ( item ) ( rural ), statistic(mean  n_rural)
*table ( item ) ( saq01 ), statistic(mean  n_region)
*table ( item ) ( saq02 ), statistic(mean  n_zone)
*table ( item ) ( saq06 ), statistic(mean  n_kebele)

//  f. check distribution of unit_prices
/*
gen ln_unit_cost=log(unit_cost)
graph hbox ln_unit_cost [aw = pw2] if inlist(q00, 2, 3, 4, 5, 6, 16, 17), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)		   

graph hbox ln_unit_cost [aw = pw2] if inlist(q00, 7, 8, 12, 14, 15, 19, 21, 23), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)	
		     
graph hbox ln_unit_cost [aw = pw2] if inlist(q00, 18, 24, 25), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)		 
			   
graph hbox ln_unit_cost [aw = pw2] if inlist(q00, 2, 3, 4, 5, 6, 16, 17), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)		   
*/		  	 

//  f. construct local prices
gen price_local     = price_national
gen level2=0 // identify which hierarchy used to construct local price

replace level2=1 if n_rural>25 // identify which hierarchy used to construct local price
replace price_local = price_rural  if n_rural>25

replace level2=2 if n_region>25 // identify which hierarchy used to construct local price
replace price_local = price_region if n_region>25

replace level2=3 if n_zone>25 // identify which hierarchy used to construct local price
replace price_local = price_zone   if n_zone>25

replace level2=4 if n_kebele>25 // identify which hierarchy used to construct local price
replace price_local = price_kebele if n_kebele>25



merge m:1 ea_id2 q00 using "${input}foodprices.dta", keepusing(local_price level) assert(match) nogen


/* ---- 9. Look at distribution of constructed prices ----------------------- */

/*
graph hbox price_local [aw = pw2] if inlist(q00, 2, 3, 4, 5, 6, 16, 17), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)
graph hbox price_local [aw = pw2] if inlist(q00, 7, 8, 12, 14, 15, 19, 21, 23), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)
graph hbox price_local [aw = pw2] if inlist(q00, 1, 9, 10, 11, 13, 20, 22, 26), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)
graph hbox price_local [aw = pw2] if inlist(q00, 18, 24, 25), ///
           over(rural) asyvar over(q00) legend(row(1) symx(*0.5) span)
*/

// The median prices that I constructed are almost the same as the prices in the "foodprices" dataset. However, the levels used to assign local_prices are quite different. Therefore I decided to use the local prices that you gave to us.

/* -------------------------------------------------------------------------- */
/*          D. Value of Own Production                                        */
/* -------------------------------------------------------------------------- */

/* ---- 1. Investigate missing and outlier values: own production ----------- */

//  a. identify missing and 0 values
count if quant5 == 0 & get5
replace quant5=. if  quant5 == 0 & get5
count if quant5 == . & get5
count if quant5 > 0 & quant5 < . & !get5 // this is all how we want to begin
count if quant5 == 0 & !get5


//  b. global lower bounds on quantity from own production 
tabstat quant5 [aw = pw2] , s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (1 2 4/6 16 17 18 23 = 2) (3 7/11 15 19 20  = 0.5) (13 21 22 24 25 = 0.1) (12 14 26= 0.01),  gen(lower_q5) // global total hh lower bound for quantity


//  c. global upper bound for per capita quantity from own production 
gen pc_q5 = quant5/hhsize
tabstat pc_q5   [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
*table q00 rural [aw = pw2], c(p99 pc_q2) f(%10.0fc)
*table q00 rural [aw = pw2], c(p95 pc_q2) f(%10.0fc)
recode q00 (1/5 16 17  26 19 = 100) (6/9 11 15 18 20 22 25 = 50) (10 12 14 24= 20)  (13 21 23 = 10), gen(upper_q5) // global per capita upper bound for quantity


//  d. median 
bys q00: egen med_pc_q5 = wpctile(pc_q5), p(50) w(pw2)  // I decided to use national level median values since there is not enough observation

/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. lower bound
bys q00: egen low_q5 = wpctile(quant5), p( 1) w(pw2)
replace low_q5 = max(lower_q5, low_q5/2) // set lower bound at max of global lower limit and half the 1st percentile
replace quant5 = med_pc_q5 * hhsize if quant5 < low_q5 & quant5 < . & get5

//  b. upper bound
bys q00: egen up_pc_q5 = wpctile(pc_q5), p(99) w(pw2) 
replace up_pc_q5 = min(upper_q5, 2*up_pc_q5) // set upper bound at min of global upper limit and twice the 99th percentile
replace quant5 = med_pc_q5 * hhsize if pc_q5 > up_pc_q5 & quant5 < . & get5

//  c. missing values (no zero values, checked above)
replace quant5 = med_pc_q5 * hhsize if quant5 == . & get5
tabstat quant5 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
replace quant5 = 0 if !get5  // fill in quant5 with 0 when no consumption

/* ---- 3. Construct value of own production consumed ----------------------- */

//  STEP 1: EDIT CODE HERE
gen food_own =  quant5*local_price
count if food_own==.
count if food_own==0 & quant5>0
count if food_own!=0 & quant5==0

/* ---- 4. Check distribution ----------------------------------------------- */

tabstat food_own [aw = pw2] if get5, s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00) f(%10.0fc)


/* ---- 5. Any additional adjustments --------------------------------------- */

//  ADD CODE HERE (optional)


/* -------------------------------------------------------------------------- */
/*          E. Value of Gifts etc                                             */
/* -------------------------------------------------------------------------- */

/* ---- 1. Investigate missing and outlier values: gifts -------------------- */

//  a. identify missing and 0 values
count if quant6 == 0 & get6
replace quant6=. if  quant6 == 0 & get6
count if quant6 == . & get6
count if quant6 > 0 & quant6 < . & !get6 // this is all how we want to begin
count if quant6 == 0 & !get6


//  b. global lower bounds on quantity from gifts
tabstat quant6 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
recode q00 (1/6 16 17 = 2) (7/11 15 18/20  = 1) (13 21 22 24 25 = 0.5) (12 14 26= 0.01),  gen(lower_q6) // global total hh lower bound for quantity

//  c. global upper bound for per capita quantity from gifts
gen pc_q6 = quant6/hhsize 
tabstat pc_q6   [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
*table q00 rural [aw = pw2], c(p99 pc_q2) f(%10.0fc)
*table q00 rural [aw = pw2], c(p95 pc_q2) f(%10.0fc)
recode q00 (2 = 150) (1 3/6 9 14 16 22 26 = 100) (7/8 10/11 13 17/18 23/24  19 = 50) (15 20 25 = 15) (12 21 = 10), gen(upper_q6) // global per capita upper bound for quantity

//  d. median 
bys q00: egen med_pc_q6 = wpctile(pc_q6), p(50) w(pw2)  // I decided to use national level median values since there is not enough observation

/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. lower bound
bys q00: egen low_q6 = wpctile(quant6), p( 1) w(pw2)
replace low_q6 = max(lower_q6, low_q6/2) // set lower bound at max of global lower limit and half the 1st percentile
replace quant6 = med_pc_q6 * hhsize if quant6 < low_q6  & quant6 < . & get6

//  b. upper bound
bys q00: egen up_pc_q6 = wpctile(pc_q6), p(99) w(pw2) 
replace up_pc_q6 = min(upper_q6, 2*up_pc_q6) // set upper bound at min of global upper limit and twice the 99th percentile
replace quant6 = med_pc_q6 * hhsize if pc_q6 > up_pc_q6 & quant6 < . & get6

//  c. missing values (no zero values, checked above)
replace quant6 = med_pc_q6 * hhsize if quant6 == . & get6
tabstat quant6 [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00)
replace quant6 = 0 if !get6  // fill in quant6 with 0 when no consumption



/* ---- 3. Construct value of gifts etc consumed ---------------------------- */

//  STEP 1: EDIT CODE HERE
gen food_gift = quant6*local_price

count if food_gift==.
count if food_gift==0 & quant6>0
count if food_gift!=0 & quant6==0

/* ---- 4. Check distribution ----------------------------------------------- */

tabstat food_gift [aw = pw2] if get6, s(min p1 p5 p25 p50 p75 p95 p99 max) by(q00) f(%10.0fc)


/* ---- 5. Any additional adjustments --------------------------------------- */

//  ADD CODE HERE (optional)


 
/* -------------------------------------------------------------------------- */
/*          F. Finalize                                                       */
/* -------------------------------------------------------------------------- */

/* ---- 1. Annualize -------------------------------------------------------- */

//  STEP 1: 

foreach var of varlist  food_purch food_own food_gift {
        replace `var'=`var'*(365/7) 
}


/* ---- 2. Save household-item level data ----------------------------------- */

preserve
//  a. total quantity per household and item
//  STEP 1: ADD /EDIT CODE HERE
egen qh = rowtotal(quant3 quant5 quant6)  // total quantity (in 100 g probably) consumed by household (7 days)
replace qh=qh*(365/7)    // total quantity (in 100 g probably) consumed by household (annualized)
egen vh = rowtotal(food*)    // total value of food item consumed by household (annualized)
drop if q00==25

//  b. keep only needed variables and save
keep ea_id2 household_id2 pw2 q00 hhsize qh vh
save "${temp}Food_quant_price.dta", replace
restore


/* ---- 3. Separate out kat ------------------------------------------------- */

//  STEP 1: 
egen flag_kat=rowtotal(food_*) if q00==25 // gen total value of kat consumption (annualized)
bysort household_id2: egen kat= total(flag_kat) // assign total value of kat consumption to each observations by hhid 
drop if q00==25

/* ---- 4. Collapse to one observation per household ------------------------ */

//  STEP 1: 

collapse (sum) food_purch food_gift food_own (mean) kat, by (household_id2 ea_id2)

isid household_id2
egen total_food = rowtotal(food_*)
gen ltf = log(total_food)
*histogram ltf, normal


/* ---- 5. Save ------------------------------------------------------------- */

//  a. keep only needed variables and label
keep ea_id2 household_id2 food_purch food_own food_gift kat
lab var food_purch "expenditure on food, annual"
lab var food_own   "value of food consumed from own production, annual"
lab var food_gift  "value of food consumed from gifts / other sources, annual"
lab var kat        "value of kat consumed, annual"

//  b. save to merge with food away from home
save "${temp}food1.dta", replace



/* -------------------------------------------------------------------------- */
/*          G. Food Away from Home                                            */
/* -------------------------------------------------------------------------- */


use "${raw}sect5d_hh_w2.dta", clear
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match) nogen // merge in hh size
rename hh_s5c* *


/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. identify missing and 0 values
count if q06 ==. // 168 observations are missing for filter question
tab q07 if q06==.

//  check observations where filter question is yes, but no consumption given
count if q07==. & q06==1 // no observation

//  check observations where filter question is yes, but consumption recorded as 0
count if q07==0 & q06==1 // 6 observation
replace q07=. if q07==0 & q06==1 // recode them as missing - obs came from two household.

//  check observations where filter question is no, consumption is given or recorded as 0
count if  q07 > 0 & q07 < . & q06==2 // no observation
count if q07==0 & q06==2 // no observation

clonevar value_fafh=q07 // clonevar value from FAFH
assert 	value_fafh==q07 	
																
//  b. global lower bounds on value from FAFH
encode q0b, gen(item) // encode q0b to gen item

tabstat value_fafh [aw = pw2] , s(min p1 p5 p25 p50 p75 p95 p99 max) by(item)
recode item (1 2 5 8 = 2) (3 = 3) (4 6 7 = 0.5),  gen(lower_fafh) // global total hh lower bound for value from FAFH

//  c. global upper bound for per capita value from FAFH
gen pc_fafh = value_fafh/hhsize
tabstat pc_fafh   [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(item)
recode item (2 3 = 150) (1 5 8  = 100) (4 6 7 = 50), gen(upper_fafh) // global per capita upper bound for value from FAFH

/* ---- 2. Address missing and outlier values ------------------------------- */

//  d. median 

*bys item: egen med_pc_fafh= wpctile(pc_fafh), p(50) w(pw2)  // I decided to use rural level median values since there are enough observation and values significantly different across this level.
/*graph hbox med_pc_fafh_rural [aw = pw2] if inlist(item, 2, 3, 4, 5, 6, 16, 17), /// 
           over(rural) asyvar over(item) legend(row(1) symx(*0.5) span)
*/

*tab item rural if q06==1
bys item rural: egen med_pc_fafh= wpctile(pc_fafh), p(50) w(pw2)   // gen rural level median value

/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. lower bound
bys item: egen low_fafh = wpctile(value_fafh), p( 1) w(pw2)
replace low_fafh = max(lower_fafh, low_fafh/2) // set lower bound at max of global lower limit and half the 1st percentile
replace value_fafh = med_pc_fafh * hhsize if value_fafh < low_fafh  & value_fafh < . & q06==1

//  b. upper bound
bys item: egen up_pc_fafh = wpctile(pc_fafh), p(99) w(pw2) 
replace up_pc_fafh = min(upper_fafh, 2*up_pc_fafh) // set upper bound at min of global upper limit and twice the 99th percentile
replace value_fafh = med_pc_fafh * hhsize if pc_fafh > up_pc_fafh & value_fafh < . & q06==1

//  c. missing values (no zero values, checked above)
replace value_fafh = med_pc_fafh * hhsize if value_fafh == .  & q06==1
tabstat value_fafh [aw = pw2], s(min p1 p5 p25 p50 p75 p95 p99 max) by(item)
replace value_fafh = 0 if  q06!=1  // fill in value_fafh with 0 when no consumption


//  a. initial fafh
//  STEP 1: EDIT CODE HERE
clonevar food_fafh = value_fafh
count if food_fafh==. 
count if food_fafh==. & q06==1 
count if food_fafh==0 & q06==1 


/* ---- 3. Finalize --------------------------------------------------------- */

//  a. annualize expenditure
//  STEP 1: 
replace  food_fafh=food_fafh*(365/7)

//  b. collapse to one observation per household
//  STEP 1: 

collapse (sum) food_fafh, by (household_id2 ea_id2)

isid household_id2

/*/ check distribution
gen ltf_fafh = log(food_fafh)
histogram ltf_fafh, normal
drop ltf_fafh
*/

/* -------------------------------------------------------------------------- */
/*          H. Combine                                                        */
/* -------------------------------------------------------------------------- */

/* ---- 1. Merge with other food consumption/expenditure -------------------- */

merge 1:1 household_id2 using "${temp}food1.dta"
drop _merge

/* ---- 2. Check overall distribution --------------------------------------- */

//  a. total food consumption
//  STEP 1: 

egen total_food = rowtotal(food_*)

// check observations where total food consumption is 0
count if total_food==0
egen min_total_food = min(total_food)
replace total_food=min_total_food if total_food==0 // use value of min consumption to impute them.
drop min_total_food

//  b. log normal-ish?
gen ltf = log(total_food)
*histogram ltf, normal

//  ADD CODE HERE FOR ANY OTHER CHECKS (optional)

/* ---- 3. Save ------------------------------------------------------------- */

//  a. keep and label
keep household_id2 ea_id2 food_* kat total_food
lab var food_fafh "expenditure on FAFH, annual"
lab var total_food "total value of food purchases, own production, gifts and food away from home, annual"

 
 
//  b. save
save "${out}Cons_food.dta", replace


*_______________________________________________________________________________
***************************- END of do-file  -**********************************