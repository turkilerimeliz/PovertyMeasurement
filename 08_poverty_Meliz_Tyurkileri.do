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


/*Define local variables used for sensitivity checks-------------------------*/


/* Define deciles to construct reference population:
    decile2to7 - bottom70 - bottom50                                         */
local ref_pop = "decile2to7" // reference population for food basket

* Define adult equivalence scale:   pacific vs. dz                                		 
local household_ame= "pacific" 

* Define a target_calorie used to construct food_basket 
local target_calories = 2300 // kilocalories per adult equivalent per day

* Define nonfood_component used in Ravallion method: excludeHouse vs. all                                       
local nonfood_component = "all"


/* -------------------------------------------------------------------------- */
/*          A. Consumption Aggregates, Welfare                                */
/* -------------------------------------------------------------------------- */

/* ---- 1. Combine different datasets --------------------------------------- */

use "${out}Cons_hhcomp.dta",  clear

merge 1:1 household_id2 ea_id2 using "${out}Cons_food.dta", assert(3) gen(merge_food)
merge 1:1 household_id2 ea_id2 using "${out}Cons_nonfood.dta", assert(3) gen(merge_nonfood)
merge 1:1 household_id2 ea_id2 using "${out}Cons_durables.dta", assert(3) gen(merge_durables)
merge 1:1 household_id2 ea_id2 using "${out}Cons_education.dta", assert(3) gen(merge_educ)
merge 1:1 household_id2 ea_id2 using "${out}Cons_housing.dta", assert(3) gen(merge_house)
merge 1:1 household_id2 ea_id2 using "${out}Cons_price.dta", assert(3) gen(merge_price)

drop merge*
/* ---- 2. Construct aggregate variables ------------------------------------ */

if "`household_ame'" == "dz" gen hh_ame=es_dz
if "`household_ame'" == "pacific" gen hh_ame=hhame


egen consexp=rowtotal(total_food nonfood durables education housing_rent housing_usevalue)
gen pchhexp=(consexp/pi_spatial)/hh_size

gen welfare_food=(total_food/pi_spatial)/hh_ame
gen welfare=(consexp/pi_spatial)/hh_ame


// Construct aggregate variables used to compute non_food components of the poverty line
egen consexp_nonfood=rowtotal (nonfood durables education housing_rent housing_usevalue) // include all non_food components
egen consexp_nonfood2=rowtotal (nonfood durables education) // exclude housing 
*egen consexp_nonfood3=rowtotal (nonfood durables) // include only nonfood and durables ( I did not use this version)

gen welfare_nonfood=(consexp_nonfood/pi_spatial)/hh_ame
gen welfare_nonfood2=(consexp_nonfood2/pi_spatial)/hh_ame
*gen welfare_nonfood3=(consexp_nonfood3/pi_spatial)/hh_ame ( I did not use this version)

/*
/* ---- 3. Check and Save --------------------------------------------------- */

// a. Check distribution of total nominal, annual household consumption
gen lconsexp = log(consexp)
histogram lconsexp, normal

// b. Check distribution of per capita, real annual household consumption/expenditure
gen lpchhexp = log(pchhexp)
histogram lpchhexp, normal

// c. Check distribution of per adult equivalent, real annual total household food consumption/expenditure
gen lwelfare_food = log(welfare_food)
histogram lwelfare_food, normal

// d. Check distribution of per adult equivalent, real annual total household consumption/expenditure
gen lwelfare_all = log(welfare)
histogram lwelfare_all, normal

// e. Check distribution of per adult equivalent, real annual total household nonfood consumption/expenditure

// include all non_food components
gen lwelfare_nonfood = log(welfare_nonfood)
histogram lwelfare_nonfood, normal

// exclude housing
gen lwelfare_nonfood2 = log(welfare_nonfood2)
histogram lwelfare_nonfood2, normal

// include only nonfood and durables
gen lwelfare_nonfood3 = log(welfare_nonfood3)
histogram lwelfare_nonfood3, normal

*/

// e. Drop unnecessary variables and save
keep household_id2 ea_id2 hh_size hh_ame hhame es_dz pw2 rural saq01 saq07 consexp pchhexp welfare_food welfare welfare_nonfood welfare_nonfood2 

label var consexp "total nominal, annual household consumption"
label var pchhexp "per capita, real annual household consumption/expenditure"
label var welfare_food "per adult equivalent, real annual total household food consumption/expenditure"
label var welfare "per adult equivalent, real annual total household consumption/expenditure"
label var welfare_nonfood "per adult equivalent, real annual total household non_food consumption/expenditure"
label var welfare_nonfood2 "per adult equivalent, real annual total household non_food consumption/expenditure, excluded housing"

save "${out}Cons.dta", replace


/* -------------------------------------------------------------------------- */
/*          B. Food Poverty Line                                              */
/* -------------------------------------------------------------------------- */

tempfile calories refhh basket national region rural


/* ---- 0. Prep calorie information ----------------------------------------- */

clear
import excel using "${input}calories and NSU.xlsx", sheet(calories) firstrow
drop in 27/29  // drop unrelated lines
destring code, gen (q00) // covert string var to numeric
drop code
drop if q00==25 // exclude kat
save `calories'


/* ---- 1. Identify reference population ------------------------------------ */
// 10th to 70th pctile in terms of real, per AME food consumption



use "${out}Cons.dta", clear

//  a. construct deciles of food consumption
gen pop_weight=hh_size*pw2
xtile ndecile = welfare_food[aw=pop_weight], nq(10)

//  b. keep only households from 10th to 70th pctile
if "`ref_pop'" == "decile2to7" keep if ndecile>=2 &  ndecile<=7 & ndecile!=.
if "`ref_pop'" == "bottom70" keep if ndecile>=1 &  ndecile<=7 & ndecile!=.
if "`ref_pop'" == "bottom50" keep if ndecile>=1 &  ndecile<=5 & ndecile!=.

//  c. calculate total AME of reference households 

total hh_ame [pw = pw2] 
    matrix A = r(table)
    local tt_hhame = A[1, 1]
    scalar total_hhame= 1*`tt_hhame'
	
save `refhh'

/* ---- 2. Construct food basket -------------------------------------------- */

use "${temp}Food_quant_price.dta", clear

//  a. keep only reference households

merge m:1 household_id2 ea_id2 using `refhh', nogen 
drop rural saq01 hh_size hhame es_dz hh_ame consexp pchhexp welfare_food welfare pop_weight 
drop if ndecile==. // keep only reference households

//  b. collapse to item level with total daily consumption of each food item

collapse (sum) qh [pw = pw2], by(q00)
gen quant_day_ame = qh / (365 * total_hhame)

//  c. merge in calorie information

merge 1:1 q00 using `calories', nogen

//  d. total calories from each food and overall

gen calories = quant_day_ame * kcalper100g
total calories
matrix B = r(table)
local actual_calories = B[1, 1]

//  e. adjust to 2300 calories per AME per day

gen basket_quant = quant_day_ame * `target_calories' / `actual_calories'

save `basket'

/* ---- 3. Price food basket ------------------------------------------------ */

use "${input}foodprices.dta", clear

// a. Construct a dataset reported national prices along with item numbers

collapse (mean) national_price, by(q00)
drop if q00==25 // exclude kat

//  b. merge in quantities of basket
merge 1:1 q00 using `basket', nogen

//  c. calculate total annual cost per AME

gen basket_cost_perday  = basket_quant * national_price
total basket_cost_perday
matrix C = r(table)
local food_poverty_line_perday = C[1, 1]

scalar food_pov_line = 365*`food_poverty_line_perday' // define food poverty line

// I saved below dataset for documentation purpose.
preserve
keep q00 kcalper100g national_price basket_quant basket_cost_perday
save "${out}Food_basket_price.dta", replace
restore

/* -------------------------------------------------------------------------- */
/*          C. Total Poverty Line                                             */
/* -------------------------------------------------------------------------- */

use "${out}Cons.dta", clear

/* ---- 1. Use Ravallion to get nonfood component --------------------------- */

// Non-food component - include all non_food aggregates

matrix RAV = J(1, 10, .) 
forval x = 1/10 {    
	local upper = food_pov_line * `=1+(`x'/100)'
	local lower = food_pov_line * `=1-(`x'/100)'
	sum welfare_nonfood [aw = pw2] if inrange(welfare_food, `lower', `upper') 
	matrix RAV[1, `x'] = r(mean)
}
matrix D = J(10, 1, 1)
matrix E = (RAV * D)/10 // averages over the matrix

scalar nonfood_allowance_all = 1* E[1,1]

// Non-food component - exclude housing

matrix RAV2 = J(1, 10, .) 
forval x = 1/10 {    
	local upper = food_pov_line * `=1+(`x'/100)'
	local lower = food_pov_line * `=1-(`x'/100)'
	sum welfare_nonfood2 [aw = pw2] if inrange(welfare_food, `lower', `upper') 
	matrix RAV2[1, `x'] = r(mean)
}
matrix D2 = J(10, 1, 1)
matrix E2 = (RAV2 * D2)/10 // averages over the matrix

scalar nonfood_allowance_nonh = 1* E2[1,1]


/* ---- 2. Add to food poverty line ----------------------------------------- */

scalar pov_line_all = food_pov_line + nonfood_allowance_all // non_food item includes all components
scalar pov_line_nonh = food_pov_line + nonfood_allowance_nonh // non_food item exludes housing expenses


/* --- 3. Poverty rates ----------------------------------------------------- */
if "`nonfood_component'" == "all" gen pov_line=pov_line_all
if "`nonfood_component'" == "excludeHouse" gen pov_line=pov_line_nonh

//  a. povrty
gen poor_abs = welfare < pov_line
mean poor_abs [aw = pw2 * hh_size] // should be roughly in the 20-30% range.

//  b. extreme poverty
gen poor_extreme = welfare_food < food_pov_line
mean poor_extreme [aw = pw2 * hh_size] 


//  c. international poverty 
//  OPTIONAL: ADD CODE HERE

/* --- 4. Save as template -------------------------------------------------- */
// keep related variables
keep household_id2 ea_id2 rural pw2 saq01 saq07 hh_size hh_ame consexp pchhexp welfare_food welfare pov_line poor_abs poor_extreme

// label variables
label var hh_ame "household AME"
label var pov_line "estimated poverty line"
label var poor_abs "binary for whether the household is poor "
label var poor_abs "binary for whether the household is extremely poor "

save "${temp}Household_Poor.dta", replace

/* -------------------------------------------------------------------------- */
/*          D. Poverty Measures                                               */
/* -------------------------------------------------------------------------- */

/* -1. Define population by different levels of regional disaggregation ----- */

// a. define population weight 
gen pop_weight = pw2 * hh_size  
gen x=1 

// b. estimate population number
svyset saq07 [pweight = pop_weight], strata(saq01)
svy: total x
matrix F = e(b)
scalar population=1*F[1,1] 

// c. estimate population number by rural
svy: total x, over(rural)
matrix G = e(b)
scalar population_rural= G[1,1] 
scalar population_other= G[1,2] 
scalar population_urban= G[1,3]  

// d. estimate population number by region
svy: total x, over(saq01)
matrix I = e(b)
scalar population_tigray=I[1,1]
scalar population_afar=I[1,2]
scalar population_amhara=I[1,3]
scalar population_oromia=I[1,4]
scalar population_somalie=I[1,5]
scalar population_benshagul=I[1,6]
scalar population_SNNP=I[1,7]
scalar population_gambelia=I[1,8]
scalar population_harari=I[1,9]
scalar population_ababa=I[1,10]
scalar population_diredwa=I[1,11]



/* ---- 2. National level poverty estimates --------------------------------- */

//  a. Poverty
//   poverty rate
svy: mean poor_abs // should be roughly in the 20-30% range.
matrix J = e(b)
gen pov_rate=J[1,1]

//   poverty gap
gen poor_abs_gap = (pov_line-welfare)/pov_line if poor_abs==1
svy:total poor_abs_gap
matrix O = e(b)
gen pov_gap=O[1,1]/population

//   poverty severity
gen poor_abs_sev = ((pov_line-welfare)/pov_line)^2 if poor_abs==1
svy:total poor_abs_sev
matrix S = e(b)
gen pov_sev=S[1,1]/population


//  b. Extreme Poverty

//   poverty rate
svy: mean poor_extreme 
matrix J1 = e(b)
gen ext_pov_rate=J1[1,1]

//   poverty gap
gen poor_ext_gap = (food_pov_line-welfare_food)/food_pov_line if poor_extreme==1
svy:total poor_ext_gap
matrix O1 = e(b)
gen ext_pov_gap=O1[1,1]/population

//   poverty severity
gen poor_ext_sev = ((food_pov_line-welfare_food)/food_pov_line)^2 if poor_extreme==1
svy:total poor_ext_sev
matrix S1 = e(b)
gen ext_pov_sev=S1[1,1]/population

//  collapse poverty measures by national level 
preserve
collapse (mean) pov_line pov_rate pov_gap  pov_sev ext_pov_rate  ext_pov_gap  ext_pov_sev
gen region_name="National" 

save `national'
restore

/* ---- 3.  Urban-Rural level poverty estimates ----------------------------- */

//   a. Poverty 

//   poverty rate
svy: mean poor_abs, over(rural)
matrix K = e(b)
replace pov_rate=K[1,1] if rural==1
replace pov_rate=K[1,2] if rural==2
replace pov_rate=K[1,3] if rural==3


//  poverty gap 
svy: total poor_abs_gap, over(rural) 
matrix P = e(b)
replace pov_gap=P[1,1]/population_rural if rural==1
replace pov_gap=P[1,2]/population_other if rural==2
replace pov_gap=P[1,3]/population_urban if rural==3


//  poverty severity
svy: total poor_abs_sev, over(rural) 
matrix T = e(b)
replace pov_sev=T[1,1]/population_rural  if rural==1
replace pov_sev=T[1,2]/population_other  if rural==2
replace pov_sev=T[1,3]/population_urban  if rural==3


//   b. Extrem Poverty 

//   poverty rate
svy: mean poor_extreme, over(rural)
matrix K1 = e(b)
replace ext_pov_rate=K1[1,1] if rural==1
replace ext_pov_rate=K1[1,2] if rural==2
replace ext_pov_rate=K1[1,3] if rural==3


//  poverty gap 
svy: total poor_ext_gap, over(rural) 
matrix P1 = e(b)
replace ext_pov_gap=P1[1,1]/population_rural if rural==1
replace ext_pov_gap=P1[1,2]/population_other if rural==2
replace ext_pov_gap=P1[1,3]/population_urban if rural==3

//  poverty severity

svy: total poor_ext_sev, over(rural) 
matrix T1 = e(b)
replace ext_pov_sev=T1[1,1]/population_rural if rural==1
replace ext_pov_sev=T1[1,2]/population_other if rural==2
replace ext_pov_sev=T1[1,3]/population_urban if rural==3

//  collapse poverty measures by rural 
preserve
collapse (mean) pov_line pov_rate pov_gap  pov_sev ext_pov_rate  ext_pov_gap  ext_pov_sev, by(rural)
gen region_name="Rural" if rural==1
replace  region_name="Other Urban" if rural==2
replace  region_name="Urban" if rural==3
drop rural
save `rural'
restore


/* ---- 4.  Region level poverty estimates ---------------------------------- */


//   a. Poverty 
//   poverty rate

svy: mean poor_abs, over(saq01)
matrix L = e(b)
replace pov_rate=L[1,1] if saq01==1
replace pov_rate=L[1,2] if saq01==2
replace pov_rate=L[1,3] if saq01==3
replace pov_rate=L[1,4] if saq01==4
replace pov_rate=L[1,5] if saq01==5
replace pov_rate=L[1,6] if saq01==6
replace pov_rate=L[1,7] if saq01==7
replace pov_rate=L[1,8] if saq01==12
replace pov_rate=L[1,9] if saq01==13
replace pov_rate=L[1,10] if saq01==14
replace pov_rate=L[1,11] if saq01==15

//  poverty gap 

svy: total poor_abs_gap, over(saq01) 
matrix R = e(b)
replace pov_gap=R[1,1]/population_tigray if saq01==1
replace pov_gap=R[1,2]/population_afar if saq01==2
replace pov_gap=R[1,3]/population_amhara if saq01==3
replace pov_gap=R[1,4]/population_oromia if saq01==4
replace pov_gap=R[1,5]/population_somalie if saq01==5
replace pov_gap=R[1,6]/population_benshagul if saq01==6
replace pov_gap=R[1,7]/population_SNNP if saq01==7
replace pov_gap=R[1,8]/population_gambelia if saq01==12
replace pov_gap=R[1,9]/population_harari if saq01==13
replace pov_gap=R[1,10]/population_ababa if saq01==14
replace pov_gap=R[1,11]/population_diredwa if saq01==15

//  poverty severity
svy: total poor_abs_sev, over(saq01) 
matrix U = e(b)
replace pov_sev=U[1,1]/population_tigray if saq01==1
replace pov_sev=U[1,2]/population_afar if saq01==2
replace pov_sev=U[1,3]/population_amhara if saq01==3
replace pov_sev=U[1,4]/population_oromia if saq01==4
replace pov_sev=U[1,5]/population_somalie if saq01==5
replace pov_sev=U[1,6]/population_benshagul if saq01==6
replace pov_sev=U[1,7]/population_SNNP if saq01==7
replace pov_sev=U[1,8]/population_gambelia if saq01==12
replace pov_sev=U[1,9]/population_harari if saq01==13
replace pov_sev=U[1,10]/population_ababa if saq01==14
replace pov_sev=U[1,11]/population_diredwa if saq01==15

//   b. Extreme Poverty 
//   poverty rate

svy: mean poor_extreme, over(saq01)
matrix L1 = e(b)
replace ext_pov_rate=L1[1,1] if saq01==1
replace ext_pov_rate=L1[1,2] if saq01==2
replace ext_pov_rate=L1[1,3] if saq01==3
replace ext_pov_rate=L1[1,4] if saq01==4
replace ext_pov_rate=L1[1,5] if saq01==5
replace ext_pov_rate=L1[1,6] if saq01==6
replace ext_pov_rate=L1[1,7] if saq01==7
replace ext_pov_rate=L1[1,8] if saq01==12
replace ext_pov_rate=L1[1,9] if saq01==13
replace ext_pov_rate=L1[1,10] if saq01==14
replace ext_pov_rate=L1[1,11] if saq01==15

//  poverty gap 
svy: total poor_ext_gap, over(saq01) 
matrix R1 = e(b)
replace ext_pov_gap=R1[1,1]/population_tigray if saq01==1
replace ext_pov_gap=R1[1,2]/population_afar if saq01==2
replace ext_pov_gap=R1[1,3]/population_amhara if saq01==3
replace ext_pov_gap=R1[1,4]/population_oromia if saq01==4
replace ext_pov_gap=R1[1,5]/population_somalie if saq01==5
replace ext_pov_gap=R1[1,6]/population_benshagul if saq01==6
replace ext_pov_gap=R1[1,7]/population_SNNP if saq01==7
replace ext_pov_gap=R1[1,8]/population_gambelia if saq01==12
replace ext_pov_gap=R1[1,9]/population_harari if saq01==13
replace ext_pov_gap=R1[1,10]/population_ababa if saq01==14
replace ext_pov_gap=R1[1,11]/population_diredwa if saq01==15

//  poverty severity
svy: total poor_ext_sev, over(saq01) 
matrix U1 = e(b)
replace ext_pov_sev=U1[1,1]/population_tigray if saq01==1
replace ext_pov_sev=U1[1,2]/population_afar if saq01==2
replace ext_pov_sev=U1[1,3]/population_amhara if saq01==3
replace ext_pov_sev=U1[1,4]/population_oromia if saq01==4
replace ext_pov_sev=U1[1,5]/population_somalie if saq01==5
replace ext_pov_sev=U1[1,6]/population_benshagul if saq01==6
replace ext_pov_sev=U1[1,7]/population_SNNP if saq01==7
replace ext_pov_sev=U1[1,8]/population_gambelia if saq01==12
replace ext_pov_sev=U1[1,9]/population_harari if saq01==13
replace ext_pov_sev=U1[1,10]/population_ababa if saq01==14
replace ext_pov_sev=U1[1,11]/population_diredwa if saq01==15

//  collapse poverty measures by region

preserve
collapse (mean) pov_line pov_rate pov_gap  pov_sev ext_pov_rate  ext_pov_gap  ext_pov_sev, by(saq01)
decode saq01, gen(region_name)
drop saq01
save `region'
restore


/* ---- 5.  Append national-rural-regional level poverty measures------------ */

use `national', clear
append using `rural'
append using `region'
order region_name, first


// reporty poverty measures as percentage
foreach var of varlist pov_rate pov_gap pov_sev ext_pov_rate ext_pov_gap ext_pov_sev {
	replace `var'=`var'*100
	}
	
// label and save

label var region_name "Region or Level Name"
label var pov_line "Poverty Line"
label var pov_rate "Poverty Rate %"
label var pov_gap  "Poverty Gap %"
label var pov_sev  "Poverty severity %"
label var ext_pov_rate  "Extreme Poverty Rate %"
label var ext_pov_gap  "Extreme Poverty Gap %"
label var ext_pov_sev  "Extreme Poverty Severity %"

save "${out}PovertyMeasures.dta", replace

***************************- Additional Controls  -*****************************

/*
// double check using povdeco

use "${temp}Household_Poor.dta", clear
gen pop_weight=pw2*hh_size

povdeco welfare [aweight = pop_weight], pline(4075.8677)
povdeco welfare [aweight = pop_weight], pline(4075.8677) by(rural)
povdeco welfare [aweight = pop_weight], pline(4075.8677) by(saq01)


// double check using povdeco

povdeco welfare_food [aweight = pop_weight], pline(2925.5437)
povdeco welfare_food [aweight = pop_weight], pline(2925.5437) by(rural)
povdeco welfare_food [aweight = pop_weight], pline(2925.54377) by(saq01)

*/

*_______________________________________________________________________________
***************************- END of do-file  -**********************************

