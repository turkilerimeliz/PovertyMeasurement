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

/*Define local variables used for sensitivity checks--------------------------*/

/* (1) useActual: used actual rents reported by renters as a total housing consumption for these households
   (2) useImputed: used imputed rental values for all households even for renters.*/
                                          
local method_renters = "useActual" // reference population for food basket

/*----------------------------------------------------------------------------*/


/* -------------------------------------------------------------------------- */
/*          A. Section 6a: Actual Rent                                        */
/* -------------------------------------------------------------------------- */

use "${raw}sect6a_hh_w2.dta", clear
rename hh_* *

keep if s6aq00 == 12 // keep only rent
merge m:1 household_id2 using "${temp}hhsize.dta", assert(match) nogen // merge in hh size


/* ---- 1. Investigate missing and outlier values --------------------------- */

// a. flag outlier values and exclude them from the hedonistic rental regression

// identify missing values or unreasonable zeros
count if s6aq02 == 0 & s6aq01==1 // no observation
count if s6aq02 == 0 & s6aq01!=1 // no observation
count if s6aq02 == . & s6aq01==1 // only 1 missing observations
count if s6aq02 > 0 & s6aq02 < . & s6aq01!=1 // only 1  observations
count if s6aq02 ==. & s6aq01==. // 66 observations

// flag observations where we will use for usevalue model
gen flag=1
replace flag=0 if s6aq01==1 & s6aq02!=.

// b. investigate for outliers
gen lnrent=log(s6aq02)
*histogram lnrent [fw = round(pw2)], normal
tabstat s6aq02 [aw = pw2],  s(min p5 p50 p95 p99 max)

// c. construct 99th and 1st pctile values of rent 

// construct median level, lower and upper percentiles 
egen lower_rent   = wpctile(s6aq02), w(pw2) p(1)
egen median_rent   = wpctile(s6aq02), w(pw2) p(50) 
egen upper_rent   = wpctile(s6aq02), w(pw2) p(99) 


// d. flag outliers
// flag upper outliers 
replace flag=1 if s6aq02 > 2*upper_rent 

// flag lower outliers 
replace flag=1 if s6aq02 < lower_rent/2 

/* ---- 2. Save ------------------------------------------------------------- */

// a. keep only needed variables
keep household_id2 ea_id2 hhsize rural s6aq0a s6aq01 s6aq02 flag  median_rent 
tempfile rents
save `rents'


/* -------------------------------------------------------------------------- */
/*          B. Section 9: Housing Characteristics                             */
/* -------------------------------------------------------------------------- */

use "${raw}sect9_hh_w2.dta", clear
rename hh_s9* *


/* ---- 1. Preliminaries ---------------------------------------------------- */

//  a. merge in data on actual rents
merge 1:1 household_id2 using `rents', assert(match)
drop _merge

//  b. correspondence between report renting in section 9, and report paying rent in section 6
tab s6aq01 if q03==1 
  replace flag=1 if q03==1 & flag==0 // exclude hh occupied privately owned dwellings even if they reported rent expenditure in section 6 - 63 obs 
  replace flag=1 if q03==2 & flag==0 // exclude hh occupied free of rent dwellings even if they reported rent expenditure in section 6 - 49 obs?


//  c. percentage of households renting by values of rural
bys rural: sum flag [fw = round(pw2)] // around 1% of the households in rural live in rented dwellings. The sample size is not enough to estimate hedonistic model seperately for rural and urban.
bys rural: tabstat s6aq02 if flag==0 [aw = pw2] , s(min p5 p50 p95 p99 max)

// note: we don't have respondent estimates of rental value, so will have to use hedonistic rental value model


/* ---- 2. Construct independent variables for hedonistic rental model ------ */

// note: would recommend you define all independent variables for all households
//       so if a variable is missing for a particular household, set it to the median

// a) independent variable: number of rooms
tab q04 flag
bys rural: tabstat q04 [aw = pw2] , s(min p5 p50 p95 p99 max)
   egen med_rooms = wpctile(q04), w(pw2) p(50) by(rural) // computed median values for number of rooms
   clonevar nrooms=q04
   assert nrooms==q04
   replace nrooms=5 if nrooms>5 & nrooms!=. // winsorized obs where number_rooms>5
   replace nrooms=med_rooms if nrooms==. // imputed missing values using median value
   
   gen nrooms2=nrooms^2
// b) independent variable: type of walls  
tab q05 flag 
   recode q05 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (else=8), gen(type_wall) // aggregated variable for type_wall
   replace type_wall=1 if type_wall==. // imputed missing values. "Wood and mood" is the most common material both in rural and urban.
   
// c) independent variable: type of roof  
tab q06 flag 
   recode q06 (1=1) (2=2) (3=3) (4=4) (else=5), gen(type_roof) // aggregated variable for type_roof
   replace type_roof=1 if type_roof==. // imputed missing values. "Corrugated iron" is the most common material both in rural and urban.

// d) independent variable: type of floor  
tab q07 flag 
   recode q07 (1=1) (5=2) (else=3), gen(type_floor) //  aggregated variable for type_roof
   replace type_floor=1 if type_floor==. // imputed missing values. "Mud/dung" is the most common material both in rural and urban.
     
// e) independent variable: type of kitchen  
tab q08 flag 
   clonevar kitchen=q08 // 
   replace kitchen=3 if kitchen==. // imputed missing values. "A room used for traditional kitchen outside the housing unit" is the most common type both in rural and urban.
   
// f) independent variable: type of toilet  
tab q10 flag 
   clonevar toilet=q10 // 
   replace toilet=8 if toilet==. & rural==1 // imputed missing values for rural (mod)
   replace toilet=5 if toilet==. & rural==2 // imputed missing values for small town  (mod)  
   replace toilet=6 if toilet==. & rural==3 // imputed missing values for large town  (mod)  
   
// g) independent variable: main source of drinking water
tab q13 flag 
   clonevar drinking_water=q13 // 
   replace drinking_water=7 if drinking_water==. // imputed missing values for rural (mod level) (1 obs)
   
// h) independent variable: main source of light
tab q19_a flag 
     
   * I could not be sure whether I need to employ the  q19_a as an independent variable. There are significant variations in the distributions of these variable across rented vs other dwellings . Introducing them to the model may cause a noise in the estimation.  
	 

/* ---- 3. Model ------------------------------------------------------------ */

// a) define dependent variable for observation where the rent expenditure is plausible
clonevar rent= s6aq02 if flag==0 // define rent only for observations that we will use in the model.
gen lnrent= log(rent)

// b) define binary variable will be used for independent var in the Heckman selection equation.
gen occupied_rent=(q03==3) & flag==0

// c) estimate model
  
// I could not find true post estimation commands of the heckman that I can use to estimate usevalue. Therefore I used manuel heckman rather than the below command. 
/*   
heckman lnrent nrooms nrooms2 i.type_wall i.type_roof i.type_floor i.kitchen i.toilet i.drinking_water [aw = round(pw2)], select(occupied_rent= i.rural  i.saq01 nrooms type_roof type_floor  drinking_water  )
predict lnrent_hat
*/


// estimate selection model and mills ratio

probit occupied_rent i.rural  i.saq01 nrooms type_roof type_floor drinking_water  [pw = pw2]
predict probitxb, xb
ge pdf = normalden(probitxb)
ge cdf = normal(probitxb)
ge imr = pdf/cdf

// estimate main model
reg lnrent nrooms nrooms2 i.type_wall i.type_roof i.type_floor i.kitchen i.toilet i.drinking_water imr [aw = pw2]
scalar RMSE = e(rmse)
predict lnrent_hat
gen usevalue = exp(lnrent_hat) * exp(RMSE^2/2)

gen lusevalue=log(usevalue)	  
*histogram lusevalue if rural==3, normal 


// check outliers of the estimation and winsorize 

//lower outliers
** construct lower percentile based on total expenditure
tabstat usevalue [aw = pw2], s(min p5 p50 p95 p99 max) by(rural)
egen lower_usevalue   = wpctile(usevalue), w(pw2) p(1) by(rural)

** winsorize lower outliers
replace usevalue = lower_usevalue if usevalue<lower_usevalue/2

//upper outliers
** define global upper bound for usevalue
recode rural (1 = 1500) (2 = 2500) (3= 7000), gen (upper_level) 

** construct median level, and upper percentiles 
egen med_usevalue   = wpctile(usevalue), w(pw2) p(50) by(rural)
egen upper_usevalue   = wpctile(usevalue), w(pw2) p(99) by(rural)
replace upper_usevalue = min(upper_level, 2*upper_usevalue) // set upper bound at min of global upper limit and twice the 99th percentile

** winsorize upper outliers
replace usevalue = upper_usevalue if usevalue>upper_usevalue

/* ---- 4. Do self-reported actual rents differ systematically from imputed - */
/*         rents?                                                             */

sum rent usevalue if rent!=., d 
*histogram lnrent if rent!=., normal

/* ---- 5. Construct variables ---------------------------------------------- */

//  a. imputed rent for owner occupied or other
// housing usevalue based on model with heckman correction
gen housing_usevalue = usevalue if q03!=3  
	 
// b. adress upper and lower outliers

// construct median, lower and upper percentiles by rural

egen low_house_value  = wpctile(housing_usevalue), w(pw2) p(1) by(rural)
egen med_house_value   = wpctile(housing_usevalue), w(pw2) p(50) by(rural)
egen upper_house_value   = wpctile(housing_usevalue), w(pw2) p(99) by(rural)

replace housing_usevalue =med_house_value if housing_usevalue>2*upper_house_value & housing_usevalue<.
replace housing_usevalue =med_house_value if housing_usevalue<low_house_value/2  & housing_usevalue<.


//  c. actual rent
// define this variable for everyone who says they are a renter, whether or not a (reasonable) value is given in section 6
//  add code to deal with cases where no amount of rent was given in section 6, or a likely incorrect amount was given

if "`method_renters'" == "useActual" {
	 gen housing_rent = s6aq02 if q03==3 
	 replace housing_rent=median_rent if q03==3  & flag==1 // use median rent value to impute outliers or inplausible observations	 
}

if "`method_renters'" == "useImputed" {
	 gen housing_rent = usevalue if q03==3 
     replace housing_rent =med_usevalue if housing_rent>2*upper_usevalue & housing_rent<. // use mnedian usevalue to impute outliers
     replace housing_rent =med_usevalue if housing_rent<lower_usevalue/2  & housing_rent<.
 }

//  c. annualize the rent and usevalue 
foreach var of varlist housing_rent housing_usevalue  {
	     replace `var'=`var'*12
		 
		
}
/* ---- 8. Check distribution ----------------------------------------------- */

//  a. check distribution

gen log_rent=log(housing_rent)
*histogram log_rent [fw = round(pw2)], normal

gen log_usevalue=log(housing_usevalue)
*histogram log_usevalue [fw = round(pw2)], normal // with heckman correction

//

/* ---- 9. Save ------------------------------------------------------------- */

keep household_id2 ea_id2 housing_rent housing_usevalue

if "`method_renters'" == "useActual" lab var housing_rent "actual rent paid by households renting, annual"
if "`method_renters'" == "useImputed" lab var housing_rent "use value of renter occupied housing, annual"
lab var housing_usevalue "use value of owner occupied housing, annual"
save "${out}Cons_housing.dta", replace

*_______________________________________________________________________________
***************************- END of do-file  -**********************************

