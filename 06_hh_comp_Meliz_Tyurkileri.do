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

/* ---- 1.  Construct household size----------------------------------------- */

use "${raw}sect1_hh_w2.dta"
drop if hh_s1q04c == 2 // drop people no longer in the hh
assert rural != . // just pick a random, never missing numeric variable to count when collapsing
bys household_id2: egen hh_size=count(rural) // construct hhsize


/* ---- 2.  Identify children and adults ------------------------------------ */

// a. Generate age variable
clonevar age= (hh_s1q04_a) 
replace age=hh_s1q04h if hh_s1q04h!=. // use corrected age values 
count  if age==. //

// b. Identify children and adults based on age treshold.

// Individuals lower than 18 years old are considered as child in Ethiopia
gen child=(age<18 & age!=.) // define who is identified as child,
gen adult=(age>=18 & age!=.) // define who is identified as adult
replace adult=1 if age==. // define obsevations as adult where age is missing 


/* ---- 3. Construct total number of children and adults for each household-- */

bys household_id2: egen total_child=sum(child) 
bys household_id2: egen total_adult=sum(adult)

assert hh_size==total_child+total_adult // double check


/*4.collapse to household-level data with one variable (called nonfood2) for total hh expenditure (also keep ea and weight variables) */

collapse (mean) hh_size total_child total_adult, by(household_id2 ea_id2 pw2 rural saq01)

/* ---- 4. Define household equivalence indexes ----------------------------- */

gen es_pc=hh_size  // per capita
gen es_dz=(total_adult+0.33*total_child)^0.9       // Deaton & Zaidi
gen es_pacific=(total_adult+0.5*total_child)       // Pacific Equivalence Scale
gen es_lis=(hh_size)^0.5                           // Luxembourg Income Scale
gen es_oxford=0.3+0.7*total_adult+0.5*total_child  // Oxford Scale
gen es_oecd2=0.5+0.5*total_adult+0.3*total_child   // OECD-II Scale

corr es*


/* ---- 5. Select household equivalence index--- ----------------------------- */

// I decided to use pacific scale after I did sensitivity analysis.

*graph box es_pc es_dz es_pacific es_lis es_oxford es_oecd2 [aweight = pw2]
rename es_pacific hhame


/* ---- 6. Drop unnecessary variables and Save------------------------------- */

drop  total_child total_adult
label var hh_size "Household size"
label var hhame "Household adult male equivalents using pacific scale"
label var es_pc "Household adult male equivalents per-capita "
label var es_dz "Household adult male equivalents using dz scale"
label var es_lis "Household adult male equivalents using LIS scale"
label var es_oxford "Household adult male equivalents using Oxford scale"
label var es_oecd2 "Household adult male equivalents using OECD-II scale"

order hhame, after(hh_size)
save "${out}Cons_hhcomp.dta", replace


*_______________________________________________________________________________
***************************- END of do-file  -**********************************
