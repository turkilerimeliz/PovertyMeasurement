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
/*          A. Household Level Paasche Food Price Index                       */
/* -------------------------------------------------------------------------- */

/* --- 1. Combine household level data on expenditures with data on prices - */

use "${temp}Food_quant_price.dta", clear

merge m:1 ea_id2 q00 using "${input}foodprices"
drop if q00==25 // drop observations belong to item kat
assert _merge ==3
drop _merge


/* ---- 2. Construct w_k^h -------------------------------------------------- */

// construct share of household h's budget devoted to good k.

// a. construct total expenditure of household
count if vh==.
bysort household_id2: egen total_food_expenditure= total(vh)

// b. construct share of a given item in total expenditure of household
gen item_share= vh/total_food_expenditure 
count if  total_food_expenditure==0 // for 81 household total_food_expenditure is 0 


/* ---- 3. Construct weighted price ratio (w_k^h * p_k^0 / p_k^h) ----------- */
// use local prices as p_k^h
gen  w_price_ratio= item_share*national_price/local_price


/* ---- 4. Sum over all items for each household ---------------------------- */
collapse (sum) w_price_ratio ,  by (household_id2 ea_id2 pw2 saq01 saq02 saq03 saq04 saq05 saq06 saq07)


/* ---- 5. Calculate Paasche price index ------------------------------------ */
gen pi_spatial = 1/(w_price_ratio)


/* ---- 6. Check with official price index from HIES ------------------------ */

recode saq01 (1 = 1.034) (2 = 1.021) (3 = 0.949) (4 = 0.981) (5 = 1.132) ///
             (6 = 0.958) (7 = 0.906) (12 = 1.056) (13 = 1.227) (14 = 1.554) ///
             (15 = 1.245), gen(opi)
lab var opi "official price index"

/*graph hbox pi_spatial opi [aw = pw2], over(saq01) ///
           legend(ring(0) pos(2) col(1) symx(*0.5))
*/		   


/* ---- 7. Save ------------------------------------------------------------- */

lab var pi_spatial "spatial price index, Paasche, household level, food prices from purchases"
save "${out}Cons_price.dta", replace


*_______________________________________________________________________________
***************************- END of do-file  -**********************************