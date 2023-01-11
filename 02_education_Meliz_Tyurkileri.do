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
/*          A. Section 2: Individual Education                                */
/* -------------------------------------------------------------------------- */

use "${raw}sect2_hh_w2.dta", clear
rename hh_s2* *


/* ---- 0. Preliminaries ---------------------------------------------------- */

//  a. merge in age from household roster
merge 1:1 individual_id2 using "${raw}sect1_hh_w2.dta", nogen keep(master match) keepusing(hh_s1q04_a) // merge in age from hh roster


/* ---- 1. Investigate missing and outlier values --------------------------- */

//  a. is this section completed for the correct persons? and missing values of q06
tab q06 if q01 == "X" & q03 == 1, m  // just 7, won't worry about

//  b. missing values of q15 (scholarship / aid received) if receiving
count if q15 == . & q14 == 1

//  c. missing values of q16 (fees) if currently attending school
count if q16 == . & q06 == 1  // 70 missing values.

//  d. missing values of q17 (books, uniforms etc) if currently attending school
count if q17 == . & q06 == 1  // 44 missing values.

recode q08 (0 = 0 "nursery") (1/8 = 1 "primary") (9/13 21/24 = 2 "secondary") (14/20 25/35 95 = 3 "tertiary"), gen(level)
recode q09 (1 = 1 "gov't") (2/7 = 2 "non gov't"), gen(provider)

//  e. outliers in q15
gen scholarship = q15
tabstat scholarship [aw = pw2], s(min p5 p50 p95 p99 max)
bys provider: tabstat scholarship [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education provider.
bys level: tabstat scholarship [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education level.
bys rural: tabstat scholarship [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education level.

//  f. outliers in q16
gen fees = q16
tabstat fees [aw = pw2], s(min p5 p50 p95 p99 max)
bys provider: tabstat fees [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education provider.
bys level: tabstat fees [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education level.
bys rural level: tabstat fees [aw = pw2], s(min p5 p50 p95 p99 max) // 


//  g. outliers in q17
gen books = q17
tabstat books [aw = pw2], s(min p5 p50 p95 p99 max)
bys provider: tabstat books [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education provider.
bys level: tabstat books [aw = pw2], s(min p5 p50 p95 p99 max) // it is more logical to identify outliers based on education level.
bys rural: tabstat books [aw = pw2], s(min p5 p50 p95 p99 max) // 




/* ---- 2. Address missing and outlier values ------------------------------- */

//  a. construct medians and upper and lower bounds for scholarship, fees and books

foreach var of varlist scholarship fees books {
       egen upper_`var' = wpctile(`var'), w(pw2) p(99) by(level provider rural)
       egen med_`var'   = wpctile(`var'), w(pw2) p(50) by(level provider rural)
       egen lower_`var' = wpctile(`var'), w(pw2) p(1) by(level provider rural)
}

//  b. address missing values of scholarship
replace scholarship=0 if q15==2 & q06==1 // adress missing value where child attending to school and have not received scholarship
replace scholarship= med_scholarship if scholarship==. & q14==1 
replace scholarship=med_scholarship if scholarship==0 & q14==1 // adress inappropriate values of 0 
replace scholarship=. if scholarship==0 & q14==2 // adress inappropriate values of 0 (do not receive a scholarship while value of scholarship is recorded as 0)

//  c. address missing values of fees and books

foreach var of varlist fees books  {
          replace `var'= med_`var' if `var'==. & q06==1
}

//  c. address any upper and lower outliers in scholarship 

foreach var of varlist scholarship fees books  {
        gen log_`var' = log(`var')
}
      *histogram log_scholarship, normal //check distribution  
	  *browse if log_scholarship>0.5 & log_scholarship<1
      *histogram log_fees, normal //check distribution 
      *histogram log_books, normal //check distribution 
  
egen log_scholarship_med = wpctile(log_scholarship), w(pw2) p(50) by(level provider rural)
mads log_scholarship [aw = pw2], by(level provider rural) m(A)
egen groupnum_scholarship = group(level provider rural)
gen Zscore_scholarship = (log_scholarship-log_scholarship_med)/A[groupnum_scholarship, 5]
count if Zscore_scholarship < -3
count if Zscore_scholarship > 3 & Zscore_scholarship < .
	   
replace scholarship	=exp(log_scholarship - 3 * A[groupnum_scholarship, 5]) if Zscore_scholarship < -3 // winsorize lower outliers 
replace scholarship =exp(log_scholarship + 3 * A[groupnum_scholarship, 5]) if Zscore_scholarship > 3 & Zscore_scholarship < .	// winsorize upper outliers 	
					  
//  c. address any upper and lower outliers in fees and books 				



foreach var of varlist fees books  {
           count if `var' > 2 * upper_`var' & `var' < .
		   count if `var'< lower_`var'/2 & `var' < .
}

foreach var of varlist fees books  {
           replace `var'=2 * upper_`var'  if `var'> 2 * upper_`var' & `var' < . // winsorize upper outliers 
}

foreach var of varlist fees books  {
           replace `var'= lower_`var'/2  if `var'< lower_`var'/2 & `var' < . // winsorize lower outliers 
}

/* ---- 3. Construct annual education expenditure --------------------------- */

//  a. annualize ( recall period is 12 months for q16 and q17 while it is current school year for q15. I think we need to use interview month and year to adjust the q15, however sect_cover_hh_w2 file reports interview year as 2006. There is a mismatch.)
//  STEP 1: 
egen ann_educ=rowtotal(scholarship fees books)
//  b. sum over household
//  STEP 1:
collapse (sum) education=ann_educ (mean) pw2 (firstnm) ea_id2, by (household_id2)
isid household_id2

//  c. check distribution
gen logeducation = log(education) // 
*histogram logeducation, normal // there are not any obs in the lower tail. Should I be suspicious about it?



/* ---- 4. Save ------------------------------------------------------------- */

//  a. keep only variables needed
//  STEP 1: 
keep ea_id2 household_id2 education 

//  b. save
isid household_id2
lab var education "expenditure on eduction, annual"
save "${out}Cons_education.dta", replace


*_______________________________________________________________________________
***************************- END of do-file  -**********************************