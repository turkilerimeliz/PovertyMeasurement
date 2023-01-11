/* =============================================================================

Poverty Measurement Course July 2022
Poverty and Equity Global Practice
The World Bank

Stata Exercises: Construction of Consumption Aggregate for 
                 2013 Ethiopia Socioeconomic Survey

By: Meliz Tyurkileri
Email: melizturkileri@sabanciuniv.edu

============================================================================= */

/*-----------------------------------------------------------------------------
 There is a section in the beginning of the "do-file 8" and "do-file 4" where 
 you can change some of the local variables to produce sentivity checks.     
-----------------------------------------------------------------------------*/


version 16.1

// add the specific file path to your folder here
global path "C:\Users\WB586966\OneDrive - WBG\Desktop\Lecture\Stata Exercises\"

global do    "${path}do_files\"
global raw   "${path}public_data\"
global temp  "${path}temp\"
global out   "${path}out\"
global input "${path}input\"

cd "${do}"



do 01_nonfood_Meliz_Tyurkileri.do
do 02_education_Meliz_Tyurkileri.do
do 03_durables_Meliz_Tyurkileri.do
do 04_housing_Meliz_Tyurkileri.do
do 05_food_Meliz_Tyurkileri.do
do 06_hh_comp_Meliz_Tyurkileri.do
do 07_price_index_Meliz_Tyurkileri.do
do 08_poverty_Meliz_Tyurkileri.do



*_______________________________________________________________________________
***************************- END of do-file  -**********************************
