


set maxvar 30000

use "D:\Research\BDHS Research\Mental Health\Bangladesh\data\BDIR81FL.DTA", clear






****************************************************************************
*---------------------------------------------------------------------------
// Sampling weight
*Since DHS follws stratified sampling we need to apply survey weigth, primary sampling unit (V021), and sampling strata (V022).
*In DHS we need to create Weight variable first, following this formula:
***-------------------------------------------------------------
gen WGT=(v005/1000000)

*Now we will label this variable 

label variable WGT "Survey Weight"
svyset [pw=WGT],psu(v021) strata(v022)
****************************************************************************




svy: tab v020
describe v020
codebook v020










**********************************************************
 *--------------------------------------------------------
 * Outcome:
 *--------------------------------------------------------
 *******************************************************
*******************************************************
***************************************************************
* Create binary depression and anxiety variables (publication standard)
***************************************************************

* Depression: PHQ-9 score (mth22)
gen depression_cat = .
replace depression_cat = 1 if mth22 >= 10 & mth22 <= 27
replace depression_cat = 0 if mth22 < 10 & !missing(mth22)

* Anxiety: GAD-7 score (mth24)
gen anxiety_cat = .
replace anxiety_cat = 1 if mth24 >= 10 & mth24 <= 21
replace anxiety_cat = 0 if mth24 < 10 & !missing(mth24)

* Label definitions and application
label define yesno 0 "No" 1 "Yes"
label values depression_cat yesno
label values anxiety_cat yesno

label variable depression_cat "Depression (PHQ-9 ≥10 = Yes)"
label variable anxiety_cat "Anxiety (GAD-7 ≥10 = Yes)"

* ---------------------------------------------------------------
* R1 ADDITION: keep the raw PHQ-9 / GAD-7 scores and build the
* published severity bands, so the dichotomy at >=10 can be checked
* against a categorical analysis (reviewer comment on GAD-7 coding).
* PHQ-9  (Kroenke 2001): 0-4 none/minimal, 5-9 mild, 10-14 moderate,
*                        15-19 moderately severe, 20-27 severe.
* GAD-7  (Spitzer 2006): 0-4 minimal, 5-9 mild, 10-14 moderate,
*                        15-21 severe.
* ---------------------------------------------------------------
rename mth22 phq9_score
rename mth24 gad7_score
label variable phq9_score "PHQ-9 total score (0-27)"
label variable gad7_score "GAD-7 total score (0-21)"

egen phq9_band = cut(phq9_score), at(0,5,10,15,20,28) icodes
label define phq9lbl 0 "None/minimal (0-4)" 1 "Mild (5-9)" 2 "Moderate (10-14)" ///
                     3 "Moderately severe (15-19)" 4 "Severe (20-27)", replace
label values phq9_band phq9lbl
label variable phq9_band "PHQ-9 severity band"

egen gad7_band = cut(gad7_score), at(0,5,10,15,22) icodes
label define gad7lbl 0 "Minimal (0-4)" 1 "Mild (5-9)" 2 "Moderate (10-14)" ///
                     3 "Severe (15-21)", replace
label values gad7_band gad7lbl
label variable gad7_band "GAD-7 severity band"

tab phq9_band, missing
tab gad7_band, missing


* Tabulate
tabulate depression_cat, missing
tabulate anxiety_cat, missing



drop if missing(depression_cat)
drop if missing(anxiety_cat)



*******************************************************************************
********************************************************************************
* SECTION : CONFOUNDERS
********************************************************************************
*******************************************************************************
*** 1. INDIVIDUAL-LEVEL SOCIODEMOGRAPHIC VARIABLES ***
* Age Group recoding and labeling with missing handling

gen women_age = .
replace women_age = 1 if inrange(v012, 15, 19)
replace women_age = 2 if inrange(v012, 20, 24)
replace women_age = 3 if inrange(v012, 25, 29)
replace women_age = 4 if inrange(v012, 30, 34)
replace women_age = 5 if inrange(v012, 35, 39)
replace women_age = 6 if inrange(v012, 40, 44)
replace women_age = 7 if inrange(v012, 45, 49)

label define agegroup 1 "15–19" 2 "20–24" 3 "25–29" 4 "30–34" 5 "35–39" 6 "40–44" 7 "45–49", replace
label values women_age agegroup

tab women_age


* Woman's Education
gen education_woman = .
replace education_woman = 0 if v106 <= 1
replace education_woman = 1 if v106 == 2
replace education_woman = 2 if v106 >= 3
label define edu_lbl 0 "No/Primary" 1 "Secondary Incomplete" 2 "Secondary+"
label values education_woman edu_lbl

tab education_woman

tab v501



* Number of Living Children with missing handling
tab v128
gen num_children = .
replace num_children = 0 if v218 == 0
replace num_children = 1 if inrange(v218, 1, 2)
replace num_children = 2 if v218 >= 3 & v218 != .

label define child_lbl 0 "None" 1 "1–2" 2 "3+", replace
label values num_children child_lbl

tab num_children




* Recode v213 to a new variable called currently_pregnant
tab v213

gen currently_pregnant = .
replace currently_pregnant = 0 if v213 == 0    // no or unsure
replace currently_pregnant = 1 if v213 == 1    // yes

* Label the new variable
label define preg_lbl 0 "No or unsure" 1 "Yes"
label values currently_pregnant preg_lbl

* Add a variable label
label variable currently_pregnant "Currently pregnant status"


tab currently_pregnant


* Recode v504 to a new variable called residing_with

tab v504
gen residing_status = .
replace residing_status = 1 if v504 == 1
replace residing_status = 0 if inlist(v504, 2, 3)

label define residing_status_lbl 0 "Not residing with husband/partner" 1 "Residing with husband/partner"
label values residing_status residing_status_lbl
label variable residing_status "Current residence status with husband/partner"

tab residing_status



 tab v406 // abstainng
 

 
* Create binary variable: 1 = Yes, 0 = No
gen currently_abstaining = .
replace currently_abstaining = 1 if v406 == 1
replace currently_abstaining = 0 if v406 == 0

* Define and assign value labels
label define abstain_lbl 0 "No" 1 "Yes"
label values currently_abstaining abstain_lbl

* Label the variable for publication
label variable currently_abstaining "Currently abstaining from sexual intercourse (Yes=1, No=0)"

* Check the result
tab currently_abstaining
 
 tab v503 // কয়বার বিবাহ/দাম্পত্য সম্পর্ক

* Create a binary variable: 0 = once, 1 = more than once
gen number_of_unions = .
replace number_of_unions = 0 if v503 == 1
replace number_of_unions = 1 if v503 > 1

* Define value labels
label define union_lbl 0 "Once" 1 "More than once"

* Assign value labels
label values number_of_unions union_lbl

* Label the variable for publication
label variable number_of_unions "Number of marital unions (Once = 0, More than once = 1)"

* Tabulate to confirm
tab number_of_unions

 
 
 
 
 
*To categorize v245 (pregnancy losses)
gen preg_loss_cat = .
replace preg_loss_cat = 0 if v245 == 0
replace preg_loss_cat = 1 if v245 == 1
replace preg_loss_cat = 2 if v245 >= 2

label define preg_loss_lbl 0 "None" 1 "One loss" 2 "Two or more losses"
label values preg_loss_cat preg_loss_lbl
tab preg_loss_cat

 
 

* Generate a new variable menstruated_last6wks
gen menstruated_last6wks = .
replace menstruated_last6wks = 1 if v216 == 1   // yes
replace menstruated_last6wks = 0 if v216 == 0   // no

* Label the new variable values
label define menses_lbl 0 "No" 1 "Yes"
label values menstruated_last6wks menses_lbl

* Label the variable
label variable menstruated_last6wks "Menstruated in last six weeks"

* Check the recoded variable with a tabulation
tab menstruated_last6wks




* Generate binary variable: 1 = Yes, 0 = No
gen has_health_card = .
replace has_health_card = 1 if s1115a == 1
replace has_health_card = 0 if s1115a == 0

* Define and apply value labels
label define healthcard_lbl 0 "No" 1 "Yes"
label values has_health_card healthcard_lbl

* Label the variable clearly for publication
label variable has_health_card "Possesses a free/subsidized health care services card (Yes=1, No=0)"

* Check the result
tab has_health_card


* pregnent
rename v636 pressure_to_pregnant

tab pressure_to_pregnant




* Create a binary variable for house ownership
generate hh_own_house = .

* Recode: 1 = Owns (any form), 0 = Does not own
replace hh_own_house = 0 if v745a == 0                   // Does not own
replace hh_own_house = 1 if inlist(v745a, 1,2,3,4,5)     // Owns (any form)

* Add value labels
label define hh_own_house_label 0 "Does not own" 1 "Owns"
label values hh_own_house hh_own_house_label

* Check the distribution
tab hh_own_house

    












*********************************
* Empowerment

**********************************
*Empowerment Index
gen emp_health = inlist(v743a, 1, 2)   // respondent alone or joint
gen emp_purchase = inlist(v743b, 1, 2)
gen emp_money = inlist(v743f, 1, 2)
gen emp_visit = inlist(v743d, 1, 2)
gen emp_fp = inlist(v632, 1, 3)        // respondent or joint decision

* Composite empowerment score
gen emp_score = emp_health + emp_purchase + emp_money + emp_visit + emp_fp



gen emp_level = .
replace emp_level = 0 if emp_score <= 2   // No
replace emp_level = 1 if emp_score >= 3   // Yes
label define emp_lbl 0 "No" 1 "Yes"
label values emp_level emp_lbl

tab emp_level // Yes" (empowered) ,"No" (not empowered)











 tab v621 // husband desire for children

* Drop existing if necessary
capture drop husb_pref_more_same

* Generate and label
gen husb_pref_more_same = .
replace husb_pref_more_same = 1 if inlist(v621, 1, 2)
replace husb_pref_more_same = 0 if v621 == 3

label define husb_pref_lbl 0 "No: husband wants fewer" 1 "Yes: same or more"
label values husb_pref_more_same husb_pref_lbl
label variable husb_pref_more_same "Husband's fertility preference: same/more = 1, fewer = 0"

* Summary table
tab husb_pref_more_same





tab v632 // decision maker for using contraception

rename v632 contracept_decision
tab contracept_decision


* IPV
 tab v744a
 tab v744b
 tab v744c
 tab v744d
 tab v744e
 
 * Recode "don't know" (usually coded as 2) to missing for each IPV justification question
foreach var in v744a v744b v744c v744d v744e {
    replace `var' = . if `var' == 2
}

* Generate IPV justification binary variable:
* 1 = Justifies beating in at least one condition
* 0 = Does NOT justify beating in any condition
gen ipv_justified = 0
replace ipv_justified = 1 if v744a == 1 | v744b == 1 | v744c == 1 | v744d == 1 | v744e == 1

* Label the variable for publication
label define ipv_lbl 0 "Does not justify IPV" 1 "Justifies IPV"
label values ipv_justified ipv_lbl
label variable ipv_justified "Justification of intimate partner violence (any condition)"

* Tabulate the new variable
tab ipv_justified, missing

 
 
 
 
 
 
 
 
 




*******Another confounder****

* Place of residence/area (v025)
rename v025 area
tab area, mis

label define area_label 1 "Urban" 2 "Rural"
label values area area_label
tabulate area

* Division (v024)
rename v024 hh_division
tab hh_division, mis

* Religion (v130)
rename v130 religion
tab religion
* Household wealth index
rename v190 hh_wealth
label variable hh_wealth "Household Wealth Index"

tab hh_wealth, mis


* Household size (v136)
tab v136, mis

gen hh_size_cat=.
replace hh_size_cat = 1 if v136<=5
replace hh_size_cat = 0 if v136>5

label define hh_size_cat_lbl 1"<=5" 0">5"

* Label the variable for clarity
label variable hh_size_cat "Categories of size of household"

* Label the groups for clarity
label values hh_size_cat hh_size_cat_lbl 

tab hh_size_cat, mis

* sex of the household head (V151)
tab v151, mis
rename v151 hh_head_sex
tab hh_head_sex


* currently working
rename  v714 currently_working
tab currently_working

* father edu
rename s115_1 father_edu

tab father_edu









	  
*********************************
*********************************
* Health care
*******************************
*********************************  
	  
	  
	  
tab v467d  // Access to healthcare : distance
describe v467d
codebook v467d

* Rename variable for journal-ready format
rename v467d hh_healthcare_dist

* ---------------------------------------------------------------
* R1 REVISION (reviewer comment: Results vs Conclusion contradiction)
* v467d is coded 1 = "big problem", 2 = "not a big problem".
* With i.hh_healthcare_dist Stata takes level 1 ("big problem") as the
* base, so the reported OR belonged to the "not a big problem" group.
* Recoded so that "not a big problem" is the reference category.
* ---------------------------------------------------------------
recode hh_healthcare_dist (2=0) (1=1), gen(hh_healthcare_dist_rc)
label define distlbl 0 "Not a big problem" 1 "Big problem", replace
label values hh_healthcare_dist_rc distlbl
label variable hh_healthcare_dist_rc "Difficulty in accessing health care (distance)"
tab hh_healthcare_dist hh_healthcare_dist_rc, missing


* Check tabulation
tab hh_healthcare_dist


tab s1115a   // health card
describe s1115a
codebook s1115a

   
   * Rename variable for journal-ready format
rename s1115a hh_health_card

* Check tabulation
tab hh_health_card

*****************************
*****************************
*Household media
*****************************
*****************************


  tab v120
  describe v120
  codebook v120
  
  
  
  tab v121
  describe v121
  codebook v121


tab v157 // news paper
describe v157
codebook v157




   
 tab v169a // Mobile phone ownership
describe v169a
codebook v169a
 
   
   
   
   * Rename original variables first (optional, for clarity)
rename v120 hh_radio
rename v121 hh_tv
rename v157 hh_newspaper
rename v169a hh_mobile

* Create a combined media variable
generate hh_media = 0

* If household has any media, mark as 1
replace hh_media = 1 if hh_radio==1 | hh_tv==1 | hh_newspaper>0 | hh_mobile==1

* Check distribution
tab hh_media

* Label for clarity
label define media_lbl 0 "No media" 1 "Has media"
label values hh_media media_lbl
* Check distribution
tab hh_media
   
   
   
tab v171a // internet
describe v171a
codebook v171a
   
 
 gen internet_use = .
replace internet_use = 0 if inlist(v171a, 0, 2)  // never or yes, before last 12 months = No
replace internet_use = 1 if v171a == 1            // yes  last 12 months = Yes

label define internet_lbl 0 "No internet use" 1 "Internet use  last 12 months"
label values internet_use internet_lbl
label variable internet_use "Internet use (binary: only  last 12 months = Yes)"

tab internet_use
 
 
*****************************
*****************************
*Household Assest
*****************************
*****************************
  

  tab v122
  describe v122
  codebook v122
  
  
  tab v123
  describe v123
  codebook v123
   
   
   
   
  tab v124
  describe v124
  codebook v124
  
  
  
  tab v125
  describe v125
  codebook v125
   
   
   

   
 * Create a single household asset variable (binary)
gen hh_assets = 0

* Mark as 1 if household owns any asset (recode not a dejure resident as no)
replace hh_assets = 1 if inlist(v122,1) | inlist(v123,1) | inlist(v124,1) | inlist(v125,1)

* Label the variable
label define hh_assets_lbl 0 "No" 1 "Yes"
label values hh_assets hh_assets_lbl
label variable hh_assets "Household owns any major asset (refrigerator, bicycle, motorcycle/scooter, car/truck)"

* Check
tab hh_assets
  
   
   
   

*****************************
***************************
* Household materials
 
*****************************
*******************************
   
tab v128     //  wall material

describe v128
codebook v128
   
tab v127 
describe v127
 codebook v127
   
   
tab v129 
describe v129
codebook v129
   
   
   
   
   *-------------------------------
* Create household materials variable
*-------------------------------

gen hh_materials = 0   // default = unimproved

* Wall material: assign 1 if improved (cement, bricks, cement blocks, tin, stone with lime/cement)
replace hh_materials = 1 if inlist(v128, 31, 34, 35, 37, 36, 38, 39)  

* Floor material: assign 1 if improved (cement, ceramic tiles, vinyl, parquet, carpet)
replace hh_materials = 1 if inlist(v127, 34, 30, 31, 32, 33)  

* Roof material: assign 1 if improved (cement, tin/metal, ceramic tiles, calamine/cement fiber, roofing shingles)
replace hh_materials = 1 if inlist(v129, 31, 35, 36, 37, 38, 39)  

* Treat "not dejure resident" as unimproved
replace hh_materials = 0 if inlist(v128,97) | inlist(v127,97) | inlist(v129,97)

* Label the variable
label define hh_materials_lbl 0 "Unimproved" 1 "Improved"
label values hh_materials hh_materials_lbl
label variable hh_materials "Household materials quality (Improved vs Unimproved)"

* Check
tab hh_materials










keep phq9_score gad7_score phq9_band gad7_band depression_cat anxiety_cat women_age education_woman num_children currently_pregnant residing_status currently_abstaining number_of_unions preg_loss_cat menstruated_last6wks has_health_card pressure_to_pregnant hh_own_house emp_level husb_pref_more_same ipv_justified area hh_division hh_wealth hh_size_cat hh_head_sex currently_working father_edu hh_healthcare_dist hh_healthcare_dist_rc hh_health_card hh_media internet_use hh_assets hh_materials v005 v021 v022 WGT







save "D:\Research\BDHS Research\Mental Health\Bangladesh\data\mental_health_clean.dta", replace


















































