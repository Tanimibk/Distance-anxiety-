* ===============================================================
* R1 REVISION NOTE
* 1. Exposure recoded in "Data management.do": hh_healthcare_dist_rc
*      0 = Not a big problem (REFERENCE), 1 = Big problem.
*    In the first submission i.hh_healthcare_dist took "big problem"
*    as the base, so the reported OR described the wrong group.
* 2. i.hh_assets dropped: the DHS wealth index is built by PCA from
*    these same assets (reviewer comment).
* 3. i.father_edu dropped: cross-tabulation showed it reproduces
*    education_woman, which made the two education terms collinear.
* 4. Duplicated covariates within a single model removed.
* ===============================================================



* Title :Does Difficulty in Accessing Health Care Affect Anxiety and Depression Among Ever-Married Women? Insights from BDHS 2022
use "D:\Research\BDHS Research\Mental Health\Bangladesh\data\mental_health_clean.dta", clear

*Association Between Difficulty in Accessing Health Care and Anxiety and Depression Among Ever-Married Women: Insights from BDHS 2022







****************************************************************************
* Stepwise variable selection for adjusted models
****************************************************************************
* Depression
****************************************************************************
* Bivariate association between each variable and depression
* Use chi2 for categorical variables, t-test/ANOVA for continuous

* Demographics
tab depression_cat women_age, chi2
tab depression_cat education_woman, chi2
tab depression_cat num_children, chi2
tab depression_cat currently_pregnant, chi2
tab depression_cat residing_status, chi2
tab depression_cat currently_abstaining, chi2

* Household & socio-economic
tab depression_cat hh_own_house, chi2
tab depression_cat emp_level, chi2
tab depression_cat husb_pref_more_same, chi2
tab depression_cat ipv_justified, chi2
tab depression_cat hh_division, chi2
tab depression_cat area, chi2
tab depression_cat hh_wealth, chi2
tab depression_cat hh_size_cat, chi2
tab depression_cat hh_head_sex, chi2
tab depression_cat hh_media, chi2
tab depression_cat internet_use, chi2
tab depression_cat hh_assets, chi2
tab depression_cat hh_materials, chi2

* Health & services
tab depression_cat has_health_card, chi2
tab depression_cat hh_healthcare_dist_rc, chi2
tab depression_cat currently_working, chi2
tab depression_cat father_edu, chi2

****************************************************************************
* Anxiety
****************************************************************************
* Bivariate association between each variable and anxiety
tab anxiety_cat women_age, chi2
tab anxiety_cat education_woman, chi2
tab anxiety_cat num_children, chi2
tab anxiety_cat currently_pregnant, chi2
tab anxiety_cat residing_status, chi2
tab anxiety_cat currently_abstaining, chi2

* Household & socio-economic
tab anxiety_cat hh_own_house, chi2
tab anxiety_cat emp_level, chi2
tab anxiety_cat husb_pref_more_same, chi2
tab anxiety_cat ipv_justified, chi2
tab anxiety_cat hh_division, chi2
tab anxiety_cat area, chi2
tab anxiety_cat hh_wealth, chi2
tab anxiety_cat hh_size_cat, chi2
tab anxiety_cat hh_head_sex, chi2
tab anxiety_cat hh_media, chi2
tab anxiety_cat internet_use, chi2
tab anxiety_cat hh_assets, chi2
tab anxiety_cat hh_materials, chi2

* Health & services
tab anxiety_cat has_health_card, chi2
tab anxiety_cat hh_healthcare_dist_rc, chi2
tab anxiety_cat currently_working, chi2
tab anxiety_cat father_edu, chi2








******************************************************************************
******************************************************************************
* Models********
******************************************************************************
******************************************************************************








* crude models

svy: logistic depression_cat i.hh_healthcare_dist_rc




svy: logistic depression_cat ///
i.hh_healthcare_dist_rc ///
i.women_age ///
i.education_woman ///
i.menstruated_last6wks ///
i.preg_loss_cat ///
num_children ///
i.hh_own_house ///
i.ipv_justified ///
i.hh_division ///
i.hh_size_cat ///
i.hh_head_sex ///
i.hh_materials ///
i.internet_use ///
i.currently_pregnant ///
i.hh_wealth ///
i.area ///
i.currently_working, or








********************************************************************************
********************************************************************************
* Depression*****
********************************************************************************
********************************************************************************

svy: logistic anxiety_cat i.hh_healthcare_dist_rc



svy: logistic anxiety_cat ///
i.hh_healthcare_dist_rc ///
i.women_age ///
i.education_woman ///
i.menstruated_last6wks ///
i.preg_loss_cat ///
num_children ///
i.hh_own_house ///
i.ipv_justified ///
i.hh_division ///
i.hh_size_cat ///
i.hh_head_sex ///
i.hh_materials ///
i.internet_use ///
i.currently_pregnant ///
i.hh_wealth ///
i.area ///
i.currently_working, or










* =======================================================
* Survey Logistic Models - Depression & Anxiety by Area
* -------------------------------------------------------------
* R1 ADDITION: crude (unadjusted) models by residence.
* -------------------------------------------------------------
svy, subpop(if area == 2): logistic depression_cat i.hh_healthcare_dist_rc, or
svy, subpop(if area == 1): logistic depression_cat i.hh_healthcare_dist_rc, or
svy, subpop(if area == 2): logistic anxiety_cat    i.hh_healthcare_dist_rc, or
svy, subpop(if area == 1): logistic anxiety_cat    i.hh_healthcare_dist_rc, or


* =======================================================

* --------------------------
* Rural - Depression
* --------------------------
svy, subpop(if area == 2): logistic depression_cat ///
    i.hh_healthcare_dist_rc ///
    i.women_age ///
    i.education_woman ///
    i.menstruated_last6wks ///
    i.preg_loss_cat ///
    num_children ///
    i.hh_own_house ///
    i.ipv_justified ///
    i.hh_division ///
    i.hh_size_cat ///
    i.hh_head_sex ///
    i.hh_materials ///
    i.internet_use ///
    i.currently_pregnant ///
    i.hh_wealth ///
    i.currently_working, or
estimates store rural_depression

* --------------------------
* Urban - Depression
* --------------------------
svy, subpop(if area == 1): logistic depression_cat ///
    i.hh_healthcare_dist_rc ///
    i.women_age ///
    i.education_woman ///
    i.menstruated_last6wks ///
    i.preg_loss_cat ///
    num_children ///
    i.hh_own_house ///
    i.ipv_justified ///
    i.hh_division ///
    i.hh_size_cat ///
    i.hh_head_sex ///
    i.hh_materials ///
    i.internet_use ///
    i.currently_pregnant ///
    i.hh_wealth ///
    i.currently_working, or
estimates store urban_depression

* --------------------------
* Rural - Anxiety
* --------------------------
svy, subpop(if area == 2): logistic anxiety_cat ///
    i.hh_healthcare_dist_rc ///
    i.women_age ///
    i.education_woman ///
    i.menstruated_last6wks ///
    i.preg_loss_cat ///
    num_children ///
    i.hh_own_house ///
    i.ipv_justified ///
    i.hh_division ///
    i.hh_size_cat ///
    i.hh_head_sex ///
    i.internet_use ///
    i.currently_pregnant ///
    i.hh_wealth ///
    i.currently_working, or
estimates store rural_anxiety

* --------------------------
* Urban - Anxiety
* --------------------------
svy, subpop(if area == 1): logistic anxiety_cat ///
    i.hh_healthcare_dist_rc ///
    i.women_age ///
    i.education_woman ///
    i.menstruated_last6wks ///
    i.preg_loss_cat ///
    num_children ///
    i.hh_own_house ///
    i.ipv_justified ///
    i.hh_division ///
    i.hh_size_cat ///
    i.hh_head_sex ///
    i.hh_materials ///
    i.internet_use ///
    i.currently_pregnant ///
    i.hh_wealth ///
    i.currently_working, or
estimates store urban_anxiety

* =======================================================
* Compare Models - Example Table
* =======================================================
* Using esttab to show Odds Ratios and 95% CI
esttab rural_depression urban_depression rural_anxiety urban_anxiety, cells("or ci")
      stats(N r2_p, fmt(0 2) labels("N" "Pseudo R2")) ///
      label





	  

	  
	  
	  
	  
	  

	  
	  
	  
	  
	  ********************************************************************************
* Folder path (Bangladesh – Healthcare access)
********************************************************************************
local folder "D:\Research\BDHS Research\Mental Health\Bangladesh\data"

********************************************************************************
* Helper program: collapse YES (big problem) cases and export %
********************************************************************************
capture program drop make_pct_hc
program define make_pct_hc
    syntax varname using/

    preserve
        * Keep only "big problem"
        keep if `varlist' == 1

        * Count by division
        collapse (count) Yes_cases = `varlist', by(hh_division)

        * Total YES
        summ Yes_cases
        local total_yes = r(sum)

        * Percentage
        gen pct_yes = round((Yes_cases / `total_yes') * 100, 1)

        * Export
        export excel hh_division Yes_cases pct_yes ///
            using "`using'", firstrow(variables) replace
    restore
end

********************************************************************************
* Healthcare access: Distance to health facility is a BIG PROBLEM
********************************************************************************
make_pct_hc hh_healthcare_dist_rc ///
    using "`folder'\healthcare_distance_bigproblem_division_pct.xlsx"

	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
tab women_age anxiety_cat, row
tab women_age depression_cat, row

	  
	  
	  
	  
	  
	  

	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
************************************************
* Survey Logistic Models by Division - Depression & Anxiety
************************************************

* --------------------------
* Chittagong
* --------------------------
svy, subpop(if hh_division==2): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store chittagong_depression

svy, subpop(if hh_division==2): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store chittagong_anxiety

* --------------------------
* Dhaka
* --------------------------
svy, subpop(if hh_division==3): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store dhaka_depression

svy, subpop(if hh_division==3): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store dhaka_anxiety

* --------------------------
* Khulna
* --------------------------
svy, subpop(if hh_division==4): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store khulna_depression

svy, subpop(if hh_division==4): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store khulna_anxiety

* --------------------------
* Mymensingh
* --------------------------
svy, subpop(if hh_division==5): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store mymensingh_depression

svy, subpop(if hh_division==5): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store mymensingh_anxiety

* --------------------------
* Rajshahi
* --------------------------
svy, subpop(if hh_division==6): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store rajshahi_depression

svy, subpop(if hh_division==6): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store rajshahi_anxiety

* --------------------------
* Rangpur
* --------------------------
svy, subpop(if hh_division==7): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store rangpur_depression

svy, subpop(if hh_division==7): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store rangpur_anxiety

* --------------------------
* Sylhet
* --------------------------
svy, subpop(if hh_division==8): logistic depression_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store sylhet_depression

svy, subpop(if hh_division==8): logistic anxiety_cat ///
    i.women_age i.education_woman i.menstruated_last6wks ///
    i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
    i.hh_size_cat i.hh_head_sex i.hh_materials ///
    i.hh_healthcare_dist_rc i.internet_use i.currently_pregnant ///
    i.hh_wealth i.area i.currently_working
estimates store sylhet_anxiety

************************************************
* End of Division-specific Models for Depression & Anxiety
************************************************


* ===============================================================
* R1 SENSITIVITY ANALYSIS (GAD-7 / PHQ-9 severity bands)
* Needs the updated mental_health_clean.dta produced after re-running
* "Data management.do" (now keeps phq9_score, gad7_score, bands).
* ===============================================================
global ADJ i.women_age i.education_woman i.menstruated_last6wks ///
           i.preg_loss_cat num_children i.hh_own_house i.ipv_justified ///
           i.hh_division i.hh_size_cat i.hh_head_sex i.hh_materials ///
           i.internet_use i.currently_pregnant i.hh_wealth i.area ///
           i.currently_working

svy: tab phq9_band hh_healthcare_dist_rc, col
svy: tab gad7_band hh_healthcare_dist_rc, col

svy: ologit phq9_band i.hh_healthcare_dist_rc $ADJ, or
svy: ologit gad7_band i.hh_healthcare_dist_rc $ADJ, or

foreach c in 5 10 15 {
    capture drop phq_ge`c'
    gen phq_ge`c' = phq9_score >= `c' if !missing(phq9_score)
    svy: logistic phq_ge`c' i.hh_healthcare_dist_rc $ADJ
}
foreach c in 5 10 15 {
    capture drop gad_ge`c'
    gen gad_ge`c' = gad7_score >= `c' if !missing(gad7_score)
    svy: logistic gad_ge`c' i.hh_healthcare_dist_rc $ADJ
}

* multinomial alternative if proportional odds does not hold
* svy: mlogit phq9_band i.hh_healthcare_dist_rc $ADJ, baseoutcome(0) rrr
* svy: mlogit gad7_band i.hh_healthcare_dist_rc $ADJ, baseoutcome(0) rrr
