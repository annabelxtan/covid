*Table 2 Negative Binomial Regressions
*updated 22 feb 2021

**************************CASES**********************************************************

use "C:\Users\annabelx\Box\Annabel Tan's Files\COVID-19\Created Datasets\covid_cases_edit_03_01_21.dta", replace

replace cases = 0 if cases < 0 

gen logpop = log(population)

//mask use is a string -- need to convert to integers
gen mask_never_new = real(mask_never)
gen mask_sometimes_new = real(mask_sometimes)
gen mask_frequently_new = real(mask_frequently)
gen mask_always_new = real(mask_always)

*unadjusted
*march/april is the exp(gini_coeff_dev) 
nbreg cases gini_coeff_dev ib1.time c.gini_coeff_dev#ib1.time, offset(logpop) irr

*march/april
*RR = 1.307517, 95%CI: 1.253233, 1.364153

*may/june
lincom  _b[gini_coeff_dev] + _b[2.time#c.gini_coeff], irr
*RR = 1.28 (1.23, 1.3378)

*july august
lincom  _b[gini_coeff_dev] + _b[3.time#c.gini_coeff], irr
*RR = 1.38(1.33, 1.44)

*sep oct
lincom  _b[gini_coeff_dev] + _b[4.time#c.gini_coeff], irr
*RR = 0.94 (0.90, 0.98)

*nov dec
lincom  _b[gini_coeff_dev] + _b[5.time#c.gini_coeff], irr
*RR = 0.885 (0.848, 0.923)

*jan feb
lincom  _b[gini_coeff_dev] + _b[6.time#c.gini_coeff], irr
*coeff =  .075199 , 95% CI: (   .0313761,    .1190219) 
*RR =1.074 ( 1.028, 1.122)

nbreg cases c.gini_coeff_dev##c.time 
*p-value over epochs  (for trend) = <0.001

*adjusted
*march/april is the exp(gini_coeff_dev)
*to find main effects + interaction coefficient thru lincom
nbreg cases gini_coeff_dev percbelowpoverty_noperc ///
			age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			i.state_no ///
			ib1.time c.gini_coeff_dev#ib1.time, offset(logpop) irr

*march april
*RR = 1.18, 95%CI: 1.13, 1.24

*may june
lincom  _b[gini_coeff_dev] + _b[2.time#c.gini_coeff], irr 
*RR =   1.23, 95%CI: 1.18, 1.29

*july august
lincom  _b[gini_coeff_dev] + _b[3.time#c.gini_coeff], irr 
*RR = 1.28, 95%CI: 1.22, 1.33

*sep october
lincom  _b[gini_coeff_dev] + _b[4.time#c.gini_coeff], irr
*RR =  0.90, 95%CI: 0.87, 0.94

*nov dec 
lincom  _b[gini_coeff_dev] + _b[5.time#c.gini_coeff], irr
*RR =  0.85, 95%CI: 0.81, 0.88

*jan feb
lincom  _b[gini_coeff_dev] + _b[6.time#c.gini_coeff], irr
*RR = 1.02, 95%CI: 0.98, 1.07

*to find p-value for trend (p<0.001)
nbreg cases age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			state_no ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			c.gini_coeff_dev##c.time, offset(logpop) 

//global test for interaction
nbreg cases c.gini_coeff_dev##i.time
estimates store A
nbreg cases c.gini_coeff_dev i.time
estimates store B
lrtest A B 
//p < 0.0001

//global test for interaction - adjisted

nbreg cases age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			state_no ///
			c.gini_coeff_dev##i.time
estimates store A
nbreg cases  age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			state_no ///
			c.gini_coeff_dev i.time
estimates store B
lrtest A B
//p < 0.00001

			
			
//testing for interaction between poverty and gini coefficients 

univar percbelowpoverty_noperc
//median is 0.36

//indicator variable for poverty 
gen poverty = 0 
replace poverty = 1 if percbelowpoverty_noperc < 0.36
replace poverty=. if percbelowpoverty_noperc ==.
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty, offset(logpop) irr // interaction p-value = 0.476
nbreg cases c.gini_coeff_dev##ib0.poverty##c.time, offset(logpop) irr // ************

nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 1, offset(logpop) irr // interaction p-value = 0.052
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 2, offset(logpop) irr // interaction p-value = 0.366
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 3, offset(logpop) irr // interaction p-value = 0.179
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 4, offset(logpop) irr // interaction p-value = 0.909
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 5, offset(logpop) irr // interaction p-value = 0.984
nbreg cases c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 6, offset(logpop) irr // interaction p-value = 0.545

//interaction btwn gini and poverty level when looking at covid-19 cases is not significant
			

*************************DEATHS*********************************************************
use "C:\Users\annabelx\Box\Annabel Tan's Files\COVID-19\Created Datasets\covid_deaths_edit_03_01_21.dta", replace
replace deaths = 0 if deaths < 0 

gen logpop = log(population)

//mask use is a string -- need to convert to integers
gen mask_never_new = real(mask_never)
gen mask_sometimes_new = real(mask_sometimes)
gen mask_frequently_new = real(mask_frequently)
gen mask_always_new = real(mask_always)


*to find main effects + interaction thru lincom
*march apr is just the exp(gini_coeff_dev) -- dont use lincom
nbreg deaths gini_coeff_dev ib1.time c.gini_coeff_dev#ib1.time, offset(logpop) irr

*march apr
*RR = 1.536175, 95%CI: 1.44264, 1.635774

*may/june
lincom  _b[gini_coeff_dev] + _b[2.time#c.gini_coeff], irr
*RR =    1.46525, 95%CI: 1.378873, 1.557037

*july/aug
lincom  _b[gini_coeff_dev] + _b[3.time#c.gini_coeff], irr
*RR = 1.697051, 95%CI: 1.59211, 1.808909

*sep/oct 
lincom  _b[gini_coeff_dev] + _b[4.time#c.gini_coeff], irr 
*RR =  1.181049, 95%CI: 1.111512, 1.254935

*nov/dec
lincom  _b[gini_coeff_dev] + _b[5.time#c.gini_coeff], irr 
*RR = 0.8319391, 95%CI: 0.7853305, 0.8813139

*jan/feb
lincom  _b[gini_coeff_dev] + _b[6.time#c.gini_coeff], irr 
*RR =  1.113, 95%CI: 1.051, 1.180

*to find p-value trends over time
nbreg deaths c.gini_coeff_dev##c.time 

*unadjusted p-value over epochs = <0.001

*adjusted
*to find main effects + interaction coefficient thru lincom
nbreg deaths gini_coeff_dev percbelowpoverty_noperc ///
			age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			i.state_no ///
			ib1.time c.gini_coeff_dev#ib1.time, offset(logpop) irr

			
*march/apr
*RR = 1.245; 95% CI: 1.167, 1.3284

*may june
lincom  _b[gini_coeff_dev] + _b[2.time#c.gini_coeff], irr
*RR = 1.20, 95%CI: 1.13, 1.28

*july aug
lincom  _b[gini_coeff_dev] + _b[3.time#c.gini_coeff], irr
*RR =  1.46, 95% CI: 1.37, 1.55

*sep oct
lincom  _b[gini_coeff_dev] + _b[4.time#c.gini_coeff], irr
*RR =   1.04, 95% CI: 0.983, 1.104

*nov dec
lincom  _b[gini_coeff_dev] + _b[5.time#c.gini_coeff], irr
*RR =   0.76, 95%CI: 0.72, 0.81

*jan feb
lincom  _b[gini_coeff_dev] + _b[6.time#c.gini_coeff], irr
*RR =   1.02, 95% CI: 0.96, 1.07

*to find p-value over time

nbreg deaths gini_coeff_dev percbelowpoverty_noperc ///
			age_under25_prop age_25to39_prop age_40to65_prop age_65to79_prop age_80to84_prop age_over85_prop ///
			race_white_prop race_black_prop race_asian_prop race_native_prop race_hawaiian_prop race_hispanic_prop ///
			crowd_owner_pointfive_prop crowd_owner_lessone_prop crowd_owner_lessoneptfive_prop crowd_owner_lesstwo_prop crowd_owner_morethantwo_prop ///
			crowd_renter_pointfive_prop crowd_renter_lessone_prop crowd_renter_lessoneptfive_prop crowd_renter_lesstwo_prop crowd_renter_morethantwo_prop ///
			urbanpercentage ruralpercentage ///
			educ_lessthanhighschool_noperc educ_highschool_noperc educ_somecollege_noperc educ_college_noperc ///
			md_rate ///
			i.state_no ///
			mask_never_new mask_sometimes_new mask_frequently_new mask_always_new ///
			c.gini_coeff_dev##c.time, offset(logpop) 
			

*adjusted model p-value over epochs = <0.001

//testing for interaction between poverty and gini coefficients 

univar percbelowpoverty_noperc
//median is 0.36

//indicator variable for poverty 
gen poverty = 0 
replace poverty = 1 if percbelowpoverty_noperc < 0.36
replace poverty=. if percbelowpoverty_noperc ==.

//three way interaction -- all significant?
nbreg deaths c.gini_coeff_dev##ib0.poverty##c.time, offset(logpop) irr // ************

nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty, offset(logpop) irr // interaction p-value = 0.789
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 1, offset(logpop) irr // interaction p-value = 0.039
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 2, offset(logpop) irr // interaction p-value = 0.080
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 3, offset(logpop) irr // interaction p-value = 0.197
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 4, offset(logpop) irr // interaction p-value = 0.074
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 5, offset(logpop) irr // interaction p-value = 0.705
nbreg deaths c.gini_coeff_dev ib0.poverty c.gini_coeff_dev#ib0.poverty if time == 6, offset(logpop) irr // interaction p-value = 0.034

//interaction btwn gini and poverty level is not significant
