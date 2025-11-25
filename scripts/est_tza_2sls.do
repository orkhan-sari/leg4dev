**# Estimations Tanzania
**## Drop missing
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"
use "data\est\dftza.dta", clear

**## ranksum test
ranksum HDDS if female_dm !=1, by(female_dm ) porder
ranksum HDDS if female_dm !=2, by(female_dm ) porder
ranksum HDDS if female_dm !=3, by(female_dm ) porder

**## Estimations
mdesc HDDS female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu subsistence_ag  ///
	extension_service credit crop_residue_short crop_residue_long ///
	iv_maize organic_fertilizer inorganic_fertilizer  ///
	average_rainfall adopted_neighbors_10km zone
* 5 mising for subssitence since they did not have harvest. 
global vars HDDS subsistence_ag
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 510 hhs

global xlist i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size i.plot_certificate maize_share other_crops i.flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu i.subsistence_ag  ///
	i.extension_service i.credit i.crop_residue_short i.crop_residue_long ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	average_rainfall ///
	i.zone i.year
global ivlist adopted_neighbors_10km 

*Hausman test
xtset hh_id_obs year
xtlogit legume_intens $xlist $ivlist, fe
estimates store fixed
xtlogit legume_intens $xlist $ivlist, re
estimates store random
hausman fixed random
* pooled specification is more appropriate
* 3105 groups (610 obs) omitted because of all positive or all negative outcomes. 
* for 60% of the sample the outcome within hh does not change, thus FE keeps only 414 obs
* with limited status changes, standard errors explode (small number of switchers leads to high variance)

**## Legume intensification and gender
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu subsistence_ag  ///
	extension_service credit crop_residue_short crop_residue_long ///
	iv_maize organic_fertilizer inorganic_fertilizer  ///
	average_rainfall adopted_neighbors_10km
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}

xtlogit legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\supplementary", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Tanzania") addnote(Bootstrapped standard errors in paranthesis)
xtreg legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_tz", excel replace bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Random effects Probit") addnote(Bootstrapped standard errors in paranthesis; Hausman test that indicates random-effects is appropriate)
xtprobit legume_intens $xlist $ivlist mean_*, re vce(cluster hh_id_obs)
outreg2 using "outputs\results\determinants_tz", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("CRE probit")
logit legume_intens $xlist $ivlist, vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_tz", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Pooled Logit")

**## Main estimation - xtivreg: HDDS, FCS using IV Wooldridge 2010 procedure 21.1 
* total consumption - missing for wave 4 and 5
** Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append  bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Tanzania HDDS")
xtivreg fcs $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Tanzania FCS")



































