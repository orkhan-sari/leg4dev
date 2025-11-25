**# Estimations Ethiopia
**## Drop missing
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"
use "data\est\dfeth.dta", clear

**## descriptive tests
tab female_dm legume_intens, chi2 col row
kwallis legume_intens, by(female_dm)
kwallis HDDS, by(female_dm)

**## Estimations
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size count_plot maize_share other_crops flat_slope plot_dist_household ///
	ln_other_income tlu subsistence_ag psnp_labour psnp_assistance ///
	extension_program advisory_service credit_unavailable eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km dist_market
global vars female_manager primary_education_manager age_manager plot_dist_household
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 487  panel hhs 
bys hh_id_obs: egen dis_market_m = mean(dist_market)
gen check = 1 if dis_market_m != dist_market 
* dis to market varies for around 144 hhs; they may have moved to a different location becuase their location data is also different 
drop check dis_market_m

global xlist i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu i.subsistence_ag psnp_labour psnp_assistance ///
	i.extension_program i.advisory_service i.credit_unavailable i.eco_soil_mng ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg ///
	i.year i.region
global ivlist adopted_neighbors_10km 

* Hausman test
xtset hh_id_obs year
xtlogit legume_intens $xlist $ivlist , fe
* does not convergence
xtreg legume_intens $xlist $ivlist , fe
estimates store fixed
xtreg legume_intens $xlist $ivlist, re
estimates store random
hausman fixed random
* logit fixed does not converge and 306 groups (612 obs) omitted because of all positive or all negative outcomes.
* for 63% of the sample the outcome within hh does not change, thus FE keeps only 362 obs (182 groups) and
* nevertheless time-invariant unobserved hetrogeneity shall not be ignored.
* xtreg results show fied effects are more appropriate. 

**## Legume intensification and gender
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu subsistence_ag psnp_labour psnp_assistance ///
	extension_program advisory_service credit_unavailable eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km  
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}

xtlogit legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\supplementary", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Ethiopia") addnote(Bootstrapped standard errors in paranthesis)
xtreg legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_eth", excel replace  bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects LPM") addnote(Bootstrapped standard errors in paranthesis; Hausman test that indicates random-effects is appropriate)
xtprobit legume_intens $xlist $ivlist mean_*, re vce(cluster hh_id_obs)
outreg2 using "outputs\results\determinants_eth", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("CRE probit")

**## Main estimation - xtivreg: HDDS, FCS using IV Wooldridge 2010 procedure 21.1 
** Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Ethiopia HDDS")
* NO EFFECT OF LEGUME DIVERSIFICATION 
