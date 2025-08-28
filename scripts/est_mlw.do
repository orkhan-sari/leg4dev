**# Estimations Malawi - last three waves
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"

**# Last three rounds
use "data\est\dfmlw3.dta", clear
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops slope ///
	ln_wage_income b_enterprise_income tlu subsistence_ag ///
	extension_service no_credit_access fert_voucher bean_voucher eco_soil_mng ///
	soil_fertility_index twi average_rainfall adopted_neighbors_10km
global vars female_manager primary_education_manager age_manager farm_size ///
	maize_share subsistence_ag soil_fertility_index twi average_rainfall
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 873 hhs
bysort hh_id_obs: egen mode_region = mode(region)   
replace region = mode_region // 13 obs have error in region
tabulate slope, gen(dslope)
bys hh_id_obs: egen dis_plot = max(plot_dist_household)
rename dslope1 flat_slope
rename dslope2 mod_slope

**## Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
* planting dates for maize usually june july, and for legumes end july and august

local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope mod_slope ///
	ln_wage_income b_enterprise_income tlu subsistence_ag ///
	extension_service no_credit_access fert_voucher bean_voucher eco_soil_mng ///
	soil_fertility_index twi average_rainfall 
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
qui egen adopted_neighbors_10km_mean=mean(adopted_neighbors_10km), by(hh_id_obs)
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size  maize_share other_crops i.flat_slope i.mod_slope ///
	ln_wage_income i.b_enterprise_income tlu i.subsistence_ag ///
	i.extension_service i.no_credit_access i.fert_voucher i.bean_voucher i.eco_soil_mng ///
	soil_fertility_index twi average_rainfall 
global ivlist adopted_neighbors_10km 
xtset hh_id_obs year
probit legume_intens i.female_manager $xlist $ivlist mean_* i.year adopted_neighbors_10km_mean i.region, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS i.legume_intens##i.female_manager i.year $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
margins legume_intens, at(female_manager ==1)

** HDDS one step etregress
** Mundlak correlated random effects type etregress MLE approach as in Murtuzashvili and Wooldridge (2016, Journal of Econometrics) and suggested Procedure 21.4 from Wooldridge (2010, p949)  
etregress HDDS i.legume_intens i.female_manager $xlist mean_* i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* adopted_neighbors_10km_mean i.year i.region) vce(cluster hh_id_obs)
outreg2 using "outputs\results\results_mlw", excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Base model")
margins, dydx(legume_intens female_manager) post
estimates store three
etregress HDDS i.legume_intens##i.female_manager $xlist mean_* adopted_neighbors_10km_mean i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year i.region) vce(robust)
eststo female: margins legume_intens, at(female_manager ==1) vce(unconditional) post
lincom 1.legume_intens - 0.legume_intens
local fem_diff = string(r(estimate),"%3.2f")
local fem_p    = string(r(p),"%3.2f")
qui etregress HDDS i.legume_intens##i.female_manager $xlist mean_* adopted_neighbors_10km_mean i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year i.region) vce(robust)
eststo male: margins legume_intens, at(female_manager ==0) vce(unconditional) post
lincom 1.legume_intens - 0.legume_intens
local male_diff = string(r(estimate),"%3.2f")
local male_p    = string(r(p),"%3.2f")
coefplot ///
    (female, label(Female) mcolor("90 180 160") msize(medium) ///
             ciopts(lcolor("90 180 160") lwidth(*3)) ///
             mlabcolor("90 180 160")) ///
    (male,  label(Male)  mcolor("100 150 250") msize(medium) ///
             ciopts(lcolor("100 150 250") lwidth(*3)) ///
             mlabcolor("100 150 250")), ///
    xtitle(HHDS, margin(small)) msymbol(D) mlabel format(%3.2f) mlabposition(12) ///
    coeflabel(0.legume_intens="No diversification" 1.legume_intens="Diversification")  ///
    levels(95) ciopts(lwidth(*2) lcolor(*.7)) ///
    title(Predicted dietary diversities, size(medium)) ///
    note("Marginal effects in boxes") ///
    text(1.7 8.1 " Diff: `fem_diff' (p=`fem_p')"  , box  bcolor("90 180 160") size(small)) ///
    text(2.1 8.35 " Diff: `male_diff' (p=`male_p')" , box  bcolor("100 150 250") size(small))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\hdds_etregress.tiff", as(tif) width(800) height(550) replace

* from above calculation  it is obvious that average benefit from legume intensification is higher for female managers than for male managers. 


**# All four rounds
use "data\est\dfmlwfull.dta", clear 
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size  maize_share other_crops slope ///
	ln_wage_income b_enterprise_income tlu subsistence_ag ///
	extension_service no_credit_access fert_voucher bean_voucher eco_soil_mng ///
	soil_fertility_index twi average_rainfall adopted_neighbors_10km
* Bean voucher and other soil management practices are missing and won't be included to the estimations for full sample

global vars female_manager primary_education_manager age_manager farm_size ///
	maize_share subsistence_ag soil_fertility_index twi average_rainfall
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 873 hhs
bysort hh_id_obs: egen mode_region = mode(region)   
replace region = mode_region // 13 obs have error in region
tabulate slope, gen(dslope)
bys hh_id_obs: egen dis_plot = max(plot_dist_household)
rename dslope1 flat_slope
rename dslope2 mod_slope

**## Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
* planting dates for maize usually june july, and for legumes end july and august
gen rainfall_z = (seasonal_rainfall - average_rainfall) / sd_rainfall // capturing if there was any strong deviations from average during the prodcution year. 
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size dis_plot maize_share other_crops  flat_slope mod_slope ///
	ln_wage_income b_enterprise_income tlu subsistence_ag ///
	extension_service no_credit_access fert_voucher ///
	soil_fertility_index twi average_rainfall rainfall_variation
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
qui egen adopted_neighbors_10km_mean=mean(adopted_neighbors_10km)
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size dis_plot maize_share other_crops flat_slope mod_slope ///
	ln_wage_income b_enterprise_income tlu i.subsistence_ag ///
	i.extension_service i.no_credit_access i.fert_voucher  ///
	soil_fertility_index twi average_rainfall rainfall_variation
global ivlist adopted_neighbors_10km 
xtset hh_id_obs year

** HDDS one step etregress
etregress HDDS i.legume_intens i.female_manager $xlist mean_* adopted_neighbors_10km_mean i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year i.region) vce(robust)
outreg2 using "outputs\results\results_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Base model all rounds")
margins, dydx(legume_intens female_manager) post
estimates store full
etregress HDDS i.legume_intens##i.female_manager $xlist mean_* adopted_neighbors_10km_mean i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year i.region) vce(robust)
eststo female: margins legume_intens, at(female_manager ==1) vce(unconditional) post
lincom 1.legume_intens - 0.legume_intens
local fem_diff = string(r(estimate),"%3.2f")
local fem_p    = string(r(p),"%3.2f")
qui etregress HDDS i.legume_intens##i.female_manager $xlist mean_* adopted_neighbors_10km_mean i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year i.region) vce(robust)
eststo male: margins legume_intens, at(female_manager ==0) vce(unconditional) post
lincom 1.legume_intens - 0.legume_intens
local male_diff = string(r(estimate),"%3.2f")
local male_p    = string(r(p),"%3.2f")
coefplot ///
    (female, label(Female) mcolor("90 180 160") msize(medium) ///
             ciopts(lcolor("90 180 160") lwidth(*3)) ///
             mlabcolor("90 180 160")) ///
    (male,  label(Male)  mcolor("100 150 250") msize(medium) ///
             ciopts(lcolor("100 150 250") lwidth(*3)) ///
             mlabcolor("100 150 250")), ///
    xtitle(HHDS, margin(small)) msymbol(D) mlabel format(%3.2f) mlabposition(12) ///
    coeflabel(0.legume_intens="No diversification" 1.legume_intens="Diversification")  ///
    levels(95) ciopts(lwidth(*2) lcolor(*.7)) ///
    title(Predicted dietary diversities, size(medium)) ///
    note("Marginal effects in boxes") ///
	text(1.7 7.6 " Diff: `fem_diff' (p=`fem_p')"  , box  bcolor("90 180 160") size(small)) ///
	text(2.1 8.2 " Diff: `male_diff' (p=`male_p')" , box  bcolor("100 150 250") size(small))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\hdds_etregressfull.tiff", as(tif) width(800) height(550) replace

coefplot ///
    (three, label("Three waves") mcolor("90 180 160") msize(medium) ///
           ciopts(lcolor("90 180 160") lwidth(*2))) ///
    (full, label("Four waves") mcolor("100 150 250") msize(medium) ///
          ciopts(lcolor("100 150 250") lwidth(*2))) ///
, xline(0) ///
  xtitle("Average marginal effects") ///
  coeflabels(1.legume_intens = "Legume diversification" ///
             1.female_manager = "Female manager")
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\effectsalwaavesmlw.tiff", as(tif) width(800) height(550) replace
