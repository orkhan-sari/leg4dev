**# Estimations Malawi - last three waves
**## Drop missing
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"
use "data\est\dfmlw3.dta", clear
mdesc legume_intens female_manager primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation slope ln_wage_income b_enterprise_income tlu subsistence_ag extension_service no_credit_access fert_voucher bean_voucher eco_soil_mng soil_fertility_index rainfall_variation seasonal_rainfall
global vars female_manager primary_education_manager age_manager maize_share elevation subsistence_ag soil_fertility_index seasonal_rainfall
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 866 hhs


**# Panel data switching regression with control function - Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
tabulate region, gen(regiond)
tabulate slope, gen(dslope)
rename dslope1 flat_slope
rename dslope2 mod_slope
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation flat_slope mod_slope ln_wage_income b_enterprise_income tlu subsistence_ag extension_service no_credit_access fert_voucher bean_voucher regiond2 regiond3 eco_soil_mng soil_fertility_index rainfall_variation seasonal_rainfall
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation flat_slope mod_slope ln_wage_income b_enterprise_income tlu i.subsistence_ag i.extension_service i.no_credit_access i. fert_voucher i.bean_voucher i.eco_soil_mng soil_fertility_index seasonal_rainfall
global ivlist rainfall_variation
xtset hh_id_obs year
xtprobit legume_intens i.female_manager $xlist $ivlist mean_* i.year i.region, re vce(robust)
outreg2 using "outputs\results\legume_intensification", excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Legume diversification CRE model")
predict xb, xb

**## HDDS
xtivreg HDDS i.female_manager##i.legume_intens i.year i.region $xlist (legume_intens=xb), fe vce(bootstrap, rep(100))
eststo female: margins legume_intens, at(female_manager ==1) post
lincom 1.legume_intens - 0.legume_intens
local fem_diff = string(r(estimate),"%3.2f")
local fem_p    = string(r(p),"%3.2f")
qui xtivreg HDDS i.female_manager##i.legume_intens i.year i.region $xlist (legume_intens=xb), fe vce(bootstrap, rep(100))
eststo male: margins legume_intens, at(female_manager ==0) post
lincom 1.legume_intens - 0.legume_intens
local male_diff = string(r(estimate),"%3.2f")
local male_p    = string(r(p),"%3.2f")
coefplot ///
	(female, label(Female) msymbol(D) mcolor("200 240 200") ///
             ciopts(lcolor("200 240 200") lwidth(*5))) ///
	(male,  label(Male)   msymbol(D) mcolor("255 230 230")    ///
             ciopts(lcolor("255 230 230") lwidth(*5))), ///
	xtitle(HHDS, margin(small)) msymbol(D) mlabel format(%3.2f) mlabposition(12) ///
	coeflabel(0.legume_intens="No diversification" 1.legume_intens="Diversification")  ///
	levels(95) ciopts(lwidth(*2) lcolor(*.7)) ///
	title(Predicted dietary diversities, size(medium)) ///
	note("Marginal effects in boxes") ///
	text(1.7 8 " `fem_diff' (p=`fem_p')"  , box  bcolor("200 240 200") size(small)) ///
	text(2 9.2 " `male_diff' (p=`male_p')" , box  bcolor("255 230 230") size(small))
	
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\hdds.tiff", as(tif) width(800) height(550) replace

**## Consumption








* from above calculation  it is obvious that average benefit from legume intensification is higher for female managers than for male managers. 

* ETREGRESS one step 
etregress HDDS i.female_manager##i.legume_intens $xlist i.year, treat(legume_intens= i.female_manager $xlist $ivlist mean_* i.year ) vce(bootstrap)
margins legume_intens, at(female_manager ==1) post 
lincom 1.legume_intens - 0.legume_intens


**# Estimations Malawi - all four waves
cd "C:\Users\490A\Nextcloud\Research\leg4dev\data\est"
use dfmlwfull.dta, clear
mdesc legume_intens female_manager primary_education_manager  age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher  weather_shock soil soil_fertility_index seasonal_rainfall
global vars legume_intens  female_manager formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income hh_asset_index tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher  weather_shock soil soil_fertility_index
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 866 hhs


**# Panel data switching regression with control function - Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
tabulate soil, gen(soild)
tabulate region, gen(regiond)
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share  ln_wage_income b_enterprise_income  tlu subsistence_ag extension_service  fert_voucher maize_voucher weather_shock regiond2 regiond3 eco_soil_mng soil rainfall_variation seasonal_rainfall 
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
local i=`i'+1 
}
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share   ln_wage_income b_enterprise_income  tlu i.subsistence_ag i.extension_service i. fert_voucher i.maize_voucher  i.weather_shock  i.region i.soil seasonal_rainfall 
global ivlist rainfall_variation
xtset hh_id_obs year
xtprobit legume_intens i.female_manager $xlist $ivlist mean_* i.year, re vce(robust)
predict xb, xb
xtivreg HDDS i.female_manager##i.legume_intens i.year $xlist (legume_intens=xb), fe vce(robust)
margins legume_intens, at(female_manager ==1) 
lincom 1.legume_intens - 0.legume_intens
qui xtivreg HDDS i.female_manager##i.legume_intens $xlist (legume_intens=xb), fe vce(bootstrap)
margins legume_intens, at(female_manager ==0) post
lincom 1.legume_intens - 0.legume_intens
* from above calculation  it is obvious that average benefit from legume intensification is higher for female managers than for male managers. 


**# Estimations Malawi - last two waves four waves
cd "C:\Users\490A\Nextcloud\Research\leg4dev\data\est"
use dfmlw2.dta, clear
mdesc legume_intens female_manager formal_education_manager  age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher  weather_shock soil soil_fertility_index seasonal_rainfall
global vars legume_intens  female_manager formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income hh_asset_index tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher  weather_shock soil soil_fertility_index
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 866 hhs


**# Panel data switching regression with control function - Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
tabulate soil, gen(soild)
tabulate region, gen(regiond)
gen till=0
replace till=1 if zero_till ==1
replace till=1 if min_till  ==1
local temp female_manager formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share  ln_wage_income b_enterprise_income  tlu subsistence_ag extension_service  fert_voucher maize_voucher weather_shock regiond2 regiond3 eco_soil_mng soil_fertility_index rainfall_variation seasonal_rainfall 
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
local i=`i'+1 
}
global xlist i.formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share   ln_wage_income b_enterprise_income  tlu i.subsistence_ag i.extension_service i. fert_voucher i.maize_voucher  i.weather_shock  i.region i.eco_soil_mng soil_fertility_index seasonal_rainfall 
global ivlist rainfall_variation
xtset hh_id_obs year
xtprobit legume_intens i.female_manager $xlist $ivlist mean_* i.year, re vce(robust)
predict xb, xb
xtivreg HDDS i.female_manager##i.legume_intens i.year $xlist (legume_intens=xb), fe vce(robust)
margins legume_intens, at(female_manager ==1) 
lincom 1.legume_intens - 0.legume_intens
qui xtivreg HDDS i.female_manager##i.legume_intens $xlist (legume_intens=xb), fe vce(bootstrap)
margins legume_intens, at(female_manager ==0) post
lincom 1.legume_intens - 0.legume_intens
* from above calculation  it is obvious that average benefit from legume intensification is higher for female managers than for male managers. 





outreg2 using legume_intens, excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Legume intensification extended")

xtprobit legume_intens i.year $xlist, vce(bootstrap) 