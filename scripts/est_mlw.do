**# Estimations Malawi
**## Drop missing
cd "C:\Users\490A\Nextcloud\Research\leg4dev\data\est"
use dfmlw3.dta, clear
mdesc legume_intens female_manager formal_education_manager  age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher bean_voucher weather_shock soil soil_fertility_index seasonal_rainfall
global vars legume_intens  female_manager formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation ln_wage_income b_enterprise_income hh_asset_index tlu subsistence_ag extension_service no_credit_access fert_voucher maize_voucher bean_voucher weather_shock soil soil_fertility_index
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 866 hhs


**# Panel data switching regression with control function
tabulate soil, gen(soild)
tabulate region, gen(regiond)
gen till=0
replace till=1 if zero_till ==1
replace till=1 if min_till  ==1
local temp female_manager formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share  ln_wage_income b_enterprise_income  tlu subsistence_ag extension_service fert_voucher maize_voucher bean_voucher weather_shock regiond2 regiond3 eco_soil_mng soil_fertility_index rainfall_variation seasonal_rainfall 
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
local i=`i'+1 
}
global xlist i.formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share   ln_wage_income b_enterprise_income  tlu i.subsistence_ag i.extension_service i. fert_voucher i.maize_voucher i.bean_voucher i.weather_shock  i.region i.eco_soil_mng soil_fertility_index seasonal_rainfall 
global ivlist rainfall_variation
xtset hh_id_obs year
probit legume_intens i.female_manager $xlist $ivlist mean_* i.year, cluster(hh_id_obs)
predict xb, xb
xtivreg HDDS i.female_manager##i.legume_intens $xlist (legume_intens=xb), fe vce(robust)
margins legume_intens, at(female_manager ==1) post 
lincom 1.legume_intens - 0.legume_intens
qui xtivreg HDDS i.female_manager##i.legume_intens $xlist (legume_intens=xb), fe vce(robust)
margins legume_intens, at(female_manager ==0) post
lincom 1.legume_intens - 0.legume_intens
* from above calculation  it is obvious that average benefit from legume intensification is higher for female managers than for male managers. 

* CRE probit
xtprobit legume_intens $xlist $ivlist mean_* i.year, re vce(robust)




mlogit agr_intens $xlist mean_* i.year, cluster(hh_id_obs) base(1)
margins, dydx(*)


tab1 agroecology b_agroecology legume_intens eco_soil_mng
// soil management techniques are rarely adopted
global xlist i.female_manager i.formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation  ln_wage_income b_enterprise_income ag_asset_index tlu i.subsistence_ag i.extension_service i.no_credit_access i.fert_voucher i.maize_voucher i.bean_voucher i.weather_shock i.soil i.region
xtlogit legume_intens $xlist i.year, fe
outreg2 using legume_intens, excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Legume intensification")
xtprobit legume_intens i.female_manager##i.formal_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share ln_wage_income b_enterprise_income ag_asset_index tlu i.subsistence_ag i.extension_service i.no_credit_access i.fert_voucher i.maize_voucher i.bean_voucher i.weather_shock i.soil i.region i.year, vce(robust)
outreg2 using legume_intens, excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Legume intensification extended")

xtprobit legume_intens i.year $xlist, vce(bootstrap) 