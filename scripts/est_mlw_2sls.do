**# Estimations Malawi - last three waves
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color

**# Last three rounds
use "data\est\dfmlw3.dta", clear

***## descriptive tests
tab female_dm legume_intens, chi2 col row
kwallis legume_intens, by(female_dm)
kwallis HDDS, by(female_dm)
ranksum HDDS if female_dm !=1, by(female_dm ) porder
ranksum HDDS if female_dm !=2, by(female_dm ) porder
ranksum HDDS if female_dm !=3, by(female_dm ) porder
kwallis fcs, by(female_dm)
ranksum fcs if female_dm !=1, by(female_dm ) porder
ranksum fcs if female_dm !=2, by(female_dm ) porder
ranksum fcs if female_dm !=3, by(female_dm ) porder

**## Estimations
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops flat_slope mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher bean_voucher eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km 
global vars female_manager primary_education_manager age_manager ///
	maize_share flat_slope plot_dist_household subsistence_ag soil_fertility_index twi anntot_avg
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 868 hhs

global xlist i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher i.bean_voucher i.eco_soil_mng ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg ///
	i.region i.year
global ivlist adopted_neighbors_10km 

* Hausman test
xtset hh_id_obs year
xtlogit legume_intens $xlist $ivlist, fe
estimates store fixed
xtlogit legume_intens $xlist $ivlist, re
estimates store random
hausman fixed random
drop _est_fixed _est_random
* fixed effects is indeed more appropriate. However,
* 340 groups (1,020 obs; 39%)  omitted because of all positive or all negative outcomes.

**# Legume intensification and gender
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher bean_voucher eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg ///
	adopted_neighbors_10km
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}

xtlogit legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\supplementary", excel replace bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi") addnote(Bootstrapped standard errors in paranthesis)
xtreg legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_mlw", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects LPM") addnote(Bootstrapped standard errors in paranthesis; Hausman test indicates that fixed-effects is appropriate)
xtprobit legume_intens $xlist $ivlist mean_*, re vce(cluster hh_id_obs)
outreg2 using "outputs\results\determinants_mlw", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("CRE probit")

**## Main estimation - xtivreg: HDDS, FCS using IV Wooldridge 2010 procedure 21.1 
* total consumption - missing for wave 4 and 5
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel replace bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi HDDS")
xtivreg fcs $xlist (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi FCS")

**## Interaction models
**### HDDS
graph set window fontface "Trebuchet MS"
global intlist i.legume_intens i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher i.bean_voucher i.eco_soil_mng ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg 
			
xtpoisson HDDS i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) xline(0, lwidth(medthick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("HDDS") name(gender, replace)

qui xtpoisson HDDS i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 22)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue  )) yline(0, lwidth(medthick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2)) xlabel(0(2)22)  addplot(histogram other_crops, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)22) ) legend(off) ytitle("") name(crop, replace)

qui xtpoisson HDDS i.legume_intens#c.hh_asset_index $intlist, fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.65(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Asset index (PCA)", size(small) margin(0 0 0 2)) addplot(histogram hh_asset_index, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(-0.70(0.5)5)) legend(off) ytitle("") name(asset, replace)

qui xtpoisson HDDS i.legume_intens#c.ln_other_income $intlist, fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)16)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2)) xlabel(0(1)16)  addplot(histogram ln_other_income, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)16) ) legend(off) ytitle("") name(income, replace)

graph combine gender crop asset income , col(2) ///
title("Average Marginal Effects of Legume Intensification", size(medsmall)) ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_hdds.tiff", as(tif) height(1200) replace


**### FCS
qui xtreg fcs i.legume_intens#i.female_manager $intlist, fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) xline(0, lwidth(medthick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("FCS") name(gender, replace)

qui xtreg fcs i.legume_intens#c.other_crops $intlist, fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 22)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue  )) yline(0, lwidth(medthick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2)) xlabel(0(2)22)  addplot(histogram other_crops, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)22) ) legend(off) ytitle("") name(crop, replace)

qui xtreg fcs i.legume_intens#c.hh_asset_index $intlist, fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.65(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Asset index (PCA)", size(small) margin(0 0 0 2)) addplot(histogram hh_asset_index, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(-0.70(0.5)5)) legend(off) ytitle("") name(asset, replace)

qui xtreg fcs i.legume_intens#c.ln_other_income $intlist, fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)16)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2)) xlabel(0(1)16)  addplot(histogram ln_other_income, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)16) ) legend(off) ytitle("") name(income, replace)

graph combine gender crop asset income , col(2) ///
title("Average Marginal Effects of Legume Intensification", size(medsmall)) ///
note("Linear fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_fcs.tiff", as(tif) height(1200) replace



**# All four rounds
use "data\est\dfmlwfull.dta", clear
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops flat_slope mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher bean_voucher eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km 
global vars female_manager primary_education_manager age_manager ///
	maize_share flat_slope subsistence_ag soil_fertility_index twi anntot_avg
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 615 hhs

global xlist i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg ///
	i.region i.year
global ivlist adopted_neighbors_10km 

local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher ///
	iv_maize organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi anntot_avg ///
	adopted_neighbors_10km
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}

* Hausman test
xtset hh_id_obs year
xtlogit legume_intens $xlist $ivlist, fe
estimates store fixed
xtlogit legume_intens $xlist $ivlist, re
estimates store random
hausman fixed random
drop _est_fixed _est_random

xtreg legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_mlw4", excel replace bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects LPM") addnote(Bootstrapped standard errors in paranthesis; Hausman test indicates that fixed-effects is appropriate)
xtprobit legume_intens $xlist $ivlist mean_*, re vce(cluster hh_id_obs)
outreg2 using "outputs\results\determinants_mlw4", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("CRE probit")

**## Main estimation - xtivreg: HDDS, FCS using IV Wooldridge 2010 procedure 21.1 
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results4", excel replace bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi HDDS")
xtivreg fcs $xlist (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results4", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi FCS")

**## Interaction models
**### HDDS
graph set window fontface "Trebuchet MS"
global intlist i.legume_intens i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope plot_dist_household ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg ///
	adopted_neighbors_10km
	
xtpoisson HDDS i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) xline(0, lwidth(medthick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("HDDS") name(gender, replace)

qui xtpoisson HDDS i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 22)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue  )) yline(0, lwidth(medthick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2)) xlabel(0(2)22)  addplot(histogram other_crops, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)22) ) legend(off) ytitle("") name(crop, replace)

qui xtpoisson HDDS i.legume_intens#c.hh_asset_index $intlist , fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.65(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Asset index (PCA)", size(small) margin(0 0 0 2)) addplot(histogram hh_asset_index, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(-0.70(0.5)5)) legend(off) ytitle("") name(asset, replace)

qui xtpoisson HDDS i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)16)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2)) xlabel(0(1)16)  addplot(histogram ln_other_income, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)16) ) legend(off) ytitle("") name(income, replace)

graph combine gender crop asset income , col(2) ///
title("Average Marginal Effects of Legume Intensification", size(medsmall)) ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_hdds4.tiff", as(tif) height(1200) replace

xtpoisson fcs i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) xline(0, lwidth(medthick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("FCS") name(gender, replace)

qui xtpoisson fcs i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 22)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue  )) yline(0, lwidth(medthick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2)) xlabel(0(2)22)  addplot(histogram other_crops, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)22) ) legend(off) ytitle("") name(crop, replace)

qui xtpoisson fcs i.legume_intens#c.hh_asset_index $intlist , fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.65(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Asset index (PCA)", size(small) margin(0 0 0 2)) addplot(histogram hh_asset_index, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(-0.70(0.5)5)) legend(off) ytitle("") name(asset, replace)

qui xtpoisson fcs i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)16)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(medium) lpattern(dash) lcolor(midblue )) yline(0, lwidth(medthick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2)) xlabel(0(1)16)  addplot(histogram ln_other_income, percent fcolor(gs0%35) lcolor(gs0%15) lpattern(solid) yaxis(2) yscale(alt axis(2)) xlabel(0(2)16) ) legend(off) ytitle("") name(income, replace)

graph combine gender crop asset income , col(2) ///
title("Average Marginal Effects of Legume Intensification", size(medsmall)) ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_fcs4.tiff", as(tif) height(1200) replace

