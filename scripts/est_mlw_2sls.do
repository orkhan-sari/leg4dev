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
	farm_size plot_certificate maize_share other_crops flat_slope mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher bean_voucher eco_soil_mng  organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall adopted_neighbors_10km iv_maize
global vars female_manager primary_education_manager age_manager ///
	maize_share flat_slope ln_dis_plot subsistence_ag soil_fertility_index twi rainfall_diff average_rainfall 
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 867 hhs

**## Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
* planting dates for maize usually june july, and for legumes end july and august
global xlist i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher i.bean_voucher i.eco_soil_mng ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall ///
	i.region i.year
global ivlist adopted_neighbors_10km 
xtset hh_id_obs year
xtlogit legume_intens $xlist $ivlist, fe
estimates store fixed
xtlogit legume_intens $xlist $ivlist, re
estimates store random
hausman fixed random
drop _est_fixed _est_random
* fixed effects is more appropriate
* FE,  340 groups (1,020 obs)  omitted because of all positive or all negative outcomes.

**# Legume intensification and gender
xtlogit legume_intens $xlist $ivlist, fe vce(bootstrap , rep(50))
outreg2 using "outputs\results\determinants_mlw", excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects logit") addnote(Bootstrapped standard errors in paranthesis; Hausman test indicates that fixed-effects is appropriate)
xtlogit legume_intens $xlist $ivlist, re vce(bootstrap , rep(50))
outreg2 using "outputs\results\determinants_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Random effects logit")
logit legume_intens $xlist $ivlist, vce(bootstrap , rep(50))
outreg2 using "outputs\results\determinants_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Pooled Logit")

**## Main estimation - 2SLS: HDDS, food consumption score
* total consumption - missing for wave 4 and 5
** Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service credit_constrained fert_voucher bean_voucher eco_soil_mng organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall iv_maize adopted_neighbors_10km
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi HDDS")
xtivreg fcs $xlist (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi FCS")

**## Interaction models - to understand the moderator role of manager's gender
**### HDDS
global intlist i.legume_intens i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.credit_constrained i.fert_voucher i.bean_voucher i.eco_soil_mng ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall ///
	i.region i.year
xtpoisson HDDS i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("HDDS")  name(figgender, replace)
* the effect is statistically signifficant for female managers

qui xtpoisson HDDS i.legume_intens#i.inorganic_fertilizer $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(inorganic_fertilizer=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "No fertilizer" 1 "Fertilizer") title("HDDS")  name(figfert, replace)

qui xtpoisson HDDS i.legume_intens#c.hh_asset_index $intlist , fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.60(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Asset index (PCA)", margin(0 0 0 2)) ytitle("") name(fig1, replace)

qui xtpoisson HDDS i.legume_intens#c.rainfall_diff $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(rainfall_diff=(-285(35)305)) post 
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Rainfall deviation from historic average (mm)", margin(0 0 0 2)) xlabel(-250(75)300)  ytitle("") name(fig2, replace)

qui xtpoisson HDDS i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)15)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2) ) name(fig3, replace)

qui xtpoisson HDDS i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 9)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2) ) name(fig4, replace)

graph combine fig1 fig2 fig3 fig4, col(2) ///
title("Average Marginal Effects of Legume Intensification") ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_hdds.tiff", as(tif) width(800) height(550) replace

**### FCS
xtreg fcs i.legume_intens#i.female_manager $intlist, fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("FCS") name(figgender1, replace)
* no evidence of effect heterogeneity

qui xtpoisson fcs i.legume_intens#i.inorganic_fertilizer $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(inorganic_fertilizer=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "No fertilizer" 1 "Fertilizer") title("FCS")  name(figfert1, replace)

graph combine figgender figgender1 figfert figfert1, col(2) ///
title("Average Marginal Effects of Legume Intensification") ///
note("dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_gender.tiff", as(tif) width(800) height(550) replace

qui xtreg fcs i.legume_intens#c.hh_asset_index $intlist , fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.70(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Asset index (PCA)", margin(0 0 0 2)) ytitle("") name(fig1, replace)

qui xtreg fcs i.legume_intens#c.rainfall_diff $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(rainfall_diff=(-285(35)305)) post 
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Rainfall deviation from historic average (mm)", margin(0 0 0 2)) xlabel(-250(75)300)  ytitle("") name(fig2, replace)
* no evidence of effect heterogeneity

qui xtreg fcs i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)15)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2) ) name(fig3, replace)

qui xtreg fcs i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 9)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2) ) name(fig4, replace)

graph combine fig1 fig2 fig3 fig4, col(2) ///
title("Average Marginal Effects of Legume Intensification") ///
note("Linear fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_fcs.tiff", as(tif) width(800) height(550) replace





**# All four rounds
use "data\est\dfmlwfull.dta", clear
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size plot_certificate maize_share other_crops flat_slope mod_slope  ln_dis_plot ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	extension_service no_credit_access fert_voucher bean_voucher eco_soil_mng organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall adopted_neighbors_10km iv_maize
global vars female_manager primary_education_manager age_manager farm_size ///
	maize_share flat_slope subsistence_ag soil_fertility_index twi average_rainfall
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 616 hhs

**## Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
* planting dates for maize usually june july, and for legumes end july and august
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu subsistence_ag ///
	iv_maize extension_service no_credit_access fert_voucher organic_fertilizer inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall 
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
qui egen adopted_neighbors_10km_mean=mean(adopted_neighbors_10km), by(hh_id_obs)
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops i.flat_slope i.mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.no_credit_access i.fert_voucher ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall 
global ivlist adopted_neighbors_10km 
xtset hh_id_obs year
xtlogit legume_intens i.female_manager $xlist $ivlist i.year, fe
estimates store fixed
xtlogit legume_intens i.female_manager $xlist $ivlist i.year, re
estimates store random
hausman fixed random
drop _est_fixed _est_random
* random effects is more appropriate
xtlogit legume_intens i.female_manager $xlist $ivlist i.year, fe vce(bootstrap , rep(50))
outreg2 using "outputs\results\determinants_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects logit") addnote(Bootstrapped (for FE and pooled) and clustered standard errors (for CRE probit) in paranthesis; Hausman test indicates that fixed-effects is appropriate)
xtlogit legume_intens i.female_manager $xlist $ivlist mean_* i.year adopted_neighbors_10km_mean i.region, re vce(cluster hh_id_obs)
outreg2 using "outputs\results\determinants_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("CRE logit")
logit legume_intens i.female_manager $xlist $ivlist i.year i.region, vce(bootstrap , rep(50))
outreg2 using "outputs\results\determinants_mlw", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Pooled logit")

**## Main estimation - 2SLS: HDDS, total consumption, food consumption
** Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
qui probit legume_intens i.female_manager $xlist $ivlist mean_* i.year adopted_neighbors_10km_mean i.region, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS i.female_manager $xlist i.year (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi full HDDS")
xtivreg fcs i.female_manager $xlist i.year (legume_intens=ghat) , fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Malawi full FCS")


**## Interaction model - to understand the mediator role of manager's gender
**### HDDS
global intlist i.legume_intens i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share i.legume_intens c.other_crops i.flat_slope i.mod_slope ln_dis_plot ///
	hh_asset_index ln_other_income tlu i.subsistence_ag ///
	i.extension_service i.no_credit_access i.fert_voucher ///
	i.iv_maize i.organic_fertilizer i.inorganic_fertilizer ///
	soil_fertility_index twi rainfall_diff average_rainfall
	
xtpoisson HDDS i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female") title("HDDS")  name(figgender, replace)
* the effect is statistically signifficant for female managers
qui xtpoisson HDDS i.legume_intens#i.inorganic_fertilizer $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(inorganic_fertilizer=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "No fertilizer" 1 "Fertilizer") title("HDDS")  name(figfert, replace)

qui xtpoisson HDDS i.legume_intens#c.hh_asset_index $intlist , fe vce(robust)
eststo asset: margins, dydx(legume_intens) at(hh_asset_index =(-0.70(0.5)5)) post
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Asset index (PCA)", margin(0 0 0 2)) ytitle("") name(fig1, replace)

qui xtpoisson HDDS i.legume_intens#c.rainfall_diff $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(rainfall_diff=(-285(35)305)) post 
marginsplot, title("")  plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Rainfall deviation from historic average (mm)", margin(0 0 0 2)) xlabel(-250(75)300)  ytitle("") name(fig2, replace)

qui xtpoisson HDDS i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1)15)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2) ) name(fig3, replace)

qui xtpoisson HDDS i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1) 9)) post 
marginsplot, title("") plotopts(msize(medium)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick) ) xtitle("Number of other crops produced", size(small) margin(0 0 0 2) ) name(fig4, replace)

graph combine figgender figfert fig1 fig2 fig3 fig4, col(3) ///
title("Average Marginal Effects of Legume Intensification") ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\mlw_mes_full.tiff", as(tif) width(800) height(550) replace










