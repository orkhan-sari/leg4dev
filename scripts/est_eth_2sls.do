**# Estimations Ethiopia
**## Drop missing
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"
use "data\est\dfeth.dta", clear

**## ranksum test
ranksum HDDS if female_dm !=1, by(female_dm ) porder
ranksum HDDS if female_dm !=2, by(female_dm ) porder
ranksum HDDS if female_dm !=3, by(female_dm ) porder

**## Estimations
mdesc female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size count_plot maize_share other_crops flat_slope plot_dist_household ///
	ln_other_income tlu subsistence_ag psnp_labour psnp_assistance ///
	extension_program advisory_service credit_unavailable eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km dist_market
global vars female_manager primary_education_manager age_manager plot_dist_household rainfall_diff
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
xtreg legume_intens $xlist $ivlist, fe vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_eth", excel replace  bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Fixed effects LPM") addnote(Bootstrapped standard errors in paranthesis; Hausman test that indicates random-effects is appropriate)
xtlogit legume_intens $xlist $ivlist, re vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_eth", excel append  bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Random effects Logit") addnote(Bootstrapped (for RE Logit and pooled Probit) and clustered standard errors (for CRE Probit) in paranthesis; Hausman test that indicates random-effects is appropriate)
logit legume_intens $xlist $ivlist, vce(bootstrap , rep(500))
outreg2 using "outputs\results\determinants_eth", excel append  bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Pooled Logit")

**## Main estimation - xtivreg: HDDS, FCS using IV Wooldridge 2010 procedure 21.1 
** Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope plot_dist_household dist_market ///
	hh_asset_index ln_other_income tlu subsistence_ag psnp_labour psnp_assistance ///
	extension_program advisory_service credit_unavailable eco_soil_mng ///
	iv_maize organic_fertilizer inorganic_fertilizer ///
	soil_fertility_index twi anntot_avg adopted_neighbors_10km  
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
probit legume_intens $xlist $ivlist mean_*, vce(cluster hh_id_obs)
predict ghat
xtivreg HDDS $xlist (legume_intens=ghat), fe vce(cluster hh_id_obs)
outreg2 using "outputs\results\xtivreg_results", excel append bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Ethiopia HDDS")
* NO EFFECT OF LEGUME DIVERSIFICATION 

**## Interaction models - to understand the moderator role of manager's gender
global intlist i.legume_intens i.female_manager i.primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope ln_dis_plot ///
	ln_other_income tlu i.subsistence_ag ///
	i.extension_program i.advisory_service i.credit_unavailable i.eco_soil_mng ///
	i.organic_fertilizer i.inorganic_fertilizer  ///
	soil_fertility_index twi rainfall_diff average_rainfall dist_market i.improved_maize
	
qui xtpoisson HDDS i.legume_intens#i.female_manager $intlist , fe vce(robust)
eststo female: margins, dydx(legume_intens) at(female_manager=(0 1)) post 
marginsplot, title("") horizontal plotopts(msize(large)) recast(scatter) ci(lwidth(vthick) lpattern(dash) lcolor(ebblue)) xline(0, lwidth(thick)) xtitle("Effects on linear prediction", margin(0 0 0 2)) ytitle("") ylabel( 0 "Male" 1 "Female")  name(fig1, replace)

qui xtpoisson HDDS i.legume_intens#c.rainfall_diff $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(rainfall_diff=(-285(35)370)) post 
marginsplot, title("")  plotopts(msize(large)) recast(scatter) ci(lwidth(vthick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Rainfall deviation from historic average (mm)", margin(0 0 0 2)) xlabel(-275(75)275)  ytitle("") name(fig2, replace)

qui xtpoisson HDDS i.legume_intens#c.ln_other_income $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(ln_other_income=(0(1) 12)) post 
marginsplot, title("") plotopts(msize(large)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Income from off-farm work (natural logarithm)", size(small) margin(0 0 0 2) ) name(fig3, replace)

qui xtpoisson HDDS i.legume_intens#c.other_crops $intlist , fe vce(robust)
eststo market: margins, dydx(legume_intens) at(other_crops=(0(1)18)) post 
marginsplot, title("") plotopts(msize(large)) recast(scatter) ci(lwidth(thick) lpattern(dash) lcolor(ebblue)) yline(0, lwidth(thick)) xtitle("Number of other crops produced", size(small) margin(0 0 0 2) ) name(fig4, replace)

graph combine fig1 fig2 fig3 fig4, col(2) ///
title("Average Marginal Effects of Legume Intensification") ///
note("Poisson fixed-effects model; dash lines indicate 95% CI", size(vsmall) position(12) justification(center))
graph export "C:\Users\490A\Nextcloud\Research\leg4dev\outputs\figures\eth_mes.tiff", as(tif) width(800) height(550) replace



**# Weak identification test
tab year, gen(year)
xtivreg2 HDDS primary_education_manager age_manager hh_size hh_dependency_ratio ///
	farm_size maize_share other_crops flat_slope ln_dis_plot ///
	ln_wage_income b_enterprise_income tlu subsistence_ag ///
	extension_program advisory_service credit_unavailable eco_soil_mng ///
	soil_fertility_index twi average_rainfall dist_market year2 (legume_intens=ghat) , fe
	
**# Control function approach etregress - convergence not achieved 
etregress HDDS i.female_manager $xlist mean_* i.year i.region, ///
	treat(legume_intens= i.female_manager $xlist $ivlist mean_* adopted_neighbors_10km_mean i.year i.region) poutcomes cfunction


