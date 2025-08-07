**# Estimations Ethiopia
**## Drop missing
global folder = "C:\Users\490A\Nextcloud\Research\leg4dev"
cd $folder
pwd
set seed 12345
set scheme s1color
graph set window fontface "Palatino Linotype"
use "data\est\dfeth.dta", clear
gen region = .
replace region = 1 if inlist(saq01, 2, 3)         // AFAR, AMHARA
replace region = 2 if inlist(saq01, 4, 15, 13)    // OROMIA, DIRE DAWA, HARAR
replace region = 3 if inlist(saq01, 6, 12)        // BENISHANGUL GUMUZ, GAMBELA
replace region = 4 if saq01 == 7                  // SNNP
label define region 1 "North" 2 "Central" 3 "West" 4 "South"
label values region region
tab saq01 region
mdesc rotation female_manager primary_education_manager age_manager share_female_owned hh_size hh_dependency_ratio farm_size count_plot maize_share elevation slope ln_wage_income b_enterprise_income tlu subsistence_ag extension_program advisory_service credit_unavailable eco_soil_mng soil_fertility_index rainfall_variation adopted_neighbors_10km
global vars primary_education_manager maize_share 
egen missing_flag = rowmiss($vars)
drop if missing_flag > 0
egen n_obs = count(year), by(hh_id_obs)
egen max_obs = max(n_obs)
* assuming balanced panel should have max number of periods
keep if n_obs == max_obs
drop n_obs max_obs missing_flag // 866 hhs


**# Panel data switching regression with control function - Estimating the Average Treatment Effect Using IV Wooldridge 2010 procedure 21.1 
tabulate slope, gen(dslope)
rename dslope1 flat_slope
rename dslope2 mod_slope
local temp female_manager primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation slope ln_wage_income b_enterprise_income tlu subsistence_ag extension_program advisory_service credit_unavailable eco_soil_mng soil_fertility_index  rainfall_variation adopted_neighbors_10km
foreach var of varlist `temp' {   
qui egen mean_`var'=mean(`var'), by(hh_id_obs)
}
global xlist i.primary_education_manager age_manager hh_size hh_dependency_ratio farm_size count_plot maize_share elevation flat_slope mod_slope ln_wage_income b_enterprise_income tlu i.subsistence_ag  i.advisory_service i.credit_unavailable i.eco_soil_mng soil_fertility_index 
global ivlist rainfall_variation adopted_neighbors_10km
xtset hh_id_obs year
xtprobit legume_intens i.female_manager $xlist $ivlist mean_* i.year i.region, re vce(robust)
outreg2 using "outputs\results\legume_intensificationeth", excel replace sideway bdec(2) cdec(2) label symbol(***, **, *) alpha (.01, .05, .10) stats(coef se) cti("Legume diversification CRE model")
predict xb, xb

**## HDDS
xtivreg HDDS i.female_manager##i.legume_intens i.year $xlist (legume_intens=xb), fe vce(bootstrap, rep(100))
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