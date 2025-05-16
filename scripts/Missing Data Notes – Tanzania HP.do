******Exploring Missing Data*****


*---------------------------Plot dataset - HP----------------------------------*

use "D:\WORK\LEG4DEV\leg4dev\data\hp\Plot_dataset.dta"
keep if country == "Tanzania" & wave == 4


codebook plot_dist_household
codebook geocoords_id
/*  
  TZA Wave 4 has no geocoords_id data, which is required to derive the 
  plot_dist_household variable.(see page 25 & 38 of the HP Data Description 
  document — World Bank)
  
  Alternatively, if needed, to obtain the plot_dist_household variable, we could
  use ag3a_02_01("Distance from plot to home [in KMs]") from the 'ag_sec_3a' 
  section of the "TZA_2014_NPS-R4_v03_M" dataset (Wave 4). This variable has 
  1262 missing values and 1552 zero values, out of a total of 5537 observations.  
*/


codebook rain_shock
codebook flood_shock
/*
	In Tanzania (TZA), rain and flood shocks cannot be distinguished
	(see page 30 of the HP Data Description document — World Bank)
	because the agricultural questionnaire (page 19, Q18, TZA_2014_NPS-R4_v03_M)
	asks: "What was the cause of these losses?" but does not explicitly mention 
	rain or flood shocks.
	The options given are: birds, animals, insects, diseases, theft, other.
*/


codebook soil_fertility_index
codebook nutrient_availability nutrient_retention rooting_conditions 
	oxygen_availability excess_salts toxicity workability

/*
	The variable soil_fertility_index is derived from a PCA of the soil 
	variables listed above.
	However, data for all these component soil variables are missing in the 
	household panel dataset. This is because these soil variables originate 
	from the geovariables files, which are not available for Tanzania Waves 4 
	and 5(see page 38 of the HP Data Description document — World Bank).
	
	I couldn’t figure out how they created this variable in the .do file from 
	the reproduction.new folder. :(
*/
*------------------------------------------------------------------------------*


*---------------------------Household dataset - HP-----------------------------*

use "D:\WORK\LEG4DEV\leg4dev\data\hp\Household_dataset.dta" 
keep if country == "Tanzania" & wave == 4

codebook share_kg_sold
/*
	The variable share_kg_sold is set to missing if the reported share exceeds 
	100% or for households that did not harvest any crops.
	(See page 37 of the HP Data Description document — World Bank.)
	These conditions likely explain the presence of missing values.
*/
