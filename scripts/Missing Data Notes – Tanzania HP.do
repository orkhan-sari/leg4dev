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
  
  
  - ORKHAN:
	Okay. Let us try ag_sec_3a.dta and see how many remain missing when merging 
	the datasets. Please add the varaible label at the end mentioning that this
	is based on self reported data. 
	
  - DRISYA:
	I’ve used ag_sec_3a.dta, filtered to keep only the relevant plots after 
	merging, and added the variable label: "Distance from plot to home (in KMs),
	self-reported" to indicate the source. There are no missing values for 
	plot_hh_distance in the merged dataset 
	— I think this variable works well for our purpose.
	
*/



codebook rain_shock
codebook flood_shock
/*
	In Tanzania (TZA), rain and flood shocks cannot be distinguished
	(see page 28 of the HP Data Description document — World Bank)
	because the agricultural questionnaire (page 19, Q18, TZA_2014_NPS-R4_v03_M)
	asks: "What was the cause of these losses?" but does not explicitly mention 
	rain or flood shocks.
	The options given are: birds, animals, insects, diseases, theft, other.
	
	
	- ORKHAN:
	All missing in HP anyways, right?. Does trhis mean farmers never reported rain 
	or flood wheen you look at the raw data? 
	
*/
use "C:\Users\ASUS\AppData\Local\Temp\4f3c7f6a-52b1-4e26-ac7e-d0fdc7e1bc36_TZA_2014_NPS-R4_v03_M_STATA11.zip.c36\TZA_2014_NPS-R4_v03_M_STATA11\ag_sec_4a.dta" 
codebook ag4a_18	

/*
	- DRISYA:
	No — rain or flood shocks are not reported in the raw data either. 
	Since the questionnaire didn’t list them as explicit options, farmers 
	couldn’t select them directly. A few farmers chose “other,” but there’s no 
	further detail provided to clarify if those refer to rain or flood. 
	
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
	
	
	- ORKHAN: 
	Okay, then we must leave this variable out from the analyses. 
	Could you find any other varaible from raw data that could capture
	the soil quality?

*/
use "C:\Users\ASUS\AppData\Local\Temp\f40b4da7-7440-4eac-aec8-bfae4deea222_TZA_2014_NPS-R4_v03_M_STATA11.zip.222\TZA_2014_NPS-R4_v03_M_STATA11\ag_sec_3a.dta"
codebook ag3a_10 ag3a_11 ag3a_12

/*
	
	- DRISYA:
	I identified the following soil-related questions in the raw data that could
	serve as alternative indicators of soil quality. These items are sourced 
	from the Agriculture Questionnaire – Section 3a: Plot Details – Long Rainy 
	Season (page 7), with the corresponding variables located in the dataset 
	ag_sec_3a under ag3a_10, ag3a_11, and ag3a_12:

	ag3a_10 – What was the soil type of this plot?

		1 = Sandy (614 plots)

		2 = Loam (2,511 plots)

		3 = Clay (733 plots)

		4 = Other, specify (134 plots)

		Missing: 1,545 out of 5,537 observations

	ag3a_11 – What was the soil quality of this plot?

		1 = Good (1,694 plots)

		2 = Average (2,004 plots)

		3 = Bad (294 plots)

		Missing: 1,545 out of 5,537 observations

	ag3a_12 – How do you know the quality of the soil?

		1 = Scientifically tested (10 plots)

		2 = Own experience (3,323 plots)

		3 = Other, specify (659 plots)

		Missing: 1,545 out of 5,537 observations
*/

/*
	- DRISYA:

	Out of the identified soil-related variables, I used `ag3a_10` (soil type) from `ag_sec_3a`  
	and matched it with maize plots (from `hpdata`) using `plot_id`. This serves as a proxy  
	for soil quality in the absence of geovariables used for the PCA-based soil_fertility_index.
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
	
	- ORKHAN:
	yes, this is the same also for other coutnries. Please see line 326 in eth4 
	as an example. I create a new varaible with captures if the hh does not 
	intend to/consider seel any crop - susbsitnce orientation. Also look at 
	mlw3 line 190.
	
	- DRISYA:
	I’ve taken a similar approach of Malawi's for Tanzania’s subsistence orientation.
	
	Maize-producing households were identified using maizehh and matched with 
	sale data (ag5a_01) from the agricultural questionnaire (p.22) and the 
	raw data file ag_sec_5a. Households responding “no” (2) to selling were 
	classified as subsistence-oriented, resulting in 1,323 households who 
	grew but did not sell maize in the 2014 long rainy season.
	
*/
