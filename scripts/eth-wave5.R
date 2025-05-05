# Wave 5 ####
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)
# Note: Tigray (54 EAs), Benshangul-Gumuz (28 EAs), and Afar (8 EAs) are ot present in this round. 
# To explore inercropping refer to eth-intercropping.qmd file in the outputs. 

### ### ### ### ### ### ### ###
# Maize - legume rotations----
### ### ### ### ### ### ### ###
## There are few fields reported more than 1 with different holders.
## These are probably divided between hh members.
## Let us first identify these fields
## Identify groups where s3q34 has different values
field_with_diff_s3q34 <- read_dta("data/eth/ess5/sect3_pp_w5.dta") |> 
  filter(s3q03==1) |> 
  group_by(household_id, parcel_id, field_id) |>  # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) |>  # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) |>  # Keep only groups where s3q34 has more than one unique value
  ungroup()
field_with_diff_s3q34 |> count(unique_s3q34) # 8 observations 


# At this moment this data is to identify if rotation was done in the field.
# just check the duplicates for rotation. Drop duplicates with no informative information. 
field_with_diff_s3q34 |> 
  select(household_id, parcel_id, field_id, s3q34 ) |> 
  head(8)

# # A tibble: 8 × 4
# household_id       parcel_id field_id s3q34     
# <chr>                  <dbl>    <dbl> <dbl+lbl> 
# 1 060103088806701051         3        1 2 [2. NO] 
# 2 060103088806701051         3        1 1 [1. YES]
# 3 060107088800402060         1        6 1 [1. YES]
# 4 060107088800402060         2        1 1 [1. YES]
# 5 060107088800402060         3        1 1 [1. YES]
# 6 060107088800402060         1        6 2 [2. NO] 
# 7 060107088800402060         2        1 2 [2. NO] 
# 8 060107088800402060         3        1 2 [2. NO]

field_with_diff_s3q34 <- field_with_diff_s3q34 |> 
  filter(s3q34==2) |>
  select(household_id, parcel_id, field_id, s3q34) |>
  distinct()
head(field_with_diff_s3q34)

# drop observation above from the field data  
field <- read_dta("data/eth/ess5/sect3_pp_w5.dta") |> #field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q13, s3q34) |> 
  filter(s3q03==1) |> 
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) |> 
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
maizedata <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |>  #crop roster
  select(household_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) |> 
  filter(s4q01b==2)
maizedata |> 
  group_by(household_id, parcel_id, field_id) |>
  filter(n() > 1) |>
  ungroup() # 10 observations; maize is mixed with maize for 5 hhs.
maizedata <- maizedata |> 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates
maizehh <- maizedata |> 
  distinct(household_id) # HHs that produce maize: 1004
field <- field |> 
  semi_join(maizehh, by=join_by(household_id))
cropdata <- left_join(maizedata, field, by=c("household_id", "parcel_id", "field_id")) # maize fields with field data 
cropdata |>
  group_by(household_id, parcel_id, field_id) |>
  filter(n() > 1) |>
  ungroup() # 0 duplicates 
look_for(cropdata)
glimpse(cropdata)
cropdata5 <- cropdata |> 
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
table(cropdata5$rotation)
# 0    1 
# 1305  633 maize fields had legume rotation
# Identify rotation at household level
# If household rotated at least one maize field with legumes during the past three years.
rotation5 <- collap(cropdata5, rotation~household_id, FUN=fmax)
table(rotation5$rotation)  # 392 adopters 612 non-adopters.
remove(maizedata, cropdata)
comment(rotation5) <- "hh level rotation"
var_label(rotation5$rotation) <- "If household rotated at least one maize field with legumes during the past 3y"

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |> 
  filter(country=="Ethiopia", wave==5) 
look_for(hpdata) # many missing
hpdata <- hpdata |> 
  semi_join(maizehh, by=c("hh_id_merge"="household_id"))
table(hpdata$main_crop, hpdata$intercropped) # 1046  429 intercropped maize 
table(hpdata$improved) # 992 fields with improved seeds
hpdata <- hpdata |>
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop, farm_size,
         manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, primary_education_manager, respondent_id_obs, 
         respondent_id_merge, improved,organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, 
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop,  
         used_pesticides, yield_value_LCU, yield_value_USD) |> 
  mutate(plot_id=plot_id_merge)
cropdata5 <- cropdata5 |> 
  rename(parcel_id2=parcel_id) |> 
  mutate(plot_id = paste(holder_id, parcel_id2, field_id, sep = "-")) # field means plot in HP
plotdata5 <- left_join(cropdata5, hpdata, by=c("plot_id")) |> 
  select(-s3q03, -s3q34, -s4q02)
remove(hpdata)
plotdata5 <- plotdata5 |> 
  group_by(household_id) |>
  mutate(count_plot=n()) |>
  ungroup()
table(plotdata5$count_plot)
# 1   2   3   4   5   6   7   8  10  11 
# 504 518 426 200 130  72  35  32  10  11 
plotdata5 <- plotdata5 |> select(-s4q01b)
table(plotdata5$main_crop)
plotdata5 <- plotdata5 |> mutate(main_legume=if_else(main_crop=="BEANS AND OTHER LEGUMES", 1,0)) 
var_label(plotdata5$main_legume) <- "legume is main crop on this plot"
var_label(plotdata5$count_plot) <- "number of maize plots in HH"
look_for(plotdata5) # missing come from harmonized panel.
remove(cropdata5)
comment(plotdata5) <- "maize plots and characteristics" 


### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
individual <- read_dta("data/eth/ess5/sect1_hh_w5.dta") |>  
  mutate(female=ifelse(s1q02==2,1,0)) |>
  select(household_id, individual_id, female) |>
  rename(s3q13=individual_id)
field5 <- left_join(field, individual, by=c("household_id", "s3q13"))
remove(field)
var_label(field5$female) <- "female primary dm concern. activities on plot" # also on crops to be planted
field5 <- field5 |>select(household_id, parcel_id, field_id, female)
plotdata5 <- plotdata5 |> 
  rename(parcel_id=parcel_id2) 
plotdata5 <- left_join(plotdata5,field5, by=c("household_id", "parcel_id", "field_id"))
look_for(plotdata5)
remove(field5)
table(plotdata5$female_manager, plotdata5$female) # comparing manager and decision-maker. !there are missing values in manager!
# 0    1
# 0 1550    0
# 1    0  328 # Perfect match, managers are decision-makers. 
plotdata5 <- plotdata5 |> select(-female_manager)

## Agri asset ownership ----
agriasset <- read_dta("data/eth/ess5/sect11_hh_w5.dta") |> 
  select(household_id, asset_cd,s11q00, s11q02_1, s11q02_2) |>
  filter(asset_cd %in% c(15, 16, 30, 31, 32, 33, 34)) |># agricultural assets
  filter(s11q00==1)  # keep assets owned
agriasset <- agriasset |> 
  semi_join(maizehh, by= join_by("household_id")) # keep maize producers. 
agriasset <- left_join(agriasset, individual, by=c("household_id" = "household_id",
                                                   "s11q02_1"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female2=female),
                       by=c("household_id" = "household_id", "s11q02_2"= "s3q13"))
agriasset <- agriasset |>
  mutate(womens_asset=case_when(female==1~1,
                                female2==1~1,
                                .default = 0))
agriasset <- agriasset |>
  mutate(dm_agriasset=
           case_when(womens_asset==1 & female==1 & is.na(female2) ~ 2,
                     womens_asset==1 & female==1 & female2==1 ~ 2, 
                     womens_asset==1 & female==0 & female2==1  ~ 1,
                     womens_asset==1 & female==1 & female2==0  ~ 1,
                     .default =0))
agriasset <- agriasset |>
  group_by(household_id)|>
  mutate(dupl=n())|>
  ungroup()
table(agriasset$dupl)

# 1   2   3   4   5 
# 297 602 738 140  10 

agriasset <- agriasset |>
  select(household_id, dm_agriasset, dupl)
# Function to calculate the most frequent value
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
agriasset <- agriasset |>
  select(household_id, dm_agriasset, dupl) |>
  group_by(household_id) |>
  mutate(agri_asset=case_when(
    dupl==2 ~ max(dm_agriasset, na.rm = TRUE), # two assets: if women had any say
    dupl>2 ~ mode_function(dm_agriasset),     # many assets: more frequent dm pattern 
    dupl==1 ~ dm_agriasset,
    .default = NA_real_)) |>
  ungroup()
agriasset$dm_agriasset_order <- ordered(agriasset$agri_asset, 
                                        levels = c(0, 1, 2), 
                                        labels = c("male", "joint", "female"))
agriasset %>%
  group_by(household_id) %>%
  filter(n() == 2) %>%  # Keep only household_ids that appear exactly twice
  summarise(n_unique = n_distinct(dm_agriasset_order)) %>%
  filter(n_unique > 1)  # Keep only those with different values
# no hh with two type assets but differetn decision-maker.
agriasset <- collap(agriasset, dm_agriasset_order ~ household_id, catFUN = fmode)
look_for(plotdata5)
remove(individual)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption; LATER consider alternative: Nominal annual expenditure on food per adult equivalent
# consumption quantile
hhhp5 <- read_dta("data/hp/Household_dataset.dta") |> 
  filter(country=="Ethiopia", wave==5) |> 
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio,totcons_USD, cons_quint, hh_asset_index, 
         HDDS, share_kg_sold)
hhhp5 <- hhhp5|> 
  semi_join(maizehh, by=c("hh_id_merge"="household_id"))
look_for(hhhp5)
# 104 maize producing households are not in the harmonized panel

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###

## Collapse / aggregate ----
plotdata5 <- plotdata5 |> 
  select(-s3q13) |> 
  rename(share_intercrop=s4q03)
plotdata5 |> fgroup_by(rotation) |> fselect(intercropped, main_legume) |>  fsum()
look_for(plotdata5)
wave5 <- plotdata5 |> 
  select(1,10, 15:44)
look_for(wave5)  
wave5mean <- collap(wave5,
                    farm_size + age_manager + dist_market + plot_dist_household + ag_asset_index + 
                      count_plot + soil_fertility_index  ~ household_id,
                    FUN = fmean,
                    na.rm = TRUE)
wave5sum <- collap(wave5,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ household_id,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "dist_market", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
wave5 <- wave5 %>% select(-all_of(aggregated_vars))
look_for(wave5)
wave5 <- wave5 |> 
  select(1, 2, 4:7, 11:21)
look_for(wave5)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
# # fmax() fsum(), fmean(), etc. expects pure numeric vectors. 
# If we feed it a labelled or factor column (even if it looks numeric), we may get nonsense max value, 
# only one value being returned or etc. Thus:  
wave5 <- wave5 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, intercropped, plot_certificate, irrigated),
                as.numeric))
wave5max <- collap(wave5,
                   ~ household_id, 
                   FUN = fmax,
                   na.rm = TRUE)
wave5 <- left_join(wave5mean, wave5sum, by=join_by("household_id"))
wave5 <- left_join(wave5, wave5max, by=join_by("household_id"))
wave5 <- left_join(wave5, agriasset, by=join_by("household_id"))
wave5 <- left_join(wave5, hhhp5, by=c("household_id"="hh_id_merge"))
wave5 <- left_join(wave5, rotation5, by=join_by("household_id"))
var_label(wave5$dm_agriasset_order) <- "control over agricultural assets"
remove(wave5max, wave5sum,wave5mean,rotation5, agriasset, aggregated_vars, hhhp5)

## Cleaning missing data ----
look_for(wave5)
# Many missing values!!!
# In page 22 of the Basic Information Document, Ethiopia Socioeconomic Survey 2021/22,
# it is mentioned that 11 households left their locations after the PP visit and
# 29 were in EAs that were not accessible during the second visit for a variety
# of reasons. For these 40 households only PP and livestock data are available.
# In addition, 248 rural households were not accessible during the PP visit
# but were interviewed during the PH and HH visit. The agricultural information for these
# households does not include all the details available in the PP module.  
hhdata <- read_dta("data/eth/ess5/sect1_hh_w5.dta") |>  
  distinct(household_id,.keep_all = TRUE) |> 
  filter(saq14==1)
ppdata <- read_dta("data/eth/ess5/sect1_pp_w5.dta") |> 
  distinct(household_id,.keep_all = TRUE)
# household size differs by 206 HHs, but HH  and PP may have some different additional HHs.
# let's see if these are the ones for which we have missing data in wave5
hhdata <- anti_join(hhdata, ppdata, by=join_by("household_id")) #246 HHs
hhdata <- hhdata |> 
  select(household_id) |> mutate(missinghh=1)
wave5 <- left_join(wave5, hhdata, by=join_by("household_id"))
wave5 <- wave5 |> 
  mutate(missinghh=ifelse(is.na(missinghh), 0, missinghh))
table(wave5$missinghh) # none are present in our data. 
wave5 <- wave5 |> select((-missinghh))
# now let's check the 40 HHs for which only pp is available. 
hhdata <- read_dta("data/eth/ess5/sect1_hh_w5.dta") |>  
  distinct(household_id,.keep_all = TRUE) |> 
  filter(saq14==1)
ppdata <- anti_join(ppdata, hhdata, by=join_by("household_id")) # 40 observations
ppdata <- ppdata |> 
  select(household_id) |> mutate(missinghh=1)
wave5 <- left_join(wave5, ppdata, by=join_by("household_id"))
wave5 <- wave5 |> 
  mutate(missinghh=ifelse(is.na(missinghh), 0, missinghh))
table(wave5$missinghh) # 14 are present in our data. 
wave5 <- wave5 |> 
  filter(missinghh!=1) |> select(-missinghh)
look_for(wave5)
remove(hhdata, ppdata)
# I drop households which do not have any agricultural asset, because  the control over agricultural asset variable
# is highly important for our analyses. 
missing <- wave5 |> 
  filter(is.na(dm_agriasset_order)) |>   
  select(household_id) #109 hhs.
agriasset <- read_dta("data/eth/ess4/sect11_hh_w4.dta") |> filter(saq14==1) |> 
  filter(s11q00==1) |> 
  select(household_id, asset_cd,s11q00) |>
  filter(asset_cd %in% c(15, 16, 30, 31, 32, 33, 34)) |> 
  distinct(household_id,.keep_all = TRUE)
agriassetnotmissing <- semi_join(agriasset, missing, by=join_by("household_id"))
table(agriassetnotmissing$s11q00) # 69 of the 109 hhs do have some agri asset. 
agriassetnotmissing <- agriassetnotmissing |> 
  mutate(notmissing=1) |> 
  select(household_id, notmissing)
remove(agriasset)
wave5 <- left_join(wave5, agriassetnotmissing, by=join_by("household_id"))
wave5 <- wave5 |> 
  mutate(drop=case_when(is.na(notmissing) &is.na(dm_agriasset_order) ~ 1,
                        .default = 0))
wave5 <- wave5 |> filter(drop!=1)
wave5 <- wave5 |> 
  filter(!is.na(dm_agriasset_order))
wave5 <- wave5 |> select(-notmissing, -drop)
look_for(wave5)
remove(missing, agriassetnotmissing, maizehh, plotdata5,mode_function)
look_for(wave5)
wave5 <- wave5 |> mutate(wave="wave5")
wave5<- wave5 |> 
  relocate(hh_id_obs, .after = household_id)
##  age is missing for many observations in the raw data. 29 missing for manager age.
missing <- is.na(wave5$age_manager)
wave5 <- subset(wave5, subset= !missing)













