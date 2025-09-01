# eth5 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)
### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Ethiopia", wave==5) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge) # 968 households
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata |>
  mutate(yield_value_USD= yield_value_USD * (share_value_LEGUMES + share_value_MAIZE)) |> 
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop, farm_size, 
         manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, primary_education_manager,  
         agro_ecological_zone, twi, improved, organic_fertilizer, inorganic_fertilizer, soil_fertility_index, elevation, dist_market, 
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock, flood_shock, irrigated, nb_seasonal_crop,
         used_pesticides, yield_value_USD) |> 
  mutate(plot_id=plot_id_merge)
hpdata <- hpdata |>
  group_by(hh_id_obs) |> 
  mutate(crop_count=sum(nb_seasonal_crop)) |> 
  relocate(crop_count, .after = nb_seasonal_crop) |> 
  ungroup()
look_for(hpdata) #many missing

### ### ### ### ### ### ### ###
# Crop diversification ----
### ### ### ### ### ### ### ###

## Maize - legume rotations----
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
field <- read_dta("data/eth/ess5/sect3_pp_w5.dta") |>#field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q13, s3q34) |>
  filter(s3q03==1) |>
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) |>
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
field <- field |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
maizedata <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |>   #crop roster
  select(household_id, holder_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) |>
  filter(s4q01b==2) # keep only maize
maizedata <- maizedata |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-")) # field means plot in HP
maizedata <- semi_join(maizedata, maizeplots, by="plot_id")

# We have many more maize plots in HP than in row data.
#####################EXPLORE THE MISSING PLOTS
maizeplots1 <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  mutate(plot_id=plot_id_merge)
missing <- anti_join(maizeplots1, maizedata, , by="plot_id") #175 plots are in HP but not in row data. 
sec4 <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
sec4 <- semi_join(sec4, missing, by="plot_id") # only three (of 175) are actually in the row data, and maize is not produced.
# Maize crop in hp comes from harvest section. Thus, we observe some mismatch with harvest and crop roster 
# The reason is not clear, but we will continue with row data in this case.
postharvest <- read_dta("data/eth/ess5/sect9_ph_w5.dta") |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
postharvest <- semi_join(postharvest, missing, by="plot_id") # only 29 of 175 are in the post harvest. 
# CONC: MANY OF THE MISSING ARE ALSO MISSING IN OTHER VARIABLES. THERE MUST HAVE BEEN SOME ERROR IN HP. 
# WE CONTINUE WITH THE ROW DATA MATCHED HP PLOTS. 
remove(maizeplots1, missing,sec4, postharvest)
maizedata <- maizedata |> 
  group_by(household_id)|>
  mutate(count_plot=n())|>
  ungroup()
maizedata |>
  group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup() # 4 observations; maize is mixed with maize for two hhs.
maizedata <- maizedata |>
  distinct(plot_id, .keep_all = TRUE) # Remove duplicates to identify maize grown fields
cropdata <- left_join(maizedata, field, by=c("household_id", "holder_id", "parcel_id", "field_id", "plot_id")) # maize fields with field data 
cropdata|>
  group_by(household_id, parcel_id, field_id) |> 
  filter(n() > 1) |> 
  ungroup() # 1 duplicate 
dup <- cropdata|>
  group_by(household_id, parcel_id, field_id) |> 
  filter(n() > 1) |> 
  ungroup() 
remove(dup)
cropdata <- cropdata |> filter(!is.na(s3q03)) # remove duplicate and one more missing.
look_for(cropdata)
cropdata5 <- cropdata |>
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
comment(cropdata5) <- "main crop maize plots"
table(cropdata5$rotation)
#  0   1 
# 985 474 main maize fields had legume rotation
# Identify rotation at household level
rotation5 <- collap(cropdata5, rotation+count_plot~household_id, FUN=fmax)
table(rotation5$rotation)  # 321 adopters 505 non-adopters.
remove(maizedata, cropdata)
comment(rotation5) <- "hh level rotation"
var_label(rotation5$rotation) <- "If household rotated at least one maize field with legumes during the past 3y"
var_label(rotation5$count_plot) <- "number of main maize plots"
plotdata5 <- left_join(cropdata5, hpdata, by=c("plot_id")) |>
  select(-s3q03, -s3q34, -s4q02)

## Maize - legume intercropping----
## # RARE! Also refer to the qmd file \leg4dev\outputs\ethiopia.qmd
cropdata <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |> 
  select("household_id", "holder_id", "parcel_id", "field_id","crop_id", "s4q01b", "s4q02", "s4q03") |> 
  rename(mixedcrop=s4q02)
table(cropdata$mixedcrop) # 2692 plots with mixed cropping where 6085 crops are produced
cropdata <- cropdata |> 
  dplyr::group_by(household_id, parcel_id, field_id) |> 
  mutate(legum = ifelse(s4q01b %in% c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24), 1, 0),
         legume = ifelse(any(mixedcrop == 2) & any(legum == 1), 1, 0)) |> 
  ungroup()
cropdata <- cropdata |> 
  filter(legume==1) |> 
  filter(s4q01b==2) |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-")) |> 
  select(plot_id, legume) |> 
  rename(intercrop=legume)
plotdata5 <- left_join(plotdata5, cropdata, by="plot_id")
plotdata5$intercrop[is.na(plotdata5$intercrop)] <- 0
frq(plotdata5$intercrop) # only 105 main maize plots are intercropped with legume
rm(cropdata)


### ### ### ### ### ### ### ###
# Ecological soil management ----
### ### ### ### ### ### ### ###
soilman <- read_dta("data/eth/ess5/sect3_pp_w5.dta") |> 
  select(household_id, holder_id, parcel_id, field_id, s3q35, s3q36, s3q41) |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
soilman <- semi_join(soilman, maizeplots, by="plot_id")
frq(soilman$s3q36)
# 1 no till 
# 5: Cover with crop but not tilled for this season. I believe 5 then also can be assumed no till.
# 2 = once: this can mean one-pass till meaning minimum tillage. 
# We will capture zero and minimum tillage.
frq(soilman$s3q41)
soilman <- soilman |> 
  mutate(min_till=if_else(!is.na(s3q36) & s3q36 %in% c(1,2,5), 1, 0),
         zero_till=if_else(!is.na(s3q36) & s3q36 %in% c(1,5), 1, 0),
         crop_residue=if_else(!is.na(s3q41) & s3q41==2, 1, 0)
  )
frq(soilman$min_till) # 18% 
frq(soilman$zero_till) # 3%
frq(soilman$crop_residue) #1%
soilman <- soilman |> 
  select(plot_id, min_till, zero_till, crop_residue)
plotdata5 <- left_join(plotdata5, soilman, by="plot_id")
frq(plotdata5$crop_residue)
frq(plotdata5$zero_till)
frq(plotdata5$min_till)
remove(soilman)
var_label(plotdata5$min_till) <- "minimum tillage: one-pass till"
var_label(plotdata5$zero_till) <- "no till"
var_label(plotdata5$crop_residue) <- "Cover the field surface with crop residue/leaves"

## improved maize  ----
## here we don't have the info at parcel level but only at hh level. all seeds were asked. We concentrate on maize. 
seed <- read_dta("data/eth/ess5/sect5_pp_w5.dta") |> 
  select(household_id, holder_id, s5q0B, s5q01a, s5q01b ) |> 
  filter(s5q0B==2)
look_for(seed)
seed |> group_by(household_id) |> 
  filter(n()>1) |>
  ungroup() # improved varieties in some fields of HHs. 
# HHs have several maize fields,and it is not possible to identify 
# in which parcel the improved seed was cultivated, i.e., rotated parcel or not.** 
seed <- seed |>
  mutate(s5q01a=as.numeric(s5q01a),
         improved_maize=if_else(s5q01a==2,1,0))
seed <- collap(seed, improved_maize ~ household_id, FUN = fmax)
plotdata5 <- left_join(plotdata5, seed, by="household_id")
frq(plotdata5$improved_maize) # 4 missing
plotdata5$improved_maize[is.na(plotdata5$improved_maize)] <- 0
remove(seed)

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
plotdata5 <- left_join(plotdata5,field5, by=c("household_id", "parcel_id", "field_id"))
look_for(plotdata5)
remove(field5)
table(plotdata5$female_manager, plotdata5$female) # comparing manager and decision-maker. !there are missing values in manager!
# 0    1
# 0 1204    0
# 1    0  251 # Perfect match, managers are decision-makers. 
plotdata5 <- plotdata5 |> select(-female)

## Asset ownership ----
assets <- read_dta("data/eth/ess5/sect11_hh_w5.dta") |> 
  select(household_id, asset_cd, s11q00, s11q02_1, s11q02_2) |>
  filter(asset_cd %in% c(13:18, 21:24, 29:35)) |> # agricultural and other valuable assets like car, motor, refrigerator
  filter(s11q00==1)  # keep assets owned
assets <- assets |> 
  semi_join(maizehh, by= join_by("household_id"=="hh_id_merge")) # keep maize producers. 
assets <- left_join(assets, individual, by=c("household_id" = "household_id",
                                                   "s11q02_1"= "s3q13"))
assets <- left_join(assets, 
                       individual |>rename(female2=female),
                       by=c("household_id" = "household_id", "s11q02_2"= "s3q13"))
assets <- assets |>
  mutate(womens_asset=case_when(female==1~1,
                                female2==1~1,
                                .default = 0))
assets <- assets |>
  group_by(household_id) |> 
  summarise(total=n(),
            female_owned=sum(womens_asset, na.rm = TRUE),
            share_female_owned=female_owned/total) |> 
  select(household_id, share_female_owned) |> 
  ungroup()
ggplot(assets, aes(x = share_female_owned)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "red") +
  labs(
    title = "Distribution of Share of Female-Owned Assets",
    x = "Share of Female-Owned Assets",
    y = "Frequency"
  ) +
  theme_minimal()
look_for(plotdata5)
remove(individual, cropdata5)

## Education ----
individual <- read_dta("data/eth/ess5/sect1_hh_w5.dta") |> 
  select(household_id, individual_id, s1q01, s1q02, s1q03a) |>
  mutate(female=ifelse(s1q02==2,1,0)) |>
  filter(female==1) |> 
  filter(s1q03a>17)
edu <- read_dta("data/eth/ess5/sect2_hh_w5.dta") |> 
  select(household_id, individual_id, s2q03, s2q06) 
individual <- left_join(individual, edu, by=c("household_id", "individual_id"))
remove(edu)
frq(individual$s2q03)
individual <- individual |> 
  mutate(s2q03= if_else(is.na(s2q03), 2, s2q03))
# 50% have no education and some have very basic level; we will capture just if they can read and write
individual$s2q03 <- as.numeric(individual$s2q03)
individual <- collap(individual, s2q03 ~ household_id, FUN = fmin, na.rm = TRUE)
frq(individual$s2q03) # 58% can read and write
individual <- individual |> 
  mutate(s2q03=if_else(s2q03==2,0,1))

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption; LATER consider alternative: Nominal annual expenditure on food per adult equivalent
# consumption quantile
hhhp5 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Ethiopia", wave==5) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, totcons_USD, cons_quint, hh_asset_index,
         HDDS, share_kg_sold)
hhhp5 <- hhhp5|> 
  semi_join(maizehh, by=c("hh_id_merge"))
look_for(hhhp5) # 413 missing for share_kg_sold
# 104 maize producing households are not in the harmonized panel

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###

## Collapse / aggregate ----
plotdata5 <- plotdata5 |> 
  select(-s3q13,-s4q01b,-s4q03)
look_for(plotdata5)
wave5 <- plotdata5 |> 
  select(1, 10, 15:47)
look_for(wave5)  
wave5basics <- wave5 |> 
  select(household_id, hh_id_obs) |> 
  distinct()
wave5mean <- collap(wave5,
                    farm_size + age_manager + dist_market + elevation + plot_dist_household + ag_asset_index + 
                    soil_fertility_index + crop_count + twi ~ household_id,
                    FUN = fmean,
                    na.rm = TRUE)
wave5sum <- collap(wave5,
                   plot_area_GPS + nb_seasonal_crop + yield_value_USD  ~ household_id,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "dist_market", "elevation", "plot_dist_household", "soil_fertility_index", "crop_count",
                     "ag_asset_index", "plot_area_GPS", "nb_seasonal_crop", "twi", "yield_value_USD")
wave5 <- wave5 %>% select(-all_of(aggregated_vars))
look_for(wave5)
wave5 <- wave5 |> 
  select(1, 6:9, 11:23)
look_for(wave5)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
# # fmax() fsum(), fmean(), etc. expects pure numeric vectors. 
# If we feed it a labelled or factor column (even if it looks numeric), we may get nonsense max value, 
# only one value being returned or etc. Thus:  
wave5 <- wave5 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,agro_ecological_zone,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave5max <- collap(wave5,
                   ~ household_id, 
                   FUN = fmax,
                   na.rm = TRUE)
wave5 <- left_join(wave5basics, wave5mean, by=join_by("household_id"))
wave5 <- left_join(wave5, wave5sum, by=join_by("household_id"))
wave5 <- left_join(wave5, wave5max, by=join_by("household_id"))
wave5 <- left_join(wave5, assets, by=join_by("household_id"))
wave5 <- left_join(wave5, hhhp5, by=c("household_id"="hh_id_merge"))
wave5 <- left_join(wave5, rotation5, by=join_by("household_id"))
wave5 <- left_join(wave5, individual, by=join_by("household_id"))
wave5 <- wave5 |> 
  mutate(female_edu=if_else(is.na(s2q03), 0, s2q03)) |> 
  select(-s2q03)
var_label(wave5$share_female_owned) <- "women's control over assets"
var_label(wave5$female_edu) <- "HH has an adult female who can read and write"
var_label(wave5$improved_maize) <- "HH has cultivated imporved maize in at least one field"
var_label(wave5$intercrop) <- "HH has intercroped maize-legume"
remove(wave5basics, wave5max, wave5sum,wave5mean,rotation5, assets, aggregated_vars, hhhp5, individual)

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
remove(hhdata, ppdata, hpdata, plotdata5)
look_for(wave5)
wave5 <- wave5 |> mutate(wave="wave5")
missing <- is.na(wave5$age_manager) # 1 missing
wave5 <- subset(wave5, subset= !missing)

### share_kg_sold----
### has many missing (276); the reason is because of many have reported share above 100% percent. 
### Also, some did not harvest any crops.
### We will capture subsistence orientation, by looking at the question 22 in section 4 that asks: Do you intend to sell any 
### of [CROP] to be harvested during the rainy season?
sec4 <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |> 
  select(household_id,holder_id, parcel_id, field_id, s4q22) |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
val_labels(sec4$s4q22) # 1 yes 2 no
sec4 <- semi_join(sec4, maizeplots, by="plot_id")
sec4 <- sec4 |> 
  mutate(subsistence_ag=if_else(s4q22==2,1,0))
sec4 %>%
  group_by(household_id) %>%
  summarise(n_unique = n_distinct(subsistence_ag, na.rm = TRUE)) %>%
  filter(n_unique > 1) |>  # many cases where answer to the question differs. 
  ungroup()
sec4 <- collap(sec4, subsistence_ag~household_id, FUN = fmin) # if some consider for sale then not a subsistence. 
wave5 <- left_join(wave5, sec4, by="household_id")
table(wave5$subsistence_ag)
var_label(wave5$subsistence_ag) <- "HH does not intend to sell any crop"
remove(missing, sec4)
look_for(wave5)
rural <- read_dta("data/eth/ess5/sect_cover_hh_w5.dta") |> 
  filter(saq14==1) # keep rural hhs.
wave5 <- semi_join(wave5, rural, by="household_id") # indeed all are rural farmers. 
remove(rural)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###

## Non-farm income----
wage <- read_dta("data/eth/ess5/sect4_hh_w5.dta") |> 
  filter(saq14==1) |> 
  select(household_id, s4q33, s4q33b, s4q37, s4q38, s4q39, s4q40, s4q41, s4q33, s4q33b, s4q47, s4q50)
frq(wage$s4q41)
# some mismatch. some has worked 2 months or less,
# but reported that the money received is for half a year or quarter.
# 7 and 8 can be assumed as they are from the inspection of the data. error is possible. 
wage <- wage |> 
  mutate(income=case_when(s4q41==5 ~ s4q37 * s4q40,
                          s4q41==4 ~ s4q37/2 * s4q40,
                          s4q41==3 ~ s4q37 * s4q38 * s4q40,
                          s4q41==2 ~ s4q37 * s4q38 * s4q39/8 * s4q40,
                          .default = s4q40)) |>  # very rough calculation 
  mutate(income=as.numeric(income))
look_for(wage)
wage <- wage |> 
  tidyr::replace_na(list(s4q47 = 0)) |>
  tidyr::replace_na(list(s4q50 = 0)) |>
  tidyr::replace_na(list(income=0)) |>
  mutate(wage_income=income+s4q47+s4q50)
wage <- collap(wage,wage_income~household_id, FUN = fsum)
var_label(wage$wage_income) <- "income from main and temporary off-farm work"
enterprise <- read_dta("data/eth/ess5/sect12b1_hh_w5.dta") |> 
  filter(saq14==1) |> 
  select(household_id, s12bq12, s12bq16, s12bq17a, s12bq17b, s12bq17c, s12bq17d, s12bq17e, s12bq17f) 
enterprise <- enterprise |> 
  mutate(enterprise_income=s12bq12*s12bq16-s12bq12 * (s12bq17a+s12bq17b+ s12bq17c+ s12bq17d+ s12bq17e+ s12bq17f))
enterprise <- collap(enterprise,enterprise_income~household_id, FUN = fsum)
other_income <- read_dta("data/eth/ess5/sect13_hh_w5.dta") |> 
  filter(saq14==1) |> 
  select(household_id, source_cd, s13q01, s13q02) |> 
  filter(!is.na(s13q02))
table(other_income$source_cd, other_income$s13q01)
other_income <- collap(other_income,s13q02~household_id, FUN = fsum)
other_income <- other_income |> rename(other_income=s13q02)
wave5 <- left_join(wave5, wage, by="household_id")
table(is.na(wave5$wage_income)) # only 44 maize producers
wave5 <- wave5 |> 
  tidyr::replace_na(list(wage_income = 0))
wave5 <- left_join(wave5, other_income, by="household_id")
table(is.na(wave5$other_income)) # 247 maize producers
wave5 <- wave5 |> 
  tidyr::replace_na(list(other_income = 0))
wave5 <- left_join(wave5, enterprise, by="household_id")
table(is.na(wave5$enterprise_income)) # 156 maize producers
wave5 <- wave5 |> 
  tidyr::replace_na(list(enterprise_income = 0))
wave5 <- wave5 |> 
  mutate(tot_other_income=wage_income+other_income+enterprise_income)
var_label(wave5$tot_other_income) <- "Yearly total other income"
remove(wage,enterprise,other_income)

##  Extension, advisory, credit----
extension <- read_dta("data/eth/ess5/sect7_pp_w5.dta") |> 
  select("household_id", "saq01", "s7q04", "s7q06", "s7q08" ,"s7q09", "s7q10" )
look_for(extension)
extension <- extension |> 
  filter(!if_all(c("s7q04", "s7q06", "s7q08", "s7q09", "s7q10"), is.na))
frq(extension$s7q04)
frq(extension$s7q09) # the label should be do you get advisory services?
extension <- extension |>  
  mutate(extension_program= case_when(s7q04==1 ~ 1,
                                      .default = 0)) |> 
  mutate(advisory_service= case_when(s7q09==1 ~  1,
                                     .default = 0)) |> 
  mutate(credit_unavailable=case_when(s7q08==1 ~ 1,
                                      .default = 0)) |>  
  select(household_id,extension_program, advisory_service, credit_unavailable, saq01)
# The information is gathered at the holder level. For HH level data, we collapse and choose the max value. 
extension <- collap(extension, extension_program +advisory_service + credit_unavailable + saq01 ~household_id, FUN=fmax)
wave5 <- left_join(wave5,extension,by="household_id")
remove(extension)

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/eth/ess5/sect8_1_ls_w5.dta") |> 
  select("household_id", "ls_code", "ls_s8_1q01","ls_s8_1q02")
tlu <- tlu |> 
  filter(ls_s8_1q01>0) |> 
  mutate(owned=ls_s8_1q01-ls_s8_1q02) |> 
  select("household_id", "ls_code", "owned")
frq(tlu$ls_code)
tlu <- tlu |> 
  filter(ls_code!=16) |>  # bee colony not considered
  mutate(tlu=case_when(ls_code %in% c(1:6) ~ owned * 0.7,
                       ls_code %in% c(7,8) ~ owned * 0.1,
                       ls_code==9 ~ owned * 1,
                       ls_code %in% c(10,11,12) ~ owned * 0.01,
                       ls_code==13 ~ owned * 0.8,
                       ls_code %in% c(14,15) ~ owned * 0.70,
                       .default = 0))
tlu <- collap(tlu, tlu ~ household_id, FUN = fsum)
eth5 <- left_join(wave5,tlu,by="household_id")
remove(tlu, wave5)
eth5$tlu[is.na(eth5$tlu)] <- 0

## Soil type ---- 
soil <- read_dta("data/eth/ess5/sect2_pp_w5.dta") |> 
  mutate(parcel_id = paste(holder_id, parcel_id, sep = "-")) |> 
  select("household_id", "parcel_id", "s2q16")
soil <- semi_join(soil, maizeplots, by=c("parcel_id"="parcel_id_merge"))
look_for(soil)
frq(soil$s2q16)
soil <- soil |> 
  mutate(s2q16 = as.numeric(soil$s2q16),
         soil_type=case_when(s2q16==4 ~ 2,
                             s2q16==3 ~ 1,
                             .default = 0))
mode_or_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  tab <- table(x)
  most_common <- which(tab == max(tab))
  
  if (length(most_common) == 1) {
    return(as.numeric(names(most_common)))
  } else {
    return(max(as.numeric(names(most_common))))
  }
} 
soil <- collap(soil, soil_type ~ household_id, FUN =mode_or_max) # select dominant otherwise Luvisol. 
soil <- soil |> 
  mutate(soil= factor(soil_type, levels = c(0,1,2), labels = c("Other", "Vertisol", "Luvisol"))) |> 
  select(household_id, soil)
eth5 <- left_join(eth5, soil, by="household_id")
var_label(eth5$soil) <- "HH dominant soil type"
frq(eth5$soil)
remove(soil)

## Consumption ----
cons <- read_dta("data/eth/ess5/cons_agg_w5.dta") |> 
  select(household_id, nom_foodcons_aeq, nom_totcons_aeq)
look_for(cons)
eth5 <- left_join(eth5, cons, by="household_id")
rm(cons)

## Head gender ----
head <- read_dta("data/eth/ess5/sect1_hh_w5.dta") |>
  select(household_id, s1q01, s1q02) |> 
  filter(s1q01==1)
frq(head$s1q02)
head <- head |> 
  mutate(female_head=if_else(s1q02==2,1,0)) |> 
  select(household_id, female_head)
eth5 <- left_join(eth5, head, by="household_id")
rm(head)

## Slope 
slope <- read_dta("data/eth/ess5/sect3_pp_w5.dta") |> 
  mutate(parcel_id = paste(holder_id, parcel_id, sep = "-")) |> 
  select("household_id", "parcel_id", "s3q12")
slope <- semi_join(slope, maizeplots, by=c("parcel_id"="parcel_id_merge"))
look_for(slope)
frq(slope$s3q12)
slope <- slope |> 
  mutate(flat_slope=case_when(s3q12==1 ~ 1,
                              .default=0)
  )
slope <- collap(slope, flat_slope ~ household_id, FUN =mode_or_max)
frq(slope$flat_slope)
eth5 <- left_join(eth5, slope, by="household_id")
var_label(eth5$flat_slope) <- "=1 if slope is flat"
remove(slope, maizeplots, mode_or_max)

## Other crops produced 
crops <- read_dta("data/eth/ess5/sect4_pp_w5.dta") |>   #crop roster
  select(household_id, s4q01b)
crops <- semi_join(crops, maizehh, by= join_by("household_id"=="hh_id_merge"))
frq(crops$s4q01b)
crops <- crops |> 
  rowwise() |> 
  mutate(
    other_crops_count = sum(
      !(s4q01b %in% c(2, 9, 11:19, 24, 118))  # not maize or legumes
    )
  ) %>%
  ungroup()
crops <- crops |> 
  group_by(household_id) |> 
  summarise(other_crops=sum(other_crops_count)) |> 
  ungroup()
eth5 <- left_join(eth5, crops, by="household_id")
var_label(eth5$other_crops) <- "number of other crops produced"
remove(maizehh, crops)














