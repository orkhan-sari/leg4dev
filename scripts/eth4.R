# eth4 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)
rm(list = ls())
### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Ethiopia", wave==4) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge) # 1025 households
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop, farm_size, 
         manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, primary_education_manager, respondent_id_obs, 
         respondent_id_merge, improved, organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, elevation, 
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock, flood_shock,irrigated, nb_seasonal_crop,
         used_pesticides, yield_value_LCU, yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) #many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
# RARE! Please refer to the qmd file \leg4dev\outputs\ethiopia.qmd

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
## There are few fields reported more than 1 with different holders.
## These are probably divided between hh members.
## Let us first identify these fields
## Identify groups where s3q34 has different values
field_with_diff_s3q34 <- read_dta("data/eth/ess4/sect3_pp_w4.dta") |>
  filter(s3q03==1) |>
  group_by(household_id, parcel_id, field_id) |> # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) |> # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) |> # Keep only groups where s3q34 has more than one unique value
  ungroup()
field_with_diff_s3q34 |>count(unique_s3q34) # 12 observations 

# At this moment this data is to identify if rotation was done in the field.
# just check the duplicates for rotation. Drop duplicates with no informative information. 
field_with_diff_s3q34 |>
  select(household_id, parcel_id, field_id, s3q34 ) |>
  head(12)

# obs to be dropped: 
# household_id       parcel_id field_id s3q34 
# 010103088800709032         1        2 2
# 030606088800202059         1        1 2
# 040606088801505048         2        1 2
# 060103088806701051         4        1 2
# 060206088800203045         1        1 2
# and one with pasture

field_with_diff_s3q34 <- field_with_diff_s3q34 |>
  filter(s3q34==2)|>
  select(household_id, parcel_id, field_id, s3q34)|>
  distinct()
head(field_with_diff_s3q34)

# drop observation above from the field data  
field <- read_dta("data/eth/ess4/sect3_pp_w4.dta") |>#field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q13, s3q34) |>
  filter(s3q03==1) |>
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) |>
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
field <- field |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
maizedata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") |>   #crop roster
  select(household_id, holder_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) |>
  filter(s4q01b==2) # keep only maize
maizedata <- maizedata |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-")) # field means plot in HP
maizedata <- semi_join(maizedata, maizeplots, by="plot_id") 
# We have more maize plots in HP than in row data. The reason is not clear, but we will trust the row data in this case.
# maize crop in hp comes from harvest section. 
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
  ungroup() # 0 duplicates 
look_for(cropdata)
cropdata4 <- cropdata |>
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
comment(cropdata4) <- "main crop maize plots"
table(cropdata4$rotation)
# 0    1 
# 1064  439 main maize fields had legume rotation
# Identify rotation at household level
rotation4 <- collap(cropdata4, rotation+count_plot~household_id, FUN=fmax)
table(rotation4$rotation)  # 332 adopters 691 non-adopters.
remove(maizedata, cropdata)
comment(rotation4) <- "hh level rotation"
var_label(rotation4$rotation) <- "If household rotated at least one maize field with legumes during the past 3y"
var_label(rotation4$count_plot) <- "number of main maize plots"
plotdata4 <- left_join(cropdata4, hpdata, by=c("plot_id")) |>
  select(-s3q03, -s3q34, -s4q02)
### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###

## DM at plot level ----
individual <- read_dta("data/eth/ess4/sect1_hh_w4.dta") |>
  mutate(female=ifelse(s1q02==2,1,0)) |>
  select(household_id, individual_id, female) |>
  rename(s3q13=individual_id)
field <- semi_join(field, maizeplots, by="plot_id") # keep only main maize plots and look at the DM on those plots.
field4 <- left_join(field, individual, by=c("household_id", "s3q13"))
remove(field)
var_label(field4$female) <- "female primary dm concern. activities on maize plot" # also on crops to be planted
field4 <- field4 |>select(household_id, parcel_id, field_id, plot_id, female)
plotdata4 <- left_join(plotdata4,field4, by=c("household_id", "parcel_id", "field_id", "plot_id"))
look_for(plotdata4)
remove(field4)
table(plotdata4$female_manager, plotdata4$female) # comparing manager and decision-maker. !there are missing values in manager!
# 0    1
# 0 1179    0
# 1    0  260 # Perfect match, managers are decision-makers. 
# but 64 missing data on both 
plotdata4 <- plotdata4 |> select(-female)

## Asset ownership ----
assets <- read_dta("data/eth/ess4/sect11_hh_w4.dta") |>
  select(household_id, asset_cd,s11q00, s11q02_1, s11q02_2) |> # only first two owners considered as in wave5
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
remove(individual, cropdata4)
graphics.off()

## Education ----
individual <- read_dta("data/eth/ess4/sect1_hh_w4.dta") |> 
  select(household_id, individual_id, s1q01, s1q02, s1q03a) |>
  mutate(female=ifelse(s1q02==2,1,0)) |>
  filter(female==1) |> 
  filter(s1q03a>17)
edu <- read_dta("data/eth/ess4/sect2_hh_w4.dta") |> 
  select(household_id, individual_id, s2q03, s2q06) 
individual <- left_join(individual, edu, by=c("household_id", "individual_id"))
remove(edu)
frq(individual$s2q03)
individual <- individual |> 
  mutate(s2q03= if_else(is.na(s2q03), 2, s2q03))
# 50% have no education and some have very basic level; we will capture just if they can read and write
individual$s2q03 <- as.numeric(individual$s2q03)
individual <- collap(individual, s2q03 ~ household_id, FUN = fmin, na.rm = TRUE)
frq(individual$s2q03) # 51% can read and write
individual <- individual |> 
  mutate(s2q03=if_else(s2q03==2,0,1))

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###

# HDDS
# total consumption; LATER consider alternative: Nominal annual expenditure on food per adult equivalent
# consumption quantile
hhhp4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Ethiopia", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, totcons_USD, cons_quint, hh_asset_index,
         HDDS, share_kg_sold)
hhhp4 <- hhhp4|> 
  semi_join(maizehh, by=c("hh_id_merge"))
look_for(hhhp4) # 297 mising for share_kg_sold
# 60 maize producing households are not in the harmonized panel

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###

## Collapse / aggregate ----
plotdata4 <- plotdata4 |> 
  select(-s3q13, -s4q03,-s4q01b)
look_for(plotdata4)
wave4 <- plotdata4 |> 
  select(1, 10, 15:43)
look_for(wave4)  
wave4basics <- wave4 |> 
  select(household_id, hh_id_obs) |> 
  distinct()
wave4mean <- collap(wave4,
                    farm_size + age_manager + dist_market + elevation + plot_dist_household + ag_asset_index + 
                    soil_fertility_index  ~ household_id,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(wave4,
                   plot_area_GPS + nb_seasonal_crop + yield_value_LCU + yield_value_USD  ~ household_id,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "dist_market", "elevation", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "plot_area_GPS", "nb_seasonal_crop", "yield_value_LCU", "yield_value_USD")
wave4 <- wave4 %>% select(-all_of(aggregated_vars))
look_for(wave4)
wave4 <- wave4 |> 
  select(1, 6:8, 11:20)
look_for(wave4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
# fmax() fsum(), fmean(), etc. expects pure numeric vectors. 
# If we feed it a labelled or factor column (even if it looks numeric), we may get nonsense max value, 
# only one value being returned or etc. Thus:  
wave4 <- wave4 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, intercropped, plot_certificate, irrigated),
                as.numeric))
wave4max <- collap(wave4,
                   ~ household_id, 
                   FUN = fmax,
                   na.rm = TRUE)
wave4 <- left_join(wave4basics, wave4mean, by=join_by("household_id"))
wave4 <- left_join(wave4, wave4sum, by=join_by("household_id"))
wave4 <- left_join(wave4, wave4max, by=join_by("household_id"))
wave4 <- left_join(wave4, assets, by=join_by("household_id"))
wave4 <- left_join(wave4, hhhp4, by=c("household_id"="hh_id_merge"))
wave4 <- left_join(wave4, rotation4, by=join_by("household_id"))
wave4 <- left_join(wave4, individual, by=join_by("household_id"))
wave4 <- wave4 |> 
  mutate(female_edu=if_else(is.na(s2q03), 0, s2q03)) |> 
  select(-s2q03)
var_label(wave4$share_female_owned) <- "women's control over assets"
var_label(wave4$female_edu) <- "HH has an adult female who can read and write"
remove(wave4basics, wave4max, wave4sum,wave4mean,rotation4, assets, aggregated_vars, hhhp4, individual)

## Cleaning missing data ----
look_for(wave4)
# Many missing values!!!
# This is expected for some! In page 24 and 25 of Basic Information Document, Ethiopia Socioeconomic Survey 2018-19,
# it is mentioned that in several EAs, for several households, household data is missing but agri data is included.
# for some 7 EAs post-harvest is missing and etc.   
wave4 <- wave4 |> 
  filter(!is.na(hh_size)) # household level data from HP - some HHs have agri data but no household level data4
remove(maizehh, hpdata, plotdata4)
look_for(wave4)
missing <- is.na(wave4$age_manager) # 1 missing
wave4 <- subset(wave4, subset= !missing)
wave4 <- wave4 |> mutate(wave="wave4")

### share_kg_sold----
### has many missing (276); the reason is because of many have reported share above 100% percent. 
### Also, some did not harvest any crops.
### We will capture subsistence orientation, by looking at the question 22 in section 4 that asks: Do you intend to sell any 
### of [CROP] to be harvested during the rainy season?
sec4 <- read_dta("data/eth/ess4/sect4_pp_w4.dta") |> 
  select( household_id,holder_id, parcel_id, field_id, s4q22) |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
val_labels(sec4$s4q22) # 1 yes 2 no
sec4 <- semi_join(sec4, maizeplots, by="plot_id")
sec4 |> fgroup_by(household_id) |> fselect(s4q22) |>  fsum()
sec4 <- sec4 |> 
  mutate(subsistence_ag=if_else(s4q22==2,1,0))
sec4 %>%
  group_by(household_id) %>%
  summarise(n_unique = n_distinct(subsistence_ag, na.rm = TRUE)) %>%
  filter(n_unique > 1) # many cases where answer to the question differs. 
sec4 <- collap(sec4, subsistence_ag~household_id, FUN = fmin) # if some consider for sale then not a subsistence. 
wave4 <- left_join(wave4, sec4, by="household_id")
table(wave4$subsistence_ag)
var_label(wave4$subsistence_ag) <- "HH does not intend to sell any crop"
remove(missing, sec4)
look_for(wave4)
rural <- read_dta("data/eth/ess4/sect_cover_hh_w4.dta") |> 
  filter(saq14==1) # keep rural hhs.
wave4 <- semi_join(wave4, rural, by="household_id") # indeed all are rural farmers. 
remove(rural)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###

## Non-farm income----
wage <- read_dta("data/eth/ess4/sect4_hh_w4.dta") |> 
  filter(saq14==1) |> 
  select(household_id, s4q33, s4q33b, s4q37, s4q38, s4q39, s4q40, s4q41, s4q33, s4q33b, s4q47, s4q50) 
frq(wage$s4q41)
# some mismatch. some has worked 2 months or less,
#  but reported that the money received is for half a year or quarter.
# 7 and 8 can be assumed as they are from the inspection of the data. error is possible. 
wage <- wage |> 
  mutate(income=case_when(s4q41==5 ~ s4q37 * s4q40,
                        s4q41==4 ~ s4q37/2 * s4q40,
                        s4q41==3 ~ s4q37 * s4q38 * s4q40,
                        s4q41==2 ~ s4q37 * s4q38* s4q39/8 * s4q40,
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
enterprise <- read_dta("data/eth/ess4/sect12b1_hh_w4.dta") |> 
  filter(saq14==1) |> 
  select(household_id, s12bq12, s12bq16, s12bq17a, s12bq17b, s12bq17c, s12bq17d, s12bq17e, s12bq17f) 
enterprise <- enterprise |> 
  mutate(enterprise_income=s12bq12*s12bq16-s12bq12* (s12bq17a+s12bq17b+ s12bq17c+ s12bq17d+ s12bq17e+ s12bq17f))
enterprise <- collap(enterprise,enterprise_income~household_id, FUN = fsum)
other_income <- read_dta("data/eth/ess4/sect13_hh_w4_v2.dta") |> 
  filter(saq14==1) |> 
  select(household_id, source_cd, s13q01, s13q02) |> 
  filter(!is.na(s13q02))
table(other_income$source_cd, other_income$s13q01)
other_income <- collap(other_income,s13q02~household_id, FUN = fsum)
other_income <- other_income |> rename(other_income=s13q02)
wave4 <- left_join(wave4,wage,by="household_id")
table(is.na(wave4$wage_income)) # only 18 maize producers
wave4 <- wave4 |> 
  tidyr::replace_na(list(wage_income = 0))
wave4 <- left_join(wave4,other_income,by="household_id")
table(is.na(wave4$other_income)) # 221 maize producers
wave4 <- wave4 |> 
  tidyr::replace_na(list(other_income = 0))
wave4 <- left_join(wave4,enterprise,by="household_id")
table(is.na(wave4$enterprise_income)) # 104 maize producers
wave4 <- wave4 |> 
  tidyr::replace_na(list(enterprise_income = 0))
wave4 <- wave4 |> 
  mutate(tot_other_income=wage_income+other_income+enterprise_income)
var_label(wave4$tot_other_income) <- "Yearly total other income"
remove(wage,enterprise,other_income)

##  Extension, advisory, credit----
extension <- read_dta("data/eth/ess4/sect7_pp_w4.dta") |> 
  filter(saq14==1) |> 
  select("household_id", "s7q04", "s7q06", "s7q08",  "s7q08_os","s7q09", "s7q10" )
look_for(extension)
frq(extension$s7q04)
frq(extension$s7q09) # the label should be do you get advisory services?
extension <- extension |>  
  mutate(extension_program= case_when(s7q04==1 ~ 1,
                                      .default = 0)) |> 
  mutate(advisory_service= case_when(s7q09==1 ~  1,
                                     .default = 0)) |> 
  mutate(credit_unavailable=case_when(s7q08==1 ~ 1,
                                      .default = 0)) |>  
  select(household_id,extension_program, advisory_service, credit_unavailable)
# The information is gathered at the holder level. For HH level data, we collapse and choose the max value. 
extension <- collap(extension, extension_program +advisory_service + credit_unavailable ~household_id, FUN=fmax)
wave4 <- left_join(wave4,extension,by="household_id")
remove(extension)

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/eth/ess4/sect8_1_ls_w4.dta") |> 
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
eth4 <- left_join(wave4,tlu,by="household_id")
remove(tlu, wave4)
eth4$tlu[is.na(eth4$tlu)] <- 0

### ### ### ### ### ### ### ###
## Soil type ---- 
### ### ### ### ### ### ### ###
soil <- read_dta("data/eth/ess4/sect2_pp_w4.dta") |> 
  mutate(parcel_id = paste(holder_id, parcel_id, sep = "-")) |> 
  select("household_id", "parcel_id", "s2q16")
soil <- semi_join(soil, maizeplots, by=c("parcel_id"="parcel_id_merge"))
look_for(soil)
frq(soil$s2q16)
labels <- attributes(soil$s2q16)
soil$s2q16 <- as.numeric(soil$s2q16)
soil <- collap(soil, s2q16 ~ household_id, FUN = fmode, na.rm= TRUE)
attributes(soil$s2q16) <- labels
eth4 <- left_join(eth4, soil, by="household_id")
eth4 <- eth4 |> 
  rename(soil_type=s2q16)
var_label(eth4$soil_type) <- "HH dominant soil type"
frq(eth4$soil_type)
remove(soil, maizeplots, labels)

# Notes on missing data ----
## 95 hhs do not have agricultural assets but still have asset index. This is because, 
## pca (factor) analysis still includes those with no asset and gives them a lower score. 
## Some HHs have NA share_female owned assets, because they do not own any valuable assets. 
## 295 missing for share_kg_sold; the reason is because of many have reported share above 100% percent. 
## Also, some did not harvest any crops. 

## Manager education ----
eth4 |> 
  filter(is.na(primary_education_manager)) |> 
  select(household_id)
# 16 households have missing education information. These are also missing in the row data, so not a hpdata issue. 
# After close inspection of the row data, we conclude that these most likely do not have any primary education. 
