# mlw 2 ----
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
  filter(country=="Malawi", wave==2) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  filter(main_crop=="MAIZE") |> 
  mutate(yield_value_USD= yield_value_USD * (share_value_LEGUMES + share_value_MAIZE)) |> 
  select(hh_id_merge, hh_id_obs, ea_id_merge, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, twi, improved, soil_fertility_index, elevation,
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock, flood_shock,
         irrigated, nb_seasonal_crop, used_pesticides, yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
hpdata <- hpdata |>
  group_by(hh_id_obs) |> 
  mutate(crop_count=sum(nb_seasonal_crop)) |> 
  relocate(crop_count, .after = nb_seasonal_crop) |> 
  ungroup()
look_for(hpdata) # many missing, especially improved seeds

### ### ### ### ### ### ### ###
# Crop diversification ----
### ### ### ### ### ### ### ###

## Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_13.dta") |>   # plot level 
  select(y2_hhid, ag_d00, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e) |> 
  mutate(plot_id = paste(y2_hhid, ag_d00, sep = "-"))
look_for(garden)
garden <- semi_join(garden, maizeplots, by="plot_id")
frq(garden$ag_d14) # some are fallow and missing
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
frq(garden$ag_d20a)
garden <- garden |> 
  group_by(y2_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in very rare cases other crops are listed first, 
# but the kept obs are those with maize as the highest value crop
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(11:16, 27, 34:36, 46))
  ),
  hybrid_maize=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(2:4))
  ))
## improved maize  ----
seed <- read_dta("data/mlw/ag_mod_g_13.dta") |> 
  mutate(plot_id = paste(y2_hhid, ag_g00, sep = "-")) |> 
  filter(ag_g0a==1) |> 
  select(y2_hhid, plot_id, ag_g0b)
seed <- semi_join(seed, maizeplots, by="plot_id")
frq(seed$ag_g0b)
seed <- seed |> filter(ag_g0b %in% c(1:4))
#-------------------------------------------------# 
# Unlike mlw3 and 4, here the question on type was not directly asked.
# Thus will generate from the crop code ag_g0b
seed <- seed |> 
  mutate(hybrid_maize1= case_when(ag_g0b %in% c(2:4) ~ 1,
                                  .default = 0)) |> 
  select(plot_id, hybrid_maize1)
frq(seed$hybrid_maize1)
garden <- left_join(garden, seed, by= "plot_id")
frq(garden$hybrid_maize1)
garden$hybrid_maize1[is.na(garden$hybrid_maize1)] <- 0
table(garden$hybrid_maize,garden$hybrid_maize1) # many differences. 
remove(seed)
garden <- garden |> 
  mutate(iv_maize= if_else(hybrid_maize1==1 & hybrid_maize==1 , 1 , 0)) |> 
  select(-hybrid_maize, -hybrid_maize1)
# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

### ### ### ### ### ### ### ###
## Maize - legume rotation----
# If legume was cultivated after the rainy season
### ### ### ### ### ### ### ###
modm <- read_dta("data/mlw/ag_mod_m_13.dta") |> 
  select(y2_hhid, ag_m00, ag_m0c) |> 
  filter(!is.na(ag_m0c))
# plot id is with D's (dry) but with R's in the rainy season
library(stringr)
modm <- modm |> 
  mutate(ag_m00 = str_replace(ag_m00, "^D", "R")) |> 
  mutate(plot_id = paste(y2_hhid, ag_m00, sep = "-"))
val_labels(modm$ag_m0c)
modm <- modm |> 
  mutate(rotation=case_when(ag_m0c %in% c(11:16, 27, 34:36, 46) ~ 1,
                          .default = 0)) |> 
  filter(rotation==1) # only 82 observations 
modm |> 
  group_by(plot_id)|>
  filter(n() > 1)|>
  ungroup() #1 duplicate with beans and peas, thus 81 plots only. 
modm <- modm |> 
  distinct(plot_id, .keep_all = TRUE) |> 
  select(plot_id, rotation)
garden <- left_join(garden, modm, by="plot_id")
frq(garden$rotation) # 59 plots only.
garden$rotation[is.na(garden$rotation)] <- 0
plot2 <- garden |> 
  select(y2_hhid, plot_id, count_plot, intercrop, rotation, iv_maize)
remove(garden,modm)
var_label(plot2$intercrop) <- "=1 if legume intercropped maize plot"
var_label(plot2$rotation) <- "=1 if legume is cultivated after" # ONLY AFTER CAN BE IDENTIFIED
var_label(plot2$count_plot) <- "number of main maize plots"
var_label(plot2$iv_maize) <- "=1 if improved maize was cultivated plot section"

### ### ### ### ### ### ### ###
# Soil management ----
### ### ### ### ### ### ### ###
soilman <-  read_dta("data/mlw/ag_mod_d_13.dta") |>   # plot level 
  select(y2_hhid, ag_d00, ag_d01, ag_d36, ag_d38, ag_d60, ag_d62) |> 
  mutate(plot_id = paste(y2_hhid, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
soilman <- semi_join(soilman, maizeplots, by="plot_id")
look_for(soilman)
frq(soilman$ag_d60)
# 1 LEFT IN THE FIELD UNTIL LAND PREPARED FOR CULTIVATION
# in next round this is Left in the field and then ploughed /incorporated in to the field
# unsure if this actual means crop residue cover 
# 4 and 5: ALL RESIDUES CUT AND SPREAD OVER PLOT; this may the CRC. 
frq(soilman$ag_d62) # very few practiced min or zero tillage
soilman <- soilman |> 
  mutate(min_till=if_else(!is.na(ag_d62) & ag_d62 == 6, 1, 0),
         zero_till=if_else(!is.na(ag_d62) & ag_d62 == 4, 1, 0),
         crop_residue=if_else(!is.na(ag_d60) & ag_d60 %in% c(4,5), 1, 0),
         inorganic_fertilizer= if_else(!is.na(ag_d38) & ag_d38== 1 , 1, 0),
         organic_fertilizer= if_else(!is.na(ag_d36) & ag_d36== 1 , 1, 0)
         )
frq(soilman$min_till) # 1% 
frq(soilman$zero_till) # 0.6%
frq(soilman$crop_residue) # 8% most likely incorrect because other rounds have very low values with better question and answer
frq(soilman$organic_fertilizer)
frq(soilman$inorganic_fertilizer)
soilman <- soilman |> 
  select(plot_id, min_till, zero_till, crop_residue, inorganic_fertilizer, organic_fertilizer)
plot2 <- left_join(plot2, soilman, by="plot_id")
frq(plot2$crop_residue)
frq(plot2$zero_till)
frq(plot2$min_till)
remove(soilman)
var_label(plot2$min_till) <- "minimum tillage: one-pass till"
var_label(plot2$zero_till) <- "no till"
var_label(plot2$crop_residue) <- "Cover the field surface with crop residue/leaves"
var_label(plot2$organic_fertilizer) <- "organic fertilizer applied to maize plot"
var_label(plot2$inorganic_fertilizer) <- "inorganic fertilizer applied to maize plot"

## Soil type ---- 
modd <-  read_dta("data/mlw/ag_mod_d_13.dta") |>   # plot level 
  select(y2_hhid, ag_d00, ag_d01, ag_d21) |> 
  mutate(plot_id = paste(y2_hhid, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
look_for(modd)
frq(modd$ag_d21)
modd <- modd |> 
  mutate(ag_d21 = as.numeric(modd$ag_d21),
         soil_type=case_when(ag_d21==3 ~ 2,
                             ag_d21==1 ~ 1,
                             .default = 0))
modd <- modd |> 
  mutate(soil= factor(soil_type, levels = c(0,1,2), labels = c("Other or mix", "Sandy", "Clay"))) |> 
  select(plot_id, soil)
plot2 <- left_join(plot2, modd, by="plot_id")
var_label(plot2$soil) <- "HH dominant soil type"

## Slop ----
modd <-  read_dta("data/mlw/ag_mod_d_13.dta") |> 
  select(y2_hhid, ag_d00, ag_d26) |> 
  mutate(plot_id = paste(y2_hhid, ag_d00, sep = "-"))
modd <- semi_join(modd, maizeplots, by="plot_id")
frq(modd$ag_d26)
modd <- modd |> filter(!is.na(ag_d26))
modd <- modd |>
  mutate(ag_d26 = as.numeric(modd$ag_d26))
modd <- modd |> 
  mutate(slope= factor(ag_d26, levels = c(1,2,3,4), labels = c("Flat", "Slight slope", "Moderate slope", "Steep"))) |> 
  select(plot_id, slope)
frq(modd$slope)
plot2 <- left_join(plot2, modd, by="plot_id")
var_label(plot2$slope) <- "Dominant slope"
remove(modd)

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <-  read_dta("data/mlw/ag_mod_d_13.dta") |>   # plot level 
  select(y2_hhid, ag_d00, ag_d01) |> 
  mutate(plot_id = paste(y2_hhid, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_13.dta") |> 
  select(y2_hhid, hh_b01, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  rename(id_code=hh_b01)
modd <- left_join(modd,individual, by=c("y2_hhid", "id_code"))
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b05a )
plot2 <- left_join(plot2, modd, by="plot_id")
plot2 <- plot2 |> rename(age_manager_row=hh_b05a)
remove(modd, individual)

## DM on crop income ----
sales <- read_dta("data/mlw/ag_mod_g_13.dta") |>
  select(y2_hhid, ag_g00, ag_g0a, ag_g14a, ag_g14b) |> 
  mutate(plot_id = paste(y2_hhid, ag_g00, sep = "-"))
sales <- semi_join(sales, maizeplots, by="plot_id")
# Unfortunately, there are many missing values for the question on
# Who in the household makes the decision concerning the use of the [CROP]? 
# Also we find many missing on crop income in next waves, because many did not sell and thus did not make any decisions 


## Asset ownership ---- 
## no data available on owners

## Education ----
## variable of interest: if HH has an educated adult female
individual <- read_dta("data/mlw/hh_mod_b_13.dta") |> 
  select(y2_hhid, hh_b01, hh_b03, hh_b05a, hh_b22_3) |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  filter(female==1) |> 
  filter(hh_b05a>17) |> 
  rename(id_code=hh_b01)
individual <- semi_join(individual, maizehh, by=c("y2_hhid" = "hh_id_merge"))
edu <- read_dta("data/mlw/hh_mod_c_13.dta") |> 
  select(y2_hhid, hh_c01, hh_c08 ) |> 
  rename(id_code=hh_c01)
individual <- left_join(individual, edu, by=c("y2_hhid", "id_code"))
individual$hh_c08 <- as.numeric(individual$hh_c08)
individual <- individual |> 
  filter(!is.na(hh_c08)) |> 
  mutate(female_edu=ifelse(hh_c08 > 7,1,0)) |> # =1 if hh has educated female with at least primary education completed. 
  select(y2_hhid, female_edu)
individual <- collap(individual, female_edu ~ y2_hhid, FUN = fmax)
frq(individual$female_edu)
remove(edu)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds2 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==2) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds2 <- hdds2|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds2) # 208 missing for share_kg_sold

# FCS
fcs <- read_dta("data/mlw/hh_mod_g2_13.dta")
look_for(fcs)
summary(fcs)
fcs$hh_g08c[is.na(fcs$hh_g08c)] <- 0
frq(fcs$hh_g08a)
table(fcs$hh_g08a, fcs$hh_g08c)
fcs <- fcs |> 
  group_by(y2_hhid) %>%
  mutate(
    sum_A_B = sum(hh_g08c[hh_g08a %in% c(1,2)], na.rm = TRUE),
    sum_A_B = case_when(sum_A_B > 7 ~ 7,
                        .default = sum_A_B),
    count = case_when(hh_g08a== 1 ~ sum_A_B,
                      .default = hh_g08c)
  ) |> 
  ungroup()
fcs <- fcs |> 
  filter(hh_g08a != 2)
table(fcs$hh_g08a,fcs$count)
frq(fcs$hh_g08a)
fcs <- fcs |>
  mutate(total=case_when(hh_g08a == 1 ~ count * 2,
                         hh_g08a == 3 ~ count * 3,
                         hh_g08a == 4 ~ count * 1,
                         hh_g08a == 5 ~ count * 4,
                         hh_g08a == 6 ~ count * 1,
                         hh_g08a == 7 ~ count * 4,
                         hh_g08a == 8 ~ count * 0.5,
                         hh_g08a == 9 ~ count * 0.5,
                         hh_g08a == 10 ~ count * 0)
  )
fcs <- fcs |> 
  group_by(y2_hhid) |> 
  summarise(fcs=sum(total)) |> 
  ungroup()
summary(fcs$fcs)
fcs <- semi_join(fcs, maizehh, by=c("y2_hhid" = "hh_id_merge"))
hdds2 <- left_join(hdds2, fcs, by=c("hh_id_merge" = "y2_hhid"))
rm(fcs)

# Food security
fs <- read_dta("data/mlw/hh_mod_h_13.dta") |> 
  select(y2_hhid, hh_h01, hh_h04)
look_for(fs)
fs <- fs |> 
  mutate(enough_food = if_else(hh_h01==1, 1, 0),
         enough_food12 = if_else(hh_h04==1, 1, 0)
  ) |> 
  select(-hh_h01, -hh_h04)
fs <- semi_join(fs, maizehh, by=c("y2_hhid" = "hh_id_merge"))
hdds2 <- left_join(hdds2, fs, by=c("hh_id_merge" = "y2_hhid"))
rm(fs)

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot2 <- left_join(plot2, hpdata, by="plot_id")
remove(hpdata)
look_for(plot2)
plot2 <- plot2 |> rename(hhid=y2_hhid)
plot2 <- plot2 |> relocate(ea_id_merge,.after = hhid)
table(plot2$female_manager, plot2$female) # same information.
plot2 <- plot2 |> select(-female)
# manager age is missing for 2 plots.

## Collapse / aggregate ----
mlw2 <- plot2 |> 
  select(-hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,-age_manager_row, -nb_seasonal_crop)
look_for(mlw2)
mlw2$improved <- NULL # missing
n_distinct(mlw2$hh_id_obs) # 1252 observations 1746 plots
mlw2 <- mlw2 |> 
  filter(!is.na(plot_area_GPS))
n_distinct(mlw2$hh_id_obs) # 1242 observations 1732 plots
look_for(mlw2)
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> 
  mutate(plot_area=sum(plot_area_GPS, na.rm = TRUE),
         yield_value_USD=sum(yield_value_USD, na.rm = TRUE)) |> 
  ungroup() |> 
  relocate(plot_area, .after = plot_area_GPS)
# control if hh used any organic or inorganic fertilizer in intercrop field or any other field.
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> 
  mutate(iv_maize = max(iv_maize, na.rm = TRUE),
         inorganic_fertilizer = max(inorganic_fertilizer, na.rm = TRUE),
         organic_fertilizer = max(organic_fertilizer, na.rm = TRUE),
         rotation_check=max(rotation, na.rm = TRUE))|> 
  ungroup()

### aggregating the information at the household level for adopters and non-adopters seperately.----
### 1. Select adopted plots If there are few intercropped fields select the biggest and 
### adjust if rotation was practiced in any field of the hh
### 2. Among the non-adopted plots select the biggest field. 
n_distinct(mlw2$plot_id) # 1718 but 1746 obs so duplicate ids
table(mlw2$intercrop, mlw2$rotation) # 890 non adopted plots
mlw2adoption <- mlw2 |> 
  relocate(hh_id_obs, .after = hhid) |> 
  mutate(keep=if_else(intercrop==1 & rotation==1, 1, 0))
# keep only adopting households; for non-adopters will select the biggest parcel and manager characteristics
mlw2adoption <- mlw2adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop != 0 | rotation != 0) |> 
  ungroup()
table(mlw2adoption$intercrop, mlw2adoption$rotation)
# there are only 26 observations where both rotation and intercrop are observed.
# first keep these plots and hhs
mlw2adoption <- mlw2adoption |> 
  group_by(hh_id_obs) |> 
  mutate(keephh=max(keep)) |> 
  ungroup()
mlw2adoption <- mlw2adoption |>
  filter((keep==0 & keephh==0) | (keep==1 & keephh==1)) 
mlw2adoption$keephh <- NULL
comment(mlw2adoption) <- "only adopter hhs and adopted plots"
mlw2adoption <- mlw2adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop==max(intercrop, na.rm = TRUE )) |>  # there were only three parcel that had been rotated but not intercropped
  ungroup()
mlw2adoption <- mlw2adoption |>
  mutate(rotation=rotation_check) # for the indication of adoption
# keep the biggest plot
mlw2adoption <- mlw2adoption |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(mlw2adoption$hh_id_obs) # 639 hhs, still some duplicate plots
mlw2adoption <- mlw2adoption %>%
  distinct(across(-plot_id), .keep_all = TRUE)
# still 15 duplicates remaining
mlw2adoption <- mlw2adoption |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hhid) |>  ungroup()
# differences are because of some minor difference in plot distance or soil type. select random here. 
mlw2adoption <- mlw2adoption |>
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & plot_dist_household==min(plot_dist_household, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
     ) |> 
  ungroup()
# one more here
mlw2adoption <- mlw2adoption |> select(-dupl) |>
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hhid) |>  ungroup()
# now slope
mlw2adoption <- mlw2adoption |>
  mutate(slopen=as.numeric(slope)) |> 
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & slopen==max(slopen, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) |> 
  ungroup() |> 
  select(-slopen)
n_distinct(mlw2adoption$hh_id_obs)

# select the biggest plot among non-adopters
mlw2 <- mlw2 |> 
  filter(intercrop == 0 & rotation == 0) 
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(mlw2$hh_id_obs) # 698 hh
mlw2 <- mlw2 |> distinct() # 
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
# investigating this duplicates we see that these are the same size
look_for(mlw2)
# trying complete cases to see if any hh will be dropped at the end. if this resutls in dropping a hh,
# approach is changed later
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> 
  mutate(
    complete_row = complete.cases(pick(everything())) # TRUE if no NAs in this row
  ) |> 
  filter(
    # if hh has duplicates → keep only complete rows
    (dupl > 1 & complete_row) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) %>%
  ungroup() |>  # no change
  select(-complete_row)
mlw2 <- mlw2 |> # seelcting neares plot, differences are often meager
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & plot_dist_household==min(plot_dist_household, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) |> 
  ungroup()
mlw2 <- mlw2 |> select(-dupl) |>
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hhid) |>  ungroup() 
# no difference in remaining 
mlw2 <- mlw2 |> select(-dupl) |> distinct(hh_id_obs, .keep_all = TRUE) 
# Combine adopter and non-adopter hhs
mlw2 <- bind_rows(mlw2, mlw2adoption)
n_distinct(mlw2$hh_id_obs) # 1242 hhs
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs) |> 
  arrange(desc(dupl), hh_id_obs)
# There are hhs, which have adopter plots and non-adopted plots. keep only adopted plots
mlw2 <- mlw2 |> 
  group_by(hh_id_obs) |> 
  filter((dupl>1 & (intercrop==1 | rotation==1)) |
           dupl==1)
mlw2 <- mlw2 |> 
  select(-dupl, -main_crop, -rotation_check, -keep) |> 
  arrange(hh_id_obs)
look_for(mlw2) 
var_label(mlw2$plot_area) <- "Area (in hectares) of all maize plots"
labs <- var_label(plot2)
common_names <- intersect(names(mlw2), names(labs))
var_label(mlw2)[common_names] <- labs[common_names]
look_for(mlw2) 
rm(labs, mlw2adoption, common_names)
mlw2$plot_area_GPS <- NULL
look_for(mlw2)
mlw2 <- left_join(mlw2, hdds2, by=c("hhid"="hh_id_merge"))
mlw2 <- left_join(mlw2, individual, by=c("hhid"="y2_hhid"))
mlw2 <- mlw2 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(hdds2, plot2, individual)
mlw2 <- mlw2 |> mutate(wave="wave2")

## Cleaning missing data ----
look_for(mlw2)
mlw2 <- mlw2 |> select(-manager_id_merge)
### share_kg_sold----
### share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  We will capture subsistence orientation: HHs do not sell any crops. 
sales <- read_dta("data/mlw/ag_mod_i_13.dta") |> 
  select(y2_hhid, ag_i0b, ag_i01)
sales <- semi_join(sales, maizehh, by=c("y2_hhid"="hh_id_merge"))
look_for(sales)
frq(sales$ag_i01)
sales <- sales |> mutate(subsistence_ag=if_else(ag_i01==2,1,0))
sales <- collap(sales, subsistence_ag~y2_hhid, FUN = fmin, na.rm = TRUE) # if some consider for sale then not a subsistence. 
sales <- sales |> rename(hhid=y2_hhid)
mlw2 <- left_join(mlw2, sales, by="hhid")
frq(mlw2$subsistence_ag) # 4 still missing. no harvest? 
remove(sales)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###
## Distance ----
# No wave 2 data 
# market_distance from community is missing in this wave

## Non-farm income ----
wage <- read_dta("data/mlw/hh_mod_e_13.dta")
look_for(wage, details = c("none"))
wage <- wage[,c(2, 35:36, 44:50, 53:55, 71:77, 80:82, 101:104)]
wage <- semi_join(wage, maizehh, by=c("y2_hhid"="hh_id_merge"))
look_for(wage, details = c("none"))
mainwage <- wage[,1:13]
mainwage <- mainwage |> filter(!is.na(hh_e19b))
frq(mainwage$hh_e26b)
mainwage <- mainwage |> 
  mutate(main_income=case_when(hh_e26b==5 ~ hh_e25 * hh_e22/hh_e26a,
                               hh_e26b==4 ~ hh_e25 * hh_e22 * hh_e23 /hh_e26a,
                               hh_e26b==3 ~ hh_e25 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e26a,
                               .default = NA_real_)) |>  # very rough calculation 
  mutate(main_income=as.numeric(main_income))
# 4 missing; one teacher probably gave total year. 
mainwage |> filter(y2_hhid=="1738-001")
# difficult to decide if this is an enumerator error or there is another person who earned much less. ignore
mainwage$main_income[is.na(mainwage$main_income)] <- 0
look_for(mainwage)
frq(mainwage$hh_e28b)
mainwage <- mainwage |> 
  mutate(allowance=case_when(hh_e28b==5 ~ hh_e27 * hh_e22/hh_e28a,
                             hh_e28b==4 ~ hh_e27 * hh_e22 * hh_e23 /hh_e28a,
                             hh_e28b==3 ~ hh_e27 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e28a,
                             .default = NA_real_)) |>  # very rough calculation 
  mutate(allowance=as.numeric(allowance))
mainwage <- mainwage |> mutate(income=rowSums(across(c(main_income,allowance)), na.rm=TRUE))
mainwage <- mainwage |> select(y2_hhid, income)
look_for(wage, details = c("none"))
#secondwage
secondwage <- wage[,c(1,14:27)]
look_for(secondwage)
secondwage <- secondwage |> filter(!is.na(hh_e36))
frq(secondwage$hh_e40b) # 5 obs
secondwage <- secondwage |>
  mutate(main_income=case_when(hh_e40b==5 ~ hh_e39 * hh_e36/hh_e40a,
                               hh_e40b==4 ~ hh_e39 * hh_e36 * hh_e37 /hh_e40a,
                               hh_e40b==3 ~ hh_e39 * (hh_e36 * hh_e37 * hh_e38/8) /hh_e40a,
                               .default = NA_real_)) |> 
  mutate(main_income=as.numeric(main_income))
frq(secondwage$hh_e42a) # no allowance
secondwage <- secondwage |> 
  rename(income=main_income) |> 
  select(y2_hhid, income)
look_for(wage, details = c("none"))
#ganyu
ganyu <- wage[,c(1,24:27)]
look_for(ganyu)
ganyu <- ganyu |> filter(!is.na(hh_e56))
ganyu <- ganyu |> 
  mutate(income=hh_e56*hh_e57*hh_e58*hh_e59)
ganyu <- collap(ganyu,income~y2_hhid, FUN=fsum)
wage <- rbind(ganyu, mainwage, secondwage)
remove(ganyu, mainwage, secondwage)
wage <- collap(wage,income~y2_hhid, FUN=fsum)
mlw2 <- left_join(mlw2, wage, by=c("hhid"="y2_hhid"))
mlw2 <- mlw2 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(mlw2$wage_income) <- "income from main and temporary off-farm work"
var_label(mlw2$subsistence_ag) <- "HH did not sell any crop harvest"
var_label(mlw2$female_edu) <- "HH has an educated adult female"
remove(wage)

# enterprise
enterprise <- read_dta("data/mlw/hh_mod_n2_13.dta")
look_for(enterprise, details=c("none"))
enterprise <- semi_join(enterprise,  maizehh, by=c("y2_hhid"="hh_id_merge"))
# It is not possible to calculate monthly profits from the enterprise.
# We will take the last month's profit. 
# Enterprise data is better calculated for the last month's profit. An annual income cannot be precisely calculated.
# Thus, to sum all other income, we must consider average monthly income for the wage income as well 
# (unlike in ETH where yearly is calculated). 
enterprise <- enterprise |> 
  filter(!is.na(hh_n40)) |> 
  rename(enterprise_income=hh_n40)
enterprise <- collap(enterprise, enterprise_income ~ y2_hhid, FUN=fsum)   

# other income
other_income <- read_dta("data/mlw/hh_mod_p_13.dta") |> 
  filter(hh_p01==1)
other_income <- semi_join(other_income,  maizehh, by=c("y2_hhid"="hh_id_merge"))
look_for(other_income, details = c("none"))
other_income <- other_income |> 
  mutate(other_income=rowSums(across(c(hh_p02, hh_p03a, hh_p03b, hh_p03c)), na.rm=TRUE))
other_income <- collap(other_income, other_income ~ y2_hhid, FUN=fsum)
# other income is yearly,
# wage_income is yearly,
# enterprise is but last month. 
mlw2 <- left_join(mlw2, other_income, by=c("hhid"="y2_hhid"))
mlw2 <- mlw2 |> 
  tidyr::replace_na(list(other_income = 0))
mlw2 <- left_join(mlw2, enterprise, by=c("hhid"="y2_hhid"))
mlw2 <- mlw2 |> 
  tidyr::replace_na(list(enterprise_income = 0))
mlw2 <- mlw2 |> 
  mutate(other_income=other_income / 12,
         wage_income=wage_income / 12,
         tot_other_income=wage_income+other_income+enterprise_income)
var_label(mlw2$tot_other_income) <- "Average monthly total other income"
remove(enterprise,other_income)
look_for(mlw2)

##  Extension, advisory, credit----
# We will consider most important sources like government and private ag. extension.
extension <- read_dta("data/mlw/ag_mod_t1_13.dta") |> 
  select(y2_hhid, ag_t0a, ag_t01, ag_t02) |> 
  filter(ag_t01==1)
frq(extension$ag_t0a)
frq(extension$ag_t02)
# Many report to have received extension from electronic media like tv and radio and other farmers. 
# We will consider those extension services that have been provided by gov, private and cooperative type institutions. 
extension <- extension |> 
  mutate(extension_service=case_when(ag_t0a %in% c(301:307, 309) &
                                       ag_t02 %in% c(1:10) ~ 1,
                                     .default = 0))
extension <- collap(extension, extension_service~y2_hhid, FUN = fmax)
frq(extension$extension_service)
mlw2 <- left_join(mlw2, extension, by=c("hhid"="y2_hhid"))
mlw2$extension_service[is.na(mlw2$extension_service)] <- 0
var_label(mlw2$extension_service) <- "received extension from institutions" 
remove(extension)

### Credit ----
credit <- read_dta("data/mlw/hh_mod_s2_13.dta") |> 
  select(y2_hhid, hh_s12, hh_s19a, hh_s19b)
credit <- credit |> 
  mutate(credit_constrained=case_when(
    hh_s19a %in% c(2,5,7) ~ 1,
    !is.na(hh_s19a) ~ 0,
    .default = NA_integer_)) # only looking at the main reasons
credit <- credit |> 
  filter(!is.na(credit_constrained)) |> 
  select(y2_hhid, credit_constrained)
mlw2 <- left_join(mlw2, credit, by=c("hhid"="y2_hhid"))
mlw2$credit_constrained[is.na(mlw2$credit_constrained)] <- 0
frq(mlw2$credit_constrained)
var_label(mlw2$credit_constrained) <- "becuase no collateral or no access to lender"
remove(credit)

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/mlw/ag_mod_r1_13.dta") |> 
  select(y2_hhid, ag_r00, ag_r0a, ag_r01, ag_r02) |> 
  filter(ag_r01==1)
frq(tlu$ag_r0a)
tlu <- tlu |> 
  mutate(tlu=case_when(ag_r0a %in% c(301:304, 3304) ~ ag_r02 * 0.7,
                       ag_r0a %in% c(307:308) ~ ag_r02 * 0.1,
                       ag_r0a==309 ~ ag_r02 * 0.2,
                       ag_r0a %in% c(311:315, 319, 3310, 3314) ~ ag_r02 * 0.01,
                       ag_r0a==3305 ~ ag_r02 * 0.7, 
                       .default = 0))
tlu <- collap(tlu, tlu ~ y2_hhid, FUN = fsum)
mlw2 <- left_join(mlw2, tlu, by=c("hhid"="y2_hhid"))
mlw2$tlu[is.na(mlw2$tlu)] <- 0
var_label(mlw2$tlu) <- "Tropical livestock units"
remove(tlu)
look_for(mlw2)
mlw2 <- mlw2 |> 
  rename(ea_id=ea_id_merge)

## Land tenure - not collected ----
## We will use information about MOST recent disputes from 2016 data. 
## This is not perfect and likely has a measurement error, because the question in 2016 asks about the most recent dispute.
## Thus hh's with consistent disputes or multiple disputes may not be correctly captured with below approach. 
## However, this is the only available data we have. 
tenure <- read_dta("data/mlw/ag_mod_v_16.dta") |> 
  select(y3_hhid, ag_v02_1) |> 
  filter(!is.na(ag_v02_1)) |> 
  filter(ag_v02_1<2014) # keep those disputes before 2011 for first wave
tenure <- collap(tenure, ag_v02_1 ~ y3_hhid, FUN = fmax, na.rm=TRUE)
maizehh16 <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==3) |> 
  filter(main_crop=="MAIZE") |> 
  distinct(hh_id_merge, hh_id_obs)
tenure16 <- left_join(maizehh16, tenure, by=c("hh_id_merge"="y3_hhid"))
remove(tenure, maizehh16)
tenure16 <- tenure16 |> select(hh_id_obs, ag_v02_1)
mlw2 <- left_join(mlw2, tenure16, by="hh_id_obs")
# If hhs did not have any disputes before 2013, we will assume that these hhs are very likely to possess land in 10 years too.
mlw2 <- mlw2 |>
  mutate(tenure_security=(case_when(is.na(ag_v02_1) ~ 1,
                                    ag_v02_1<2014 ~ 0,
                                    .default = 1))) |> 
  select(-ag_v02_1)
var_label(mlw2$tenure_security) <- "very likely to still possess land in ten years"  
remove(tenure16)

## Input coupon ---- 
coupon <- read_dta("data/mlw/ag_mod_e2_13.dta") |> 
  filter(ag_e0a==1) |>  # obtained coupon
  select(y2_hhid, ag_e01_1, ag_e07)
frq(coupon$ag_e01_1)
frq(coupon$ag_e07)
look_for(coupon)
coupon <- coupon |>
  mutate(fert_voucher=if_else(as.integer(ag_e07) %in% c(1:6), 1, 0),
         maize_voucher=if_else(as.integer(ag_e07) %in% c(12, 15), 1, 0),
         bean_voucher=if_else(as.integer(ag_e07) %in% c(16:19), 1, 0) 
         )
coupon <- collap(coupon, maize_voucher + fert_voucher + bean_voucher ~ y2_hhid, FUN = fmax, na.rm=TRUE)
mlw2 <- left_join(mlw2, coupon, by=c("hhid"="y2_hhid"))
mlw2$maize_voucher[is.na(mlw2$maize_voucher)] <- 0
mlw2$fert_voucher[is.na(mlw2$fert_voucher)] <- 0
mlw2$bean_voucher[is.na(mlw2$bean_voucher)] <- 0
frq(mlw2$maize_voucher)
frq(mlw2$fert_voucher)
frq(mlw2$bean_voucher)
var_label(mlw2$maize_voucher) <- "HH had obtained voucher for maize seed"
var_label(mlw2$fert_voucher) <- "HH had obtained voucher for inorganic fertilizer"
var_label(mlw2$bean_voucher) <- "HH had obtained voucher for bean seeds"
remove(coupon)

## Location ----
location <- read_dta("data/mlw/hh_mod_a_filt_13.dta") |> 
  select(y2_hhid,  region)
mlw2 <- left_join(mlw2, location, by=c("hhid"="y2_hhid"))
rm(location)

## HH head gender ----
head <- read_dta("data/mlw/hh_mod_b_13.dta") |> 
  select(y2_hhid, hh_b03, hh_b04 ) |> 
  filter(hh_b04==1) # keep only head
frq(head$hh_b04)
frq(head$hh_b03)
head <- head |> 
  mutate(female_head=if_else(hh_b03==2,1,0)) |> 
  select(y2_hhid, female_head)
mlw2 <- left_join(mlw2, head, by=c("hhid"="y2_hhid"))
rm(head)

## Other crops produced -----
garden <- read_dta("data/mlw/ag_mod_d_13.dta") |>   # plot level 
  select(y2_hhid, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e) |> 
  filter(ag_d14==1)
look_for(garden)
garden <- semi_join(garden, maizehh, by=c("y2_hhid"="hh_id_merge"))
garden <- garden |> 
  rowwise() |> 
  mutate(
    other_crops_count = sum(
      !is.na(c_across(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e))) & 
        !(c_across(ag_d20a:ag_d20e) %in% c(1, 11:16, 27, 34:36, 46))  # not maize or legumes
    )
  ) %>%
  ungroup()
garden <- garden |> 
  group_by(y2_hhid) |> 
  summarise(other_crops=sum(other_crops_count)) |> 
  ungroup()
mlw2 <- left_join(mlw2, garden, by=c("hhid"="y2_hhid"))
frq(mlw2$other_crops)
var_label(mlw2$other_crops) <- "number of other crops produced"
rm(maizehh, garden)

## Missing data and notes ----
## shocks missing for 13 hhs. 



