# mlw 4 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)

# In 2019, 3,178 households stemmed from 2,368 of the 2016 households representing 
# a household-level attrition rate of 5.6 percent.
# Individuals split from a household were followed. Who gets the hhid? - 
# Check page 4 in HP https://hdl.handle.net/10986/42446 
# hh_id_obs from the HP data is the key for panel identifier later. 

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country == "Malawi", wave == 4) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop == "MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata|>
  filter(main_crop == "MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs) |>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, hh_id_obs, ea_id_merge, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved,organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, elevation, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock, flood_shock, irrigated,  nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) # many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_19.dta") |> # plot level 
  select(y4_hhid, gardenid, plotid, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth) |> 
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-"))
look_for(garden)
garden <- semi_join(garden, maizeplots, by="plot_id")
frq(garden$ag_d14) # many plots are rented out or fallow
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
garden <- garden |> 
  group_by(y4_hhid) |>
  mutate(count_plot=n()) |>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in rare cases other corps are listed first, 
# but the kept obs are those with maize as the highest value crop
table(garden$ag_d20_oth)  # 113 cowpeas
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COW PEAS"))
  ))

# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_19.dta") |> 
  select(y4_hhid, gardenid, plotid, ag_d69, ag_d69_oth, ag_d70, ag_d70_oth) |> 
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-")) |> 
  select( -y4_hhid,-gardenid, -plotid)
modd <- semi_join(modd, maizeplots, by="plot_id")
table(modd$ag_d70_oth) # 1 beans
modd <- modd |> 
  mutate(rotation=as.integer(
    if_any(c(ag_d69, ag_d69_oth, ag_d70, ag_d70_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "LABLAB BEANS"))
  ))
look_for(garden)
plot4 <- left_join(garden, modd, by="plot_id") |> 
  select(-ag_d20a, -ag_d20b, -ag_d20c, -ag_d20d, -ag_d20e, -ag_d20_oth, -ag_d69, -ag_d69_oth, -ag_d70, -ag_d70_oth)
remove(garden,modd)
var_label(plot4$intercrop) <- "legume intercropped maize plot"
var_label(plot4$rotation) <- "legume is cultivated before or after"
var_label(plot4$count_plot) <- "number of main maize plots"

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <- read_dta("data/mlw/ag_mod_d_19.dta") |> 
  select(y4_hhid, gardenid, plotid, ag_d01) |>
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_19.dta") |> 
  select(y4_hhid, id_code, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd, individual, by=c("y4_hhid", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b05a )
plot4 <- left_join(plot4, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b05a)
remove(modd)

## Asset ownership ---- 
## no data available on owners

## Education ----
## variable of interest: if HH has an educated adult female
individual <- read_dta("data/mlw/hh_mod_b_19.dta") |> 
  select(y4_hhid, id_code, hh_b03, hh_b04, hh_b05a, hh_b21) |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  filter(female==1) |> 
  filter(hh_b05a>17)
individual <- semi_join(individual, maizehh, by=c("y4_hhid" = "hh_id_merge"))
frq(individual$hh_b21) # pscl - primary school; junior certificate of edu; Malawi school certificate
look_for(individual)
individual$hh_b21 <- as.numeric(individual$hh_b21)
individual <- collap(individual, hh_b21 ~ y4_hhid, FUN = fmax, na.rm = TRUE )
frq(individual$hh_b21) # pscl - primary school; junior certificate of edu; Malawi school certificate
individual <- individual |> 
  mutate(female_edu=ifelse(hh_b21==1,0,1)) |> 
  select(-hh_b21)
frq(individual$female_edu)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds4|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds4) # 246 missing for share_kg_sold

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot4 <- left_join(plot4, hpdata, by="plot_id")
remove(hpdata)
look_for(plot4)
plot4 <- plot4 |> rename(hhid=y4_hhid)
plot4 <- plot4 |> relocate(ea_id_merge,.after = hhid)
table(plot4$female_manager, plot4$female) # same information.
plot4 <- plot4 |> select(-female)
# manager age is missing for 15 plots.

## Collapse / aggregate ----
mlw4 <- plot4 |> 
  select(-ag_d14, -hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(mlw4)
mlw4basics <- mlw4 |> 
  select(hhid, ea_id_merge, hh_id_obs) |> 
  distinct()
wave4mean <- collap(mlw4,
                    farm_size + age_manager + elevation + plot_dist_household + ag_asset_index + count_plot + 
                      soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(mlw4,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "elevation", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
mlw4 <- mlw4 %>% select(-all_of(aggregated_vars))
look_for(mlw4)
mlw4 <- mlw4 |> 
  select(1, 6:7, 12:23)
look_for(mlw4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
mlw4 <- mlw4 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave4max <- collap(mlw4,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
mlw4 <- left_join(mlw4basics, wave4mean, by=join_by("hhid"))
mlw4 <- left_join(mlw4, wave4sum, by=join_by("hhid"))
mlw4 <- left_join(mlw4, wave4max, by=join_by("hhid"))
mlw4 <- left_join(mlw4, hdds4, by=c("hhid"="hh_id_merge"))
mlw4 <- left_join(mlw4, individual, by=c("hhid"="y4_hhid"))
mlw4 <- mlw4 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(mlw4basics, wave4max, wave4sum, wave4mean,aggregated_vars, hdds4, plot4, individual)
mlw4 <- mlw4 |> mutate(wave="wave4")

## Cleaning missing data ----
look_for(mlw4)
mlw4 <- mlw4 |> rename(ea_id=ea_id_merge)
### share_kg_sold----
### ## share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  We will capture subsistence orientation: HHs do not sell any crops. 
sales <- read_dta("data/mlw/ag_mod_i_19.dta") |> 
  select(y4_hhid, crop_code, ag_i01)
sales <- semi_join(sales, maizehh, by=c("y4_hhid"="hh_id_merge"))
look_for(sales)
frq(sales$ag_i01)
sales <- sales |> mutate(subsistence_ag=if_else(ag_i01==2,1,0))
sales <- collap(sales, subsistence_ag~y4_hhid, FUN = fmin, na.rm = TRUE) # if some consider for sale then not a subsistence. 
sales <- sales |> rename(hhid=y4_hhid)
mlw4 <- left_join(mlw4, sales, by="hhid")
frq(mlw4$subsistence_ag) # 22 still missing. 
remove(sales)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###
## Distance ----
community <- read_dta("data/mlw/com_cd_19.dta") |> 
  select(ea_id, com_cd18a , com_cd18b)
table(community$com_cd18b)
val_labels(community$com_cd18b) # 1 meter 2 km
community <- community |> 
  mutate(market_distance=case_when(com_cd18b==1 ~ com_cd18a/1000, # in km
                                   .default = com_cd18a)) |> 
  select(ea_id, market_distance)
community %>%
  count(ea_id) %>%
  filter(n > 1) # all unique copy 

## Non-farm income ----
wage <- read_dta("data/mlw/hh_mod_e_19.dta")
look_for(wage, details = c("none"))
wage <- wage[,c(1, 50:53, 73:79, 82:84, 114:120, 123:125, 143:146)]
wage <- semi_join(wage, maizehh, by=c("y4_hhid"="hh_id_merge"))
look_for(wage, details = c("none"))
mainwage <- wage[,1:15]
mainwage <- mainwage |> filter(!is.na(hh_e19b))
frq(mainwage$hh_e26b)
mainwage <- mainwage %>%
  mutate(hh_e26a = replace(hh_e26a, hh_e26a > 12 & hh_e26b == 5, 1))  # some wrote 30 (probably  meant days)
mainwage <- mainwage |> 
  mutate(main_income=case_when(hh_e26b==5 ~ hh_e25 * hh_e22/hh_e26a,
                          hh_e26b==4 ~ hh_e25 * hh_e22 * hh_e23 /hh_e26a,
                          hh_e26b==3 ~ hh_e25 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e26a,
                          .default = NA_real_)) |>  # very rough calculation 
  mutate(main_income=as.numeric(main_income))
# two cases with inf. One is waiter , the other is driver. probably indicated total income generated. 
mainwage <- mainwage |>
  filter(!is.na(main_income)) |> 
  mutate(main_income=case_when(hh_e26a==0 ~ hh_e25,
                          .default = main_income)) # one got money but put month to 0
look_for(mainwage)
frq(mainwage$hh_e28b)
mainwage <- mainwage |> 
  mutate(allowance=case_when(hh_e28b==5 ~ hh_e27 * hh_e22/hh_e28a,
                             hh_e28b==4 ~ hh_e27 * hh_e22 * hh_e23 /hh_e28a,
                             hh_e28b==3 ~ hh_e27 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e28a,
                             .default = NA_real_)) |>  # very rough calculation 
  mutate(allowance=as.numeric(allowance))
mainwage <- mainwage |>  mutate(income=rowSums(across(c(main_income,allowance)), na.rm=TRUE))
mainwage <- mainwage |> select(y4_hhid, income)
look_for(wage, details = c("none"))
#secondwage
secondwage <- wage[,c(1,16:25)]
look_for(secondwage)
secondwage <- secondwage |> filter(!is.na(hh_e36))
frq(secondwage$hh_e40b)
secondwage <- secondwage |>
  mutate(hh_e40a=as.numeric(hh_e40a)) |> 
  mutate(main_income=hh_e39 * hh_e36/hh_e40a) |>  # only 5 obs (months) 
  mutate(main_income=as.numeric(main_income))
frq(secondwage$hh_e42b) # no allowance
secondwage <- secondwage |> 
  rename(income=main_income) |> 
  select(y4_hhid, income)
look_for(wage, details = c("none"))
#ganyu
ganyu <- wage[,c(1,26:29)]
look_for(ganyu)
ganyu <- ganyu |> filter(!is.na(hh_e56))
ganyu <- ganyu |> 
  mutate(income=hh_e56*hh_e57*hh_e58*hh_e59)
ganyu <- collap(ganyu,income~y4_hhid, FUN=fsum)
wage <- rbind(ganyu, mainwage, secondwage)
remove(ganyu, mainwage, secondwage)
wage <- collap(wage,income~y4_hhid, FUN=fsum)
mlw4 <- left_join(mlw4, wage, by=c("hhid"="y4_hhid"))
mlw4 <- mlw4 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(mlw4$wage_income) <- "income from main and temporary off-farm work"
var_label(mlw4$subsistence_ag) <- "HH did not sell any crop harvest"
var_label(mlw4$plot_area_GPS) <- "Area (ha) of maize plots"
var_label(mlw4$nb_seasonal_crop) <- "Number of seasonal crops grown on maize plots"
var_label(mlw4$female_edu) <- "HH has an educated adult female"
remove(wage)
# enterprise
enterprise <- read_dta("data/mlw/hh_mod_n2_19.dta")
look_for(enterprise, details=c("none"))
enterprise <- semi_join(enterprise,  maizehh, by=c("y4_hhid"="hh_id_merge"))
# It is not possible to calculate monthly profits from the enterprise.
# We will take the last month's profit. 
# Enterprise data is better calculated for the last month's profit. An annual income cannot be precisely calculated.
# Thus, to sum all other income, we must consider average monthly income for the wage income as well 
# (unlike in ETH where yearly is calculated). 
enterprise <- enterprise |> 
  filter(!is.na(hh_n40)) |> 
  rename(enterprise_income=hh_n40)
enterprise <- collap(enterprise, enterprise_income ~ y4_hhid, FUN=fsum)   
# other income
other_income <- read_dta("data/mlw/hh_mod_p_19.dta") |> 
  filter(hh_p01==1)
other_income <- semi_join(other_income,  maizehh, by=c("y4_hhid"="hh_id_merge"))
look_for(other_income, details = c("none"))
frq(other_income$hh_p03_2) # rands
table(other_income$hh_p03_2oth) # malawian kwacha botswana pula
other_income <- other_income |> #many no currency assume local
  mutate(int_transfer=case_when(hh_p03_2==4 ~ hh_p03c* 50.86, # Average exchange rate in 2019: 50.86 MWK.
                                hh_p03_2==6 & hh_p03_2oth=="MALAWIAN KWACHA" ~ hh_p03c,
                                hh_p03_2==6 & hh_p03_2oth=="BOTSWANA PULA" ~ hh_p03c * 69,
                                .default = hh_p03c)) |>  
  tidyr::replace_na(list(int_transfer = 0)) |> 
  mutate(other_income=case_when(hh_p0a %in% c(101, 102, 103) ~ (int_transfer + hh_p03a + hh_p03b),
                                hh_p0a>103 ~ hh_p02,
                                .default = 0))
other_income <- collap(other_income, other_income ~ y4_hhid, FUN=fsum)
# other income is yearly,
# wage_income is yearly,
# enterprise is but last month. 
mlw4 <- left_join(mlw4, other_income, by=c("hhid"="y4_hhid"))
mlw4 <- mlw4 |> 
  tidyr::replace_na(list(other_income = 0))
mlw4 <- left_join(mlw4, enterprise, by=c("hhid"="y4_hhid"))
mlw4 <- mlw4 |> 
  tidyr::replace_na(list(enterprise_income = 0))
mlw4 <- mlw4 |> 
  mutate(other_income=other_income / 12,
         wage_income=wage_income / 12,
         tot_other_income=wage_income+other_income+enterprise_income)
var_label(mlw4$tot_other_income) <- "Average monthly total other income"
mlw4 <- left_join(mlw4, community, by="ea_id")
remove(enterprise,other_income, community)
look_for(mlw4)
# market_distance is still missing for half of the observations. No data on market distance can be used. 
mlw4 <- mlw4 |> select(-market_distance)

##  Extension, advisory, credit----
# We will consider most important sources like government and private ag. extension.
extension <- read_dta("data/mlw/ag_mod_t1_19.dta") |> 
  select(y4_hhid, ag_t0a, ag_t01, ag_t01_oth, ag_t02) |> 
  filter(ag_t01==1)
frq(extension$ag_t0a)
frq(extension$ag_t02)
# Many report to have received extension from electronic media like tv and radio and other farmers. 
# We will consider those extension services that have been provided by gov, private and cooperative type institutions. 
extension <- extension |> 
  mutate(ag_t0a=case_when(
    ag_t01_oth=="CHICKEN PRODUCTION" ~ 312,
    .default=ag_t0a))
extension <- extension |> 
  mutate(extension_service=case_when(ag_t0a %in% c(301:307, 309, 314:316) &
                                       ag_t02 %in% c(1:10) ~ 1,
                                     .default = 0))
extension <- collap(extension, extension_service~y4_hhid, FUN = fmax)
frq(extension$extension_service)
mlw4 <- left_join(mlw4, extension, by=c("hhid"="y4_hhid"))
mlw4$extension_service[is.na(mlw4$extension_service)] <- 0
var_label(mlw4$extension_service) <- "received extension from institutions" 
remove(extension)

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/mlw/ag_mod_r1_19.dta") |>
  select(y4_hhid, ag_r00, ag_r0a, ag_r01, ag_r01_oth, ag_r02) |> 
  filter(ag_r01==1)
frq(tlu$ag_r0a)
tlu <- tlu |> 
  mutate(tlu=case_when(ag_r0a %in% c(301:304, 3304) ~ ag_r02 * 0.7,
                       ag_r0a %in% c(307:308) ~ ag_r02 * 0.1,
                       ag_r0a==309 ~ ag_r02 * 0.2,
                       ag_r0a %in% c(311:315, 319, 3310, 3314) ~ ag_r02 * 0.01,
                       ag_r0a==3305 ~ ag_r02 * 0.7, 
                       .default = 0))
tlu <- collap(tlu, tlu ~ y4_hhid, FUN = fsum)
mlw4 <- left_join(mlw4, tlu, by=c("hhid"="y4_hhid"))
remove(tlu, maizehh)

## Land tenure - not collected ----

### ### ### ### ### ### ### ###
## Soil type ---- 
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_19.dta") |> 
  select(y4_hhid, gardenid, plotid, ag_d01, ag_d21) |>
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
look_for(modd)
frq(modd$ag_d21)
labels <- attributes(modd$ag_d21)
modd$ag_d21 <- as.numeric(modd$ag_d21)
modd <- collap(modd, ag_d21 ~ y4_hhid, FUN = fmode, na.rm= TRUE)
attributes(modd$ag_d21) <- labels
mlw4 <- left_join(mlw4, modd, by=c("hhid"="y4_hhid"))
mlw4 <- mlw4 |> 
  rename(soil_type=ag_d21)
var_label(mlw4$soil_type) <- "HH dominant soil type"
remove(modd, maizeplots, labels)



















