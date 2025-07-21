# mlw 3 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)

# identify panel households
hh16 <- read_dta("data/mlw/hh_mod_a_filt_16.dta") |> 
  select(y3_hhid, y2_hhid, ea_id,) |> 
  distinct(y3_hhid, .keep_all = TRUE) |> 
  rename(hhid=y3_hhid)

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==3) 
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
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved, organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, elevation, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock, flood_shock, irrigated, nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) # many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_16.dta") |> # plot level 
  select(y3_hhid, gardenid, plotid, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth) |> 
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-"))
look_for(garden)
garden <- semi_join(garden, maizeplots, by="plot_id")
table(garden$ag_d14) # many plots are rented out or fallow
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
garden <- garden |> 
  group_by(y3_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in rare cases other corps are listed first, 
# but the kept obs are those with maize as the highest value crop
table(garden$ag_d20_oth)  # 38 cowpeas
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COWPEAS"))
  ))

# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

## improved maize  ----
seed <- read_dta("data/mlw/ag_mod_g_16.dta") |> 
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  filter(ag_g0a==1) |> 
  select(y3_hhid, plot_id, crop_code, ag_g0e, local_vs_improved, ag_g0f)
seed <- semi_join(seed, maizeplots, by="plot_id")
frq(seed$crop_code)
seed <- seed |> filter(crop_code %in% c(1:4))
frq(seed$ag_g0f)
frq(seed$local_vs_improved) # what is this local vs improved?
#-------------------------------------------------# 
# there is a mismatch between crop code and ag_g0f;
# crop code reports 1350 hybrid while ag_g0f reports 1421 improved. 
# will refer  to ag_g0f as this was a separate question.
seed <- seed |> 
  mutate(hybrid_maize= case_when(ag_g0f==2 ~ 1,
                                 .default = 0)) |> 
  select(plot_id, hybrid_maize)
frq(seed$hybrid_maize)
garden <- left_join(garden, seed, by= "plot_id")
frq(garden$hybrid_maize)
garden$hybrid_maize[is.na(garden$hybrid_maize)] <- 0
remove(seed)

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d69, ag_d69_oth, ag_d70, ag_d70_oth) |> 
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  select( -y3_hhid,-gardenid, -plotid)
modd <- semi_join(modd, maizeplots, by="plot_id")
table(modd$ag_d70_oth) # 16 COWPEAS 
modd <- modd |> 
  mutate(rotation=as.integer(
    if_any(c(ag_d69, ag_d69_oth, ag_d70, ag_d70_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COWPEAS"))
  ))
look_for(garden)
plot3 <- left_join(garden, modd, by="plot_id") |> 
  select(-ag_d20a, -ag_d20b, -ag_d20c, -ag_d20d, -ag_d20e, -ag_d20_oth, -ag_d69, -ag_d69_oth, -ag_d70, -ag_d70_oth)
remove(garden,modd)
var_label(plot3$intercrop) <- "legume intercropped maize plot"
var_label(plot3$rotation) <- "legume is cultivated before or after"
var_label(plot3$count_plot) <- "number of main maize plots"

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d01) |>
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_16.dta") |> 
  select(y3_hhid, id_code, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd,individual, by=c("y3_hhid", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b05a )
plot3 <- left_join(plot3, modd, by="plot_id")
plot3 <- plot3 |> rename(age_manager_row=hh_b05a)
remove(modd)

## Asset ownership ---- 
## no data available on owners

## Education ----
## variable of interest: if HH has an educated adult female
individual <- read_dta("data/mlw/hh_mod_b_16.dta") |> 
  select(y3_hhid, id_code, hh_b03, hh_b04, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  filter(female==1) |> 
  filter(hh_b05a>17)
individual <- semi_join(individual, maizehh, by=c("y3_hhid" = "hh_id_merge"))
edu <- read_dta("data/mlw/hh_mod_c_16.dta") |> 
  select(y3_hhid, id_code, hh_c08 )
individual <- left_join(individual, edu, by=c("y3_hhid", "id_code"))
individual$hh_c08 <- as.numeric(individual$hh_c08)
individual <- individual |> 
  filter(!is.na(hh_c08)) |> 
  mutate(female_edu=ifelse(hh_c08 > 7,1,0)) |> # =1 if hh has educated female with at least primary education completed. 
  select(y3_hhid, female_edu)
individual <- collap(individual, female_edu ~ y3_hhid, FUN = fmax)
frq(individual$female_edu)
remove(edu)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds3 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==3) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds3 <- hdds3|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds3) # 297 missing for share_kg_sold

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot3 <- left_join(plot3, hpdata, by="plot_id")
remove(hpdata)
look_for(plot3)
plot3 <- plot3 |> rename(hhid=y3_hhid)
plot3 <- left_join(plot3, hh16, by="hhid")
plot3 <- plot3 |> relocate(ea_id,.after = hhid)
table(plot3$female_manager, plot3$female) # same information.
plot3 <- plot3 |> select(-female)
# manager age is missing for 102 plots.


## Collapse / aggregate ----
mlw3 <- plot3 |> 
  select(-ag_d14, -hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(mlw3)
mlw3basics <- mlw3 |> 
  select(hhid, hh_id_obs, ea_id) |> 
  distinct()
n_distinct(mlw3basics$hh_id_obs) # 1707 observations
wave3mean <- collap(mlw3,
                    farm_size + age_manager + elevation + plot_dist_household + ag_asset_index + count_plot + 
                    soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave3sum <- collap(mlw3,
                   plot_area_GPS + nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "elevation", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
mlw3 <- mlw3 %>% select(-all_of(aggregated_vars))
look_for(mlw3)
mlw3 <- mlw3 |> 
  select(1, 6:8, 13:24)
look_for(mlw3)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
mlw3 <- mlw3 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave3max <- collap(mlw3,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
mlw3 <- left_join(mlw3basics, wave3mean, by=join_by("hhid"))
mlw3 <- left_join(mlw3, wave3sum, by=join_by("hhid"))
mlw3 <- left_join(mlw3, wave3max, by=join_by("hhid"))
mlw3 <- left_join(mlw3, hdds3, by=c("hhid"="hh_id_merge"))
mlw3<- left_join(mlw3, individual, by=c("hhid"="y3_hhid"))
mlw3 <- mlw3 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(mlw3basics, wave3max, wave3sum,wave3mean,aggregated_vars, hdds3, plot3, individual)
mlw3 <- mlw3 |> mutate(wave="wave3")

## Cleaning missing data ----
look_for(mlw3)

### share_kg_sold----
### ## share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  We will capture subsistence orientation: HHs do not sell any crops. 
sales <- read_dta("data/mlw/ag_mod_i_16.dta") |> 
  select(y3_hhid, crop_code, ag_i01)
sales <- semi_join(sales, maizehh, by=c("y3_hhid"="hh_id_merge"))
look_for(sales)
frq(sales$ag_i01)
sales <- sales |> mutate(subsistence_ag=if_else(ag_i01==2,1,0))
sales <- collap(sales, subsistence_ag~y3_hhid, FUN = fmin, na.rm = TRUE) # if some consider for sale then not a subsistence. 
sales <- sales |> rename(hhid=y3_hhid)
mlw3 <- left_join(mlw3, sales, by="hhid")
frq(mlw3$subsistence_ag) # 15 still missing. no harvest? 
remove(sales)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###
## Distance ----
community <- read_dta("data/mlw/com_cd_16.dta") |> 
  select(ea_id, com_cd18a , com_cd18b)
table(community$com_cd18b)
val_labels(community$com_cd18b) # 1 meter 2 km
community <- community |> 
  mutate(market_distance=case_when(com_cd18b==1 ~ com_cd18a/1000,
                                   .default = com_cd18a)) |> 
  select(ea_id, market_distance)
community %>%
  count(ea_id) %>%
  filter(n > 1) # one copy 
community <- community |> distinct(ea_id,.keep_all = TRUE)

## Non-farm income ----
wage <- read_dta("data/mlw/hh_mod_e_16.dta")
look_for(wage, details = c("none"))
wage <- wage[,c(1, 56:60, 66:72, 94:100,103:110, 123:126)]
wage <- semi_join(wage, maizehh, by=c("y3_hhid"="hh_id_merge"))
look_for(wage, details = c("none"))
mainwage <- wage[,1:13]
mainwage <- mainwage |> filter(!is.na(hh_e19b))
frq(mainwage$hh_e26b)
mainwage <- mainwage %>%
  mutate(hh_e26a = replace(hh_e26a, hh_e26a > 12 & hh_e26b == 5, 1)) |>  # some wrote 30 (probably  meant days)
  mutate(hh_e26a = replace(hh_e26a, hh_e26a > 31, 1)) # there is one with 4500. error repeated from salary 
mainwage <- mainwage |> 
  mutate(income=case_when(hh_e26b==5 ~ hh_e25 * hh_e22/hh_e26a,
                          hh_e26b==4 ~ hh_e25 * hh_e22 * hh_e23 /hh_e26a,
                          hh_e26b==3 ~ hh_e25 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e26a,
                          .default = NA_real_)) |>  # very rough calculation 
  mutate(income=as.numeric(income))
# two cases with inf. One is waiter , the other is driver. probably indicated total income generated. 
mainwage <- mainwage |>
  filter(!is.na(income)) |> 
  mutate(income=case_when(hh_e26a==0 ~ hh_e25,
                          .default = income))
look_for(mainwage)
mainwage <- mainwage |> select(y3_hhid, income)
look_for(wage, details = c("none"))
# secondwage
secondwage <- wage[,c(1,14:28)]
look_for(secondwage)
secondwage <- secondwage |> filter(!is.na(hh_e36))
frq(secondwage$hh_e40b)
secondwage <- secondwage |> 
  mutate(main_income=case_when(hh_e40b==5 ~ hh_e39 * hh_e36/hh_e40a,
                          hh_e40b==4 ~ hh_e39 * hh_e36 * hh_e37/hh_e40a,
                          hh_e40b==3 ~ hh_e39 * (hh_e36 * hh_e37 * hh_e38/8) / hh_e40a,
                          .default = NA_real_)) |>  # very rough calculation 
  mutate(main_income=as.numeric(main_income))
secondwage <- secondwage |> 
  mutate(allowance=case_when(hh_e42b==5 ~ hh_e41 * hh_e36/hh_e42a,
                          .default = NA_real_)) |>  # very rough calculation 
  mutate(allowance=as.numeric(allowance))
secondwage <- secondwage |>  mutate(income=rowSums(across(c(main_income,allowance)), na.rm=TRUE))
secondwage <- secondwage |> select(y3_hhid, income)
look_for(wage, details = c("none"))
ganyu <- wage[,c(1,29:32)]
look_for(ganyu)
ganyu <- ganyu |> filter(!is.na(hh_e56))
# Ganyu typically involves agricultural tasks such as weeding, planting, harvesting, and etc.It can also
# include non-agricultural work like making bricks, digging wells, or repairing fences
ganyu <- ganyu |> 
  mutate(income=hh_e56*hh_e57*hh_e58*hh_e59)
ganyu <- collap(ganyu,income~y3_hhid, FUN=fsum)
wage <- rbind(ganyu, mainwage, secondwage)
remove(ganyu, mainwage, secondwage)
wage <- collap(wage,income~y3_hhid, FUN=fsum)
mlw3 <- left_join(mlw3, wage, by=c("hhid"="y3_hhid"))
mlw3 <- mlw3 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(mlw3$wage_income) <- "income from main and temporary off-farm work"
var_label(mlw3$subsistence_ag) <- "HH did not sell any crop harvest"
var_label(mlw3$plot_area_GPS) <- "Area (ha) of maize plots"
var_label(mlw3$nb_seasonal_crop) <- "Number of seasonal crops grown on maize plots"
var_label(mlw3$female_edu) <- "HH has an educated adult female"
var_label(mlw3$hybrid_maize) <- "Improved maize adoption"
remove(wage)
# enterprise
enterprise <- read_dta("data/mlw/hh_mod_n2_16.dta")
look_for(enterprise, details=c("none"))
enterprise <- enterprise[,c(1,35:59, 107:123)]
look_for(enterprise, details = c("none"))
enterprise <- semi_join(enterprise,  maizehh, by=c("y3_hhid"="hh_id_merge"))
# It is not possible to calculate monthly profits from the enterprise.
# We will take the last month's profit. 
# Enterprise data is better calculated for the last month's profit. An annual income cannot be precisely calculated.
# Thus, to sum all other income, we must consider average monthly income for the wage income as well 
# (unlike in ETH where yearly is calculated). 
enterprise <- enterprise |> 
  filter(!is.na(hh_n40)) |> 
  rename(enterprise_income=hh_n40)
enterprise <- collap(enterprise, enterprise_income ~ y3_hhid, FUN=fsum) 
# other income
other_income <- read_dta("data/mlw/hh_mod_p_16.dta") |> 
  filter(hh_p01==1)
other_income <- semi_join(other_income,  maizehh, by=c("y3_hhid"="hh_id_merge"))
look_for(other_income, details = c("none"))
frq(other_income$hh_p03_2) # dollar and etc
table(other_income$hh_p03_2oth) # malawian kwacha 
other_income <- other_income |>
  mutate(int_transfer=case_when(hh_p03_2==1 ~ hh_p03c* 710.1497, # Average exchange rate in 2016: 710.1497 MW
                                hh_p03_2==4 ~ hh_p03c* 48.48, # Average exchange rate in 2016: 48.48 MWK.
                                hh_p03_2==6 ~ hh_p03c,
                                .default = hh_p03c)) |>
  tidyr::replace_na(list(int_transfer = 0)) |> 
  mutate(other_income=case_when(hh_p0a %in% c(101, 102, 103) ~ (int_transfer + hh_p03a + hh_p03b),
                                hh_p0a>103 ~ hh_p02,
                                .default = 0))
other_income <- collap(other_income, other_income ~ y3_hhid, FUN=fsum)
# other income is yearly,
# wage_income is yearly,
# enterprise is but last month. 
mlw3 <- left_join(mlw3, other_income, by=c("hhid"="y3_hhid"))
mlw3 <- mlw3 |> 
  tidyr::replace_na(list(other_income = 0))
mlw3 <- left_join(mlw3, enterprise, by=c("hhid"="y3_hhid"))
mlw3 <- mlw3 |> 
  tidyr::replace_na(list(enterprise_income = 0))
mlw3 <- mlw3 |> 
  mutate(other_income=other_income / 12,
         wage_income=wage_income / 12,
         tot_other_income=wage_income+other_income+enterprise_income)
var_label(mlw3$tot_other_income) <- "Average monthly total other income"
mlw3 <- left_join(mlw3, community, by="ea_id")
remove(enterprise,other_income, community)
look_for(mlw3)
# market_distance still missing for 605 observations. No data on market distance can be used?! decide later 

##  Extension, advisory, credit----
extension <- read_dta("data/mlw/ag_mod_t1_16.dta") |> 
  select(y3_hhid, ag_t0a, ag_t01)
frq(extension$ag_t01)
extension <- extension |>  
  mutate(extension_service= case_when(ag_t01==1 ~ 1,
                                      .default = 0)) 
extension <- collap(extension, extension_service~y3_hhid, FUN = fmax)
frq(extension$extension_service) # most have received some kind of extension
extension <- read_dta("data/mlw/ag_mod_t1_16.dta") |> 
  select(y3_hhid, ag_t0a, ag_t01, ag_t01_oth) |> 
  filter(ag_t01==1)
frq(extension$ag_t0a)
extension <- extension |> 
  mutate(ag_t0a=case_when(
    ag_t01_oth=="BEE KEEPING" ~ 311,
    .default=ag_t0a)) # ntayakhasu is means no new ridge
extension <- extension |> 
  mutate(extension_service=case_when(ag_t0a %in% c(301:307, 309, 314:316) ~ 1,
                             .default = 0))
extension <- collap(extension, extension_service~y3_hhid, FUN = fmax)
frq(extension$extension_service) # again, most have received relevant extension advice.
# We will consider most important sources like government and private ag. extension.
extension <- read_dta("data/mlw/ag_mod_t1_16.dta") |> 
  select(y3_hhid, ag_t0a, ag_t01, ag_t01_oth, ag_t02) |> 
  filter(ag_t01==1)
frq(extension$ag_t0a)
frq(extension$ag_t02)
# Many report to have received extension from electronic media like tv and radio and other farmers. 
# We will consider those extension services that have been provided by gov, private and cooperative type institutions. 
extension <- extension |> 
  mutate(ag_t0a=case_when(
    ag_t01_oth=="BEE KEEPING" ~ 311,
    .default=ag_t0a)) # ntayakhasu is means no new ridge
extension <- extension |> 
  mutate(extension_service=case_when(ag_t0a %in% c(301:307, 309, 314:316) &
                                      ag_t02 %in% c(1:10) ~ 1,
                                     .default = 0))
extension <- collap(extension, extension_service~y3_hhid, FUN = fmax)
frq(extension$extension_service)
mlw3 <- left_join(mlw3, extension, by=c("hhid"="y3_hhid"))
mlw3$extension_service[is.na(mlw3$extension_service)] <- 0
var_label(mlw3$extension_service) <- "received extension from institutions" 
remove(extension)

### Credit ----
credit <- read_dta("data/mlw/hh_mod_s2_16.dta") |> 
  select(y3_hhid, hh_s12, hh_s19a, hh_s19b)
credit <- credit |> 
  mutate(no_credit_access=case_when(
    hh_s19a==7 | hh_s19b==7 ~ 1,
    !is.na(hh_s19a) | !is.na(hh_s19b) ~ 0,
    .default = NA_integer_)) # HH does not know any lender
credit <- credit |> 
  filter(!is.na(no_credit_access)) |> 
  select(y3_hhid, no_credit_access)
mlw3 <- left_join(mlw3, credit, by=c("hhid"="y3_hhid"))
mlw3$no_credit_access[is.na(mlw3$no_credit_access)] <- 0
frq(mlw3$no_credit_access)
var_label(mlw3$no_credit_access) <- "HH does not know any lender"
remove(credit)

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/mlw/ag_mod_r1_16.dta") |>
  select(y3_hhid, ag_r00, ag_r0a, ag_r01, ag_r01_oth, ag_r02) |> 
  filter(ag_r01==1)
frq(tlu$ag_r0a)
tlu <- tlu |> 
  mutate(tlu=case_when(ag_r0a %in% c(301:304, 3304) ~ ag_r02 * 0.7,
                       ag_r0a %in% c(307:308) ~ ag_r02 * 0.1,
                       ag_r0a==309 ~ ag_r02 * 0.2,
                       ag_r0a %in% c(311:315, 319, 3310, 3314) ~ ag_r02 * 0.01,
                       ag_r0a==3305 ~ ag_r02 * 0.7, 
                       .default = 0))
tlu <- collap(tlu, tlu ~ y3_hhid, FUN = fsum)
mlw3 <- left_join(mlw3, tlu, by=c("hhid"="y3_hhid"))
mlw3$tlu[is.na(mlw3$tlu)] <- 0
remove(tlu)

## Land tenure ----
tenure <- read_dta("data/mlw/ag_mod_v_16.dta") 
frq(tenure$ag_v0a)
# some information is reported once from one parcel and other parcels are missing. 
tenure <- tenure |> 
  mutate(parcel_owned=case_when(ag_v0a==1~1,
                                .default = 0)) |> 
  select(y3_hhid, parcel_owned, ag_v02_1, ag_v11, ag_v14) 
frq(tenure$ag_v11) # when there are recent disputes, hh is more likely to expect disagreement over ownership in the future too.
frq(tenure$ag_v14)
# Interestingly, many who do not expect to have any disagreements in the next 5 years, are not so sure
# about the possession of the land in 10 years. This is an indicator of long term perspectives and expectations
ownership <- collap(tenure, parcel_owned~y3_hhid, FUN = fmax)
frq(ownership$parcel_owned) # 254 hhs with no land owned
frq(tenure$ag_v11)
frq(tenure$ag_v14)
tenure <- tenure |>
  mutate(ag_v14=case_when(is.na(ag_v14) & ag_v11 >1 ~ 2,
                          is.na(ag_v14) & ag_v11 ==1 ~ 5,
                          .default = ag_v14)) |> 
         # if not sure about disagreement also unsure about ownership in 10 years
  mutate(tenure_security=(case_when(ag_v14 %in% c(3:5) ~ 1,
                                    .default = 0)))
tenure <- collap(tenure, tenure_security~y3_hhid, FUN = fmax)
var_label(tenure$tenure_security) <- "very likely to still possess land in ten years"
mlw3 <- left_join(mlw3, ownership, by=c("hhid"="y3_hhid"))
mlw3 <- left_join(mlw3, tenure, by=c("hhid"="y3_hhid"))
remove(tenure, ownership,hh16, maizehh)

## Soil type ---- 
modd <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d01, ag_d21) |>
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
look_for(modd)
frq(modd$ag_d21)
modd <- modd |> 
  mutate(ag_d21 = as.numeric(modd$ag_d21),
         soil_type=case_when(ag_d21==3 ~ 2,
                             ag_d21==1 ~ 1,
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
modd <- collap(modd, soil_type ~ y3_hhid, FUN =mode_or_max) # select dominant otherwise clay 
modd <- modd |> 
  mutate(soil= factor(soil_type, levels = c(0,1,2), labels = c("Other or mix", "Sandy", "Clay"))) |> 
  select(y3_hhid, soil)
mlw3 <- left_join(mlw3, modd, by=c("hhid"="y3_hhid"))
var_label(mlw3$soil) <- "HH dominant soil type"
remove(modd, maizeplots, mode_or_max)

## Input coupon ---- 
coupon <- read_dta("data/mlw/ag_mod_e2_16.dta") |> 
  filter(ag_e01==1) |>  # obtained coupon
  select(y3_hhid, ag_e01_1, ag_e07)
frq(coupon$ag_e01_1)
frq(coupon$ag_e07)
look_for(coupon)
coupon <- coupon |>
  mutate(fert_voucher=if_else(as.integer(ag_e07) %in% c(1:6), 1, 0),
         maize_voucher=if_else(as.integer(ag_e07) %in% c(12, 15), 1, 0),
         bean_voucher=if_else(as.integer(ag_e07) %in% c(16:19), 1, 0) 
         )
coupon <- collap(coupon, maize_voucher + fert_voucher + bean_voucher ~ y3_hhid, FUN = fmax, na.rm=TRUE)
mlw3 <- left_join(mlw3, coupon, by=c("hhid"="y3_hhid"))
mlw3$maize_voucher[is.na(mlw3$maize_voucher)] <- 0
mlw3$fert_voucher[is.na(mlw3$fert_voucher)] <- 0
mlw3$bean_voucher[is.na(mlw3$bean_voucher)] <- 0
frq(mlw3$maize_voucher)
frq(mlw3$fert_voucher)
frq(mlw3$bean_voucher)
var_label(mlw3$maize_voucher) <- "HH had obtained voucher for maize seed"
var_label(mlw3$fert_voucher) <- "HH had obtained voucher for inorganic fertilizer"
var_label(mlw3$bean_voucher) <- "HH had obtained voucher for bean seeds"
remove(coupon)
 
## Missing data and notes ----
## shocks missing for 16 hhs












