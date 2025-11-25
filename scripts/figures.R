# Figures and tables ====
pacman::p_load(vcd,
               flextable,
               officer,
               tidyverse, 
               modelsummary,
               estimatr,
               gmodels,
               pandoc)
# Tables =====
## Table 1 - selected variables----
tb1 <- dfmlw3 |>
  select("legume_intens", "HDDS", "fcs", "female_manager", "age_manager", "hh_size", "extension_service", "farm_size", 
         "maize_share", "other_crops", "subsistence_ag", "iv_maize", "organic_fertilizer", "inorganic_fertilizer", "year" )
tb2 <- dfeth |>
  rename(extension_service=advisory_service) |> 
  select("legume_intens", "HDDS", "female_manager", "age_manager", "hh_size", "extension_service", "farm_size", 
         "maize_share", "other_crops", "subsistence_ag", "iv_maize", "organic_fertilizer", "inorganic_fertilizer", "year" )
tb3 <- dftza |> 
  select("legume_intens", "HDDS", "fcs", "female_manager", "age_manager", "hh_size", "extension_service", "farm_size", 
         "maize_share", "other_crops", "subsistence_ag", "iv_maize", "organic_fertilizer", "inorganic_fertilizer", "year" )
tb1$country <- "Malawi"
tb2$country <- "Ethiopia"
tb2$inorganic_fertilizer <- as.numeric(tb2$inorganic_fertilizer)
tb3$country <- "Tanzania"
dfs <-bind_rows(tb1, tb2)
dfs <-bind_rows(dfs, tb3)
look_for(dfs)
dfs <- dfs |> 
  mutate(country_year = factor(paste(country, year, sep = "-")))
tblall <- datasummary_balance(
  ~ country_year,
  data = dfs,
  title = "Descriptive statistics of the main variables",
  fmt = fmt_decimal(digits = 2),
  output = "flextable"
)
tblall
doc <- read_docx() %>%
  body_add_flextable(tblall)
print(doc, target = "outputs/results/summary_tablesall.docx")
rm(tblall, dfs, tb1, tb2, tb3)

## Table A4-A6 - remaining variables ----
tb1 <- dfmlw3 |> 
  select("primary_education_manager", "hh_dependency_ratio", "flat_slope", "mod_slope", "plot_dist_household",
         "hh_asset_index", "ln_other_income", "tlu", "credit_constrained", "fert_voucher", 
         "bean_voucher", "eco_soil_mng", "soil_fertility_index", "twi", "anntot_avg", "adopted_neighbors_10km", "year") |> 
  mutate(plot_dist_household=if_else(is.infinite(plot_dist_household), NA, plot_dist_household))
tbl1  <-  datasummary_balance(~year , tb1, title = "Malawi",
                              fmt = fmt_decimal(digits = 2),
                              output = "flextable")
tb2 <- dfeth |> 
  select("primary_education_manager", "hh_dependency_ratio", "flat_slope", "dist_market", "plot_dist_household", 
         "hh_asset_index", "ln_other_income", "tlu", "extension_program", "credit_unavailable", 
         "eco_soil_mng", "soil_fertility_index", "twi", "anntot_avg", "adopted_neighbors_10km", "year") |> 
  mutate(plot_dist_household=if_else(is.infinite(plot_dist_household), NA, plot_dist_household))
tbl2  <-  datasummary_balance(~year , tb2, title = "Ethiopia",
                              fmt = fmt_decimal(digits = 2),
                              output = "flextable")
tb3 <- dftza |> 
  select("primary_education_manager", "hh_dependency_ratio", "flat_slope", "plot_dist_household",
         "hh_asset_index", "ln_other_income", "tlu", "credit", "crop_residue_short", "crop_residue_long",
         "average_rainfall", "dist_market","adopted_neighbors_10km", "year")
tbl3  <-  datasummary_balance(~year , tb3, title = "Tanzania",
                              fmt = fmt_decimal(digits = 2),
                              output = "flextable")
set_table_properties(tbl1, layout = "autofit")
set_table_properties(tbl2, layout = "autofit")
set_table_properties(tbl3, layout = "autofit")
doc <- read_docx() %>%
  body_add_flextable(tbl1) %>%
  body_add_par(" ") |> 
  body_add_flextable(tbl2) |> 
  body_add_par(" ") |> 
  body_add_flextable(tbl3)

print(doc, target = "outputs/results/summary_tables.docx")
rm(tbl1, tbl2, tbl3)

## Table 2 - correlation between DM and legume intensification ----
# chi2 test and Cramers V
# Malawi
table_mlw <- table(dfmlw3$female_dm, dfmlw3$legume_intens)
chisq_result_mlw <- chisq.test(table_mlw)
cramers_v_mlw <- assocstats(table_mlw)$cramer

# Ethiopia
table_eth <- table(dfeth$female_dm, dfeth$legume_intens)
chisq_result_eth <- chisq.test(table_eth)
cramers_v_eth <- assocstats(table_eth)$cramer

# Tanzania
table_tza <- table(dftza$female_dm, dftza$legume_intens)
chisq_result_tza <- chisq.test(table_tza)
cramers_v_tza <- assocstats(table_tza)$cramer

format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

# Combine into one data frame with formatted values
results_table <- tibble::tibble(
  Country   = c("Malawi", "Ethiopia", "Tanzania"),
  Chi2      = c(chisq_result_mlw$statistic, chisq_result_eth$statistic, chisq_result_tza$statistic ),
  df        = c(chisq_result_mlw$parameter, chisq_result_eth$parameter, chisq_result_tza$parameter),
  p_value   = c(chisq_result_mlw$p.value, chisq_result_eth$p.value, chisq_result_tza$p.value),
  Cramers_V = c(cramers_v_mlw, cramers_v_eth, cramers_v_tza)
) |>
  mutate(
    Chi2      = round(Chi2, 3),
    p_value   = round(p_value, 3),
    Cramers_V = round(Cramers_V, 3),
    # combine APA style text
    Chi2_result  = paste0("χ²=", Chi2)
  ) |>
  select(Country, Chi2_result, p_value, Cramers_V)

results_table
ft <- flextable::flextable(results_table) |> 
  set_caption(caption = " DM & Legume int.")
rm(table_eth, table_mlw, table_tza, kruskal_result_mlw, kruskal_result_tza, kruskal_result_tza1,
   kruskal_result_mlw1, kruskal_result_eth, cramers_v_eth, cramers_v_mlw, cramers_v_tza,
   chisq_result_eth, chisq_result_mlw, chisq_result_tza, format_p)

# Contingency table - Malawi 
sink("outputs/results/CrossTable_results.xml")
CrossTable(dfmlw3$female_dm, dfmlw3$legume_intens,
           prop.r = TRUE,   
           prop.c = TRUE, 
           prop.t = FALSE,
           prop.chisq= FALSE,
           expected= FALSE,
           chisq = TRUE)
sink()

### correlation between DM and HDDS ----
# Malawi
kmlw <-dfmlw3 |> 
  select(female_dm, HDDS, fcs)
kruskal_result_mlw <- kruskal.test(HDDS ~ female_dm, data = kmlw)
kruskal_result_mlw1 <- kruskal.test(fcs ~ female_dm, data = kmlw)
# Ethiopia
keth <- dfeth |> 
  select(female_dm, HDDS)
kruskal_result_eth <- kruskal.test(HDDS ~ female_dm, data = keth)

# Tanzania
ktza <- dftza |> 
  select(female_dm, HDDS, fcs)
kruskal_result_tza <- kruskal.test(HDDS ~ female_dm, data = ktza)
kruskal_result_tza1 <- kruskal.test(fcs ~ female_dm, data = ktza)

results_table1 <- tibble::tibble(
  Country   = c("Malawi", "Ethiopia", "Tanzania"),
  Chi2 = c(kruskal_result_mlw$statistic, kruskal_result_eth$statistic, 
                          kruskal_result_tza$statistic ),
  p_value   = c(kruskal_result_mlw$p.value, kruskal_result_eth$p.value, kruskal_result_tza$p.value)
) |>
  mutate(
    Chi2      = round(Chi2, 2),
    p_value   = round(p_value, 3),
    Chi2_result  = paste0("χ²=", Chi2)
  ) |> 
  select(Country, Chi2_result, p_value, )
results_table1
ft1 <- flextable::flextable(results_table1) |> 
  set_caption(caption = " DM & HDDS")
results_table2 <- tibble::tibble(
  Country   = c("Malawi", "Tanzania"),
  Chi2 = c(kruskal_result_mlw1$statistic,  kruskal_result_tza1$statistic ),
  p_value   = c(kruskal_result_mlw1$p.value, kruskal_result_tza1$p.value)
) |>
  mutate(
    Chi2      = round(Chi2, 2),
    p_value   = round(p_value, 3),
    Chi2_result  = paste0("χ²=", Chi2)
  ) |> 
  select(Country, Chi2_result, p_value, )
results_table2
ft2 <- flextable::flextable(results_table2) |> 
  set_caption(caption = "DM & FCS")
doc <- read_docx() |> 
  body_add_flextable(ft) |> 
  body_add_par("", style = "Normal") |> 
  body_add_flextable(ft1) |> 
  body_add_flextable(ft2)
print(doc, target = "outputs/results/chi2_results.docx")
rm(ktza, keth, kmlw, ft, ft1, ft2, doc, 
   results_table, results_table1, results_table2)

# Figures -----
## Figure 1. Timeline of the employed LSMS datasets ----
pacman::p_load(tidyverse,
               forcats,
               ggthemes,
               patchwork,
               labelled,
               viridis,
               igraph,
               ggraph,
               hrbrthemes)
survey_data <- tribble(
  ~Country,      ~Wave, ~StartYear, ~EndYear,
  "Ethiopia",    "ESS 4", 2017.5, 2018.5,
  "Ethiopia",    "ESS 5", 2021.8, 2022.6,
  "Malawi",      "IHPS 1", 2010.2, 2011.2,
  "Malawi",      "IHPS 2", 2013.2, 2014,
  "Malawi",      "IHPS 3", 2016.3, 2017.3,
  "Malawi",      "IHPS 4", 2019.3, 2020.3,
  "Tanzania",    "NPS 4", 2014.7, 2015.7,
  "Tanzania",    "NPS 5", 2020.9, 2022
)

ggplot(survey_data, aes(y = fct_rev(factor(Country)))) +
  geom_rect(aes(xmin = StartYear, xmax = EndYear,
                ymin = as.numeric(fct_rev(factor(Country))) - 0.2,
                ymax = as.numeric(fct_rev(factor(Country))) + 0.2), fill = "#D4FF1D", color="blue") +
  geom_text(aes(x = (StartYear + EndYear)/2,  # centre of bar
                label = Wave),
            color = "black", fontface = "bold", size = 6) +
  scale_x_continuous(breaks = seq(2010, 2023, 1),
                     limits = c(2010, 2023)) +
  labs(x = NULL, y = NULL) +
  theme_fivethirtyeight(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 16, color = "blue"),
        axis.text.x = element_text(size = 16, color = "blue"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "blue")
        )
ggsave("outputs/figures/datatime.tiff",dpi=360, width = 40, height= 14, units = "cm")
rm(survey_data)

## Figure 2 - Legume intercropping and rotation among maize farmers in research countries -----
df1 <- dfmlw3 |> 
  select(rotation, intercrop, year) 
df1$country <-  "Malawi"
look_for(df1)
df1 <- df1 |> 
  mutate(rotation=if_else(rotation=="rotation", 1, 0),
         intercrop=if_else(intercrop=="mixed", 1, 0))
df2 <- dfeth |> 
  select(rotation, intercrop, year)
df2$country <- "Ethiopia"
look_for(df2)
df2 <- df2 |> 
  mutate(rotation=if_else(rotation=="rotation", 1, 0))
df3 <- dftza |> 
  select(rotation, intercrop, year)
look_for(df3)
df3$country <- "Tanzania"
dffig <- bind_rows(df1, df2) 
dffig <- bind_rows(dffig, df3)
look_for(dffig)
dffig$country <- as.factor(dffig$country)
rm(df1, df2, df3)
adoption_summary <- dffig |> 
  group_by(year, country) |> 
  summarise(
    intercropping = mean(intercrop, na.rm = TRUE) *100,
    rotation=mean(rotation, na.rm = TRUE) *100,
    .groups = "drop"
  )
look_for(adoption_summary)
adoption_long <- adoption_summary |>
  pivot_longer(cols = c(rotation, intercropping),
               names_to = "practice",
               values_to = "adoption_rate")
plots <- adoption_long |>
  group_split(country) |>
  purrr::map(~{
    ggplot(.x, aes(x = factor(year), y = adoption_rate,
                   fill = practice)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = round(adoption_rate, 0)),
                position = position_dodge(width = 1.1),
                vjust = -0.4, size = 5, show.legend = FALSE) +
      scale_fill_viridis_d(option = "viridis") +
      labs(x = "Year", y = "Adoption (%)", fill = "Practice",
           title = unique(.x$country)) +
      scale_y_continuous(limits = c(0,60), breaks = seq(0,60,10))+
      theme_fivethirtyeight()+
      theme(legend.title = element_blank(), 
            legend.text  = element_text(size = 16),
            plot.title = element_text(face = "bold",  size = 18),
            axis.text.y = element_text(face = "bold", size = 14),
            axis.text.x = element_text(face = "bold", size = 14),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14))
  })

combined <- wrap_plots(plots, ncol = 3) +
  plot_annotation(caption = "Note: In Malawi, rotation was captured differently across survey years: in 2013, rotation was only recorded if legumes were cropped in the last dry season, while in 2016 and 2019, data also captured 
planned legume cultivation after the reference rainy season. In Tanzania, rotation indicates that legumes were grown on a maize plot during the short rainy season. In Ethiopia, rotation indicates
that legumes were grown on a maize plot at least once during the past three years.",
                  theme = theme(plot.caption = element_text(hjust = 0, size=12),
                                plot.caption.position = "plot"))+
  plot_layout(guides = "collect", axes= "collect", axis_titles = "collect" ) & 
  theme(legend.position = "top")
combined
ggsave("outputs/figures/adoptions_sep.tiff",dpi=360, width = 40, height= 20, units = "cm")
rm(dffig, adoption_summary, combined)
