# Figures and tables ====
# Table =====
## chi2 test and Cramers V ----
pacman::p_load(vcd,
               flextable,
               officer)
# Malawi
table_mlw <- table(dfmlwfull$female_dm, dfmlwfull$legume_intens)
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
    Chi2      = round(Chi2, 2),
    df        = as.integer(df),
    Cramers_V = round(Cramers_V, 3),
    # combine APA style text
    Chi2_result  = paste0("χ²(", df, ") = ", Chi2, ", p ", format_p(p_value))
  ) |>
  select(Country, Chi2_result, Cramers_V)

results_table
ft <- flextable::flextable(results_table)
doc <- read_docx() |> 
  body_add_flextable(ft)

print(doc, target = "outputs/results/chi2_results.docx")

# Figure -----
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
  # 1. draw the duration bar:
  geom_rect(aes(xmin = StartYear, xmax = EndYear,
                ymin = as.numeric(fct_rev(factor(Country))) - 0.2,
                ymax = as.numeric(fct_rev(factor(Country))) + 0.2), fill = "#D4FF1D", color="blue") +
  # 2. add wave label in the middle of the bar:
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

## Legume intensification ----
df1 <- dfmlwfull |> 
  select(legume_intens, year) 
df1$country <-  "Malawi"
df2 <- dfeth |> 
  select(legume_intens, year)
df2$country <- "Ethiopia"
df3 <- dftza |> 
  select(legume_intens, year,)
df3$country <- "Tanzania"
dffig <- bind_rows(df1, df2) 
dffig <- bind_rows(dffig, df3)
look_for(dffig)
dffig$country <- as.factor(dffig$country)
rm(df1, df2, df3)
adoption_summary <- dffig |> 
  group_by(year, country) |> 
  summarise(
    adoption_rate = mean(legume_intens, na.rm = TRUE)*100,
    .groups = "drop"
  )
look_for(adoption_summary)

ggplot(adoption_summary, aes(x = year, y = adoption_rate,
                             color = country, group = country)) +
  geom_segment(aes(xend = year, y = 1, yend = adoption_rate),
               size = 3) +
  geom_point(size = 10, alpha = 0.8) +
  geom_text(aes(label = round(adoption_rate, 0)),
            show.legend = FALSE,
            hjust = 1.5,
            size = 8) +
  
  scale_color_viridis_d(option="viridis") +
  
  scale_x_continuous(breaks = unique(adoption_summary$year)) + 
  # or if year is already a factor:
  # scale_x_discrete(limits = levels(adoption_summary$year)) 
  
  labs(x = "Year",
       y = "Legume intensification (%)",
       color = "Country") +
  theme_calc() +
  theme(
    plot.title = element_text(face = "bold",  size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    legend.title = element_blank(),  # title size
    legend.text  = element_text(size = 14),
    panel.grid.major.y = element_blank()
  )
ggsave("outputs/figures/adoptions.tiff",dpi=360, width = 36, height= 20, units = "cm")
rm(dffig, adoption_summary)

## Rotation and intercropping in research countries -----
df1 <- dfmlw |> 
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
  plot_annotation(caption = "Note: In Malawi, rotation was captured differently across survey years: in 2010 and 2013, rotation was only recorded if legumes were cropped in the previous dry season, while in 2016 and 2019, data also captured planned legume 
cultivation after the reference rainy season. In Tanzania, rotation indicates that legumes were grown on a maize plot during the short rainy season. In Ethiopia, rotation indicates that legumes were grown on a maize plot at least once during 
the past three years.",
                  theme = theme(plot.caption = element_text(hjust = 0, size=10),
                                plot.caption.position = "plot"))+
  plot_layout(guides = "collect", axes= "collect", axis_titles = "collect" ) & 
  theme(legend.position = "top")
combined
ggsave("outputs/figures/adoptions_sep.tiff",dpi=360, width = 40, height= 20, units = "cm")
rm(dffig, adoption_summary)
