# Figures ====
## Bar chart ====
pacman::p_load(tidyverse,
               forcats,
               ggthemes,
               patchwork,
               viridis,
               igraph,
               ggraph)
df1 <- dfmlwfull |> 
  select(legume_intens, wave) 
df1$country <-  "Malawi"
df2 <- dfeth |> 
  select(legume_intens, wave)
df2$country <- "Ethiopia"
df3 <- dftza |> 
  select(legume_intens, wave,)
df3$country <- "Tanzania"
dffig <- bind_rows(df1, df2) 
dffig <- bind_rows(dffig, df3)
look_for(dffig)
dffig$country <- as.factor(dffig$country)
dffig$wave <- as.factor(dffig$wave)
rm(df1, df2, df3)
adoption_summary <- dffig |> 
  group_by(wave, country) |> 
  summarise(
    adoption_rate = mean(legume_intens, na.rm = TRUE)*100,
    .groups = "drop"
  )
adoption_summary$wave <- fct_relevel(adoption_summary$wave, 
                                     sort(levels(adoption_summary$wave)))

look_for(adoption_summary)
adoption_plot <- ggplot(adoption_summary, aes(x=country, y=adoption_rate, fill=wave)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0(round(adoption_rate, 0), "%"),
                group = wave),
            position = position_dodge(width = 0.8),
            hjust = 1.8,
            size = 7) +
  geom_text(aes(label = paste0(wave),
                group = wave),
            position = position_dodge(width = 0.8),
            hjust = -0.2,
            size = 5) +
  coord_flip() +
  scale_fill_viridis_d(option = "cividis") +
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL,
    y = "Adoption Rate (%)",
    title = "Adoption Rates by Country and Wave",
    fill = NULL
  ) +
  theme_wsj() +
  theme(
    plot.title = element_text(face = "bold",  size = 12),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

adoption_plot
ggsave("outputs/figures/adoption.tiff",dpi=360, width = 32, height= 20, units = "cm")

