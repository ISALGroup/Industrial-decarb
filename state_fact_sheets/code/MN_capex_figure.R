#### Minnesota Capex Figure ####

investments.o <- read_excel("state_fact_sheets/data/raw/MN_sector_investments.xlsx")

investments <- 
  investments.o |>
  rename(
    eboiler_high = `Electric boiler (best case)`, 
    eboiler_low = `Electric boiler (worse case)`, 
    hthp_high = `Ambient heat pump (best case)`, 
    hthp_low = `Ambient heat pump (worse case)`
  ) |>
  select(sector, eboiler_low, eboiler_high, hthp_low, hthp_high) |>
  mutate(
    eboiler_high = eboiler_high / 1000000, 
    eboiler_low = eboiler_low / 1000000, 
    hthp_high = hthp_high / 1000000, 
    hthp_low = hthp_low / 1000000
  ) |>
  pivot_longer(
    cols = c(eboiler_low, eboiler_high, hthp_low, hthp_high), 
    names_to = "technology", 
    values_to = "capex_usd"
  ) |> 
  mutate(
    bound = case_when(
      str_detect(technology, "_low") ~ "low",
      str_detect(technology, "_high") ~ "high",
    ), 
    technology = str_remove(technology, "_low|_high")
  ) |>
  pivot_wider(
    names_from = "bound", 
    values_from = "capex_usd"
  ) 

ggplot(investments, aes(x = sector, ymin = low, ymax = high, color = technology)) +
  geom_linerange(
    position = position_dodge(width = 0.4),
    linewidth = 8  
  ) +
  labs(x = NULL, y = "Facility CAPEX ($ millions)", color = "Technology") +
  scale_color_manual(
    values = c(
      "eboiler" = "#1f78b4",
      "hthp" = "#33a02c"
    ),
    labels = c(
      "eboiler" = "E-boiler",
      "hthp" = "HTHP"
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.8, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )

ggsave("state_fact_sheets/outputs/mn_sector_capex.png", width = 8, height = 5, dpi = 300)

