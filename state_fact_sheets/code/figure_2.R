library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(stringr)
library(grid)
library(janitor)
library(here)
library(tidyr)

heat_demand_profile <-  read_excel(here("state_fact_sheets/data/raw/heat_demand_profile.xlsx"))

# Define your custom colors
temp_colors <- c(
  "<200C" = "#FEBC11",
  "200-500C" = "#EF5645",
  ">500C" = "#C43424"
)

heat_demand_long <- heat_demand_profile %>%
  rename(Sector = 1) %>%
  pivot_longer(cols = c(`<200C`, `200-500C`, `>500C`),
               names_to = "Temp_Range",
               values_to = "Percentage") %>%
  mutate(Temp_Range = factor(Temp_Range, levels = c(">500C", "200-500C", "<200C")))


heat_plot <- ggplot(heat_demand_long, aes(x = Sector, y = Percentage, fill = Temp_Range)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = temp_colors, name = "Temperature Range") +
  coord_flip() +
  labs(
    title = "Industrial Heat Demand by Temperature Range",
    x = NULL,
    y = "Share of Heat Demand"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# save plot
ggsave(
  filename = here("state_fact_sheets/outputs/heat_demand_by_temp_range.png"),
  heat_plot,  # no 'plot =' here!
  width = 10,
  height = 6,
  dpi = 300
)