library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(stringr)
library(grid)
library(janitor)

ghgrp_df <- read_excel(here("data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx")) %>%
  filter(state == "MN", year == 2022) %>%
  mutate(primary_naics = as.character(primary_naics)) %>%
  rename(naics_code = primary_naics) %>% 
  filter(str_sub(naics_code, 1, 2) %in% c("31", "32", "33") & !str_starts(naics_code, "324")) %>% 
  filter(!is.na(co2e_emission)) 

sectors <- c("Chemicals", "Food & Beverage", "Pulp & Paper") #was highlight

sector_colors <- c(
  "Chemicals" = "#FEBC11",#official color
  "Food & Beverage" = "#047C91",
  "Pulp & Paper" = "#6D7D33",
  "Other Manufacturing" = "#DCE1E5"
) #was highlight_colors

mn_target_naics <- c("311221", "325193", "311313", "322120", "311224",
                     "325110", "325311", "312140", "311611", "311225")

subsector_colors <- c("311221" = "#6D7D33", 
                      "325193" = "#FEBC11", 
                      "311313" = "#6D7D33", 
                      "322120" = "#047C91", 
                      "311224" = "#6D7D33",
                      "325110" = "#FEBC11", 
                      "325311" = "#FEBC11", 
                      "312140" = "#6D7D33", 
                      "311611" = "#6D7D33", 
                      "311225" = "#6D7D33")

mn_highlight_naics <- c("325193", "322120", "311313") # these are the NAICS codes that should get labels in the chart

# Load NAICS description data
target_naics <- read_excel(here("data/raw/target_NAICS.xlsx")) %>%
  clean_names() %>%
  mutate(naics_code = as.character(naics_code)) 

plot_data <- ghgrp_df %>%
  left_join(target_naics, by = "naics_code") %>% 
  select(facility_id, county, county_fips, co2e_emission, naics_code, description) %>% 
  group_by(naics_code, description) %>%
  summarise(emission = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    mapped_sector = ifelse(naics_code %in% mn_target_naics, naics_code, "Other Manufacturing"),
    sector = case_when(
      naics_code %in% c("325193", "325110", "325311") ~ "Chemicals",
      naics_code %in% c("311221", "311224", "311225", "311313", "311611", "312140") ~ "Food & Beverage",
      naics_code %in% c("322120") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    ),
    highlight = naics_code %in% mn_highlight_naics
  ) %>%
  group_by(mapped_sector, description, sector, highlight) %>%
  summarise(emission = sum(emission), .groups = "drop") %>%
  mutate(
    pct = emission / sum(emission) * 100,
    label = ifelse(highlight, paste0(description, "\n", round(pct, 1), "%"), ""),
    fill_color = ifelse(mapped_sector == "Other Manufacturing", "#DCE1E5", subsector_colors[mapped_sector]))

plot_data <- plot_data %>%
  arrange(desc(mapped_sector)) %>%
  mutate(
    ymax = cumsum(emission),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2,
    angle = 90 - 360 * label_pos / sum(emission),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )


donut_chart <- ggplot(plot_data, aes(x = 2, y = emission, fill = sector)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = pi/2) +
  scale_fill_manual(values = sector_colors, name = "Sector") +
  theme_void() +
  xlim(0.5, 2.5) +
  ggtitle("MN Manufacturing CO2 Emissions by Subsector (2022)") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 22,
      face = "bold",
      margin = margin(t = 10, b = 20)
    ),
    plot.margin = margin(5, 5, 5, 5)
  )



grid.newpage()
print(donut_chart)




