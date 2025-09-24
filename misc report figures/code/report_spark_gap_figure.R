# this code imports natural gas and electricity pricing data to generate a spark gap heatmap of the U.S. 
# OBQ
# Sept 23. 2025

require(plyr)
require(tidyverse)
library(readr)
library(ggplot2)
#install.packages("sf")
library(sf)
install.packages("mapview")
install.packages("showtext")
library(showtext)
#install.packages("tigris")
library(tigris)
options(tigris_use_cache = TRUE)

# get avenir

font_add(family = "Avenir", regular = "/System/Library/Fonts/Supplemental/Avenir.ttc")
showtext_auto()

# store colors
brand_colors <- c("#FFFFFF",
                  "#C2E4E6",
                  "#09847A",
                  "#003660",
                  "#DCE1E5",
                  "#047C91",
                  "#0BA89A",
                  "#FEBC11",
                  "#DCD6CC",
                  "#9CBEBE",
                  "#DAE6E6",
                  "#6D7D33")

gradient_2035_colors <- c("#DCE1E5",
                          "#C2E4E6",
                          "#0BA89A",
                          "#09847A",
                          "#003660")


#dwnld params
parameters <- read_csv("state_fact_sheets/data/parameters.csv")

#make spark gap
parameters <- parameters %>% 
  mutate(
    natgas_per_kwh = ng_price/293, #kwh per Mcf
    spark_gap = elec_price/natgas_per_kwh
  )

#filter and join to shapefiles
states <- states(cb = TRUE) %>% 
  filter(!STUSPS %in% c("AS","GU","MP","PR","VI", "DC")) %>% 
  shift_geometry()

map_data <- states %>%
  left_join(parameters, by = c("STUSPS" = "state"))

#plot
ggplot(map_data) +
  geom_sf(aes(fill = spark_gap), color = "white") +
  scale_fill_gradientn(
    colors = gradient_2035_colors,
    name   = "Spark Gap"
  ) +
  labs(
    title    = "Electricity vs Natural Gas Prices by State",
    subtitle = "Spark gap ratio ($/kWh electricity ÷ $/kWh natural gas equivalent)",
    caption  = "Source: EIA (2023)"  # caption text
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    plot.caption  = element_text(size = 8, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ggsave("misc report figures/output/spark_gap_2023.pdf", width = 5, height = 4.5, units = 'in', scale = 1.3) 

