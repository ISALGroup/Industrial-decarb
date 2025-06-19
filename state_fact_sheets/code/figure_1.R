library(readr)
library(dplyr)

mn_geo <- read_csv("mn_facility_with_missing_coords.csv")
unique_titles <- mn_geo %>%
  distinct(naics_title) %>%
  arrange(naics_title)

head(unique_titles, 20)
unique(mn_geo$naics_title)
sort(unique(mn_geo$naics_title))
mn_geo <- mn_geo %>%  #categorize naics codes names into 4 fields
  mutate(sector = case_when(
    naics_title %in% c(
      "Animal (except Poultry) Slaughtering",
      "Beet Sugar Manufacturing",
      "Cheese Manufacturing",
      "Ethyl Alcohol Manufacturing",
      "Fats and Oils Refining and Blending",
      "Frozen Fruit, Juice, and Vegetable Manufacturing",
      "Rendering and Meat Byproduct Processing",
      "Soybean and Other Oilseed Processing",
      "Wet Corn Milling and Starch Manufacturing"
    ) ~ "Food & Bev",
    
    naics_title %in% c(
      "Paper Mills",
      "Paperboard Mills"
    ) ~ "Pulp & Paper",
    
    naics_title %in% c(
      "All Other Basic Organic Chemical Manufacturing",
      "All Other Miscellaneous Chemical Product and Preparation Manufacturing"
    ) ~ "Chemicals",
    
    TRUE ~ "Other"
  ))

emissions_data <- readxl::read_excel("emission.xlsx")
emissions_data <- emissions_data %>%
  mutate(total_emissions_tons = rowSums(select(., contains("subpart")), na.rm = TRUE))

mn_geo <- mn_geo %>%
  left_join(emissions_data %>%
              select(facility_id, reporting_year, total_emissions_tons),
            by = "facility_id")

mn_geo <- mn_geo %>%
  filter(reporting_year == 2023 | is.na(reporting_year))

library(ggplot2)
library(maps)
library(showtext)
library(sf)
font_add_google("Source Sans Pro", "ssp")
showtext_auto()
#load official MN maps in .gdb format
gdb_path <-"C:/Users/Mikeychael/OneDrive/文档/Database_graph/graph1/fgdb_bdry_counties/bdry_counties.gdb"
mn_counties<- st_read(gdb_path, layer = "County_Boundaries_in_Minnesota")
mn_counties <-st_transform(mn_counties, crs = 4326)
#set ranges
mn_geo <- mn_geo %>%
  mutate(emission_bucket = case_when(
    total_emissions_tons < 25000 ~ "<25k",
    total_emissions_tons < 250000 ~ "25k–250k",
    total_emissions_tons < 1000000 ~ "250k–1M",
    total_emissions_tons >= 1000000 ~ ">1M",
    TRUE ~ NA_character_
  ))
#adjust the circle legend size
size_map <- c("<25k" = 8, "25k–250k" = 12, "250k–1M" = 16, ">1M" = 23)

ggplot()+
  geom_sf(data = mn_counties, fill = "#F2F2F2", color = "#BBBBBB", size = 0.3)+
  geom_point(data = mn_geo, aes(x = longitude, y = latitude, fill = sector, size = emission_bucket),
             shape = 21, color = "white", stroke = 0.3, alpha = 0.5)+
  scale_size_manual(
    name = "GHG Emissions (tons)",
    values = size_map,
    breaks = names(size_map),
    guide = guide_legend(override.aes = list(shape = 21, fill = "grey80")))+
  scale_fill_manual(
    name = "Sector",
    values = c(
      "Chemicals" = "#FEBC11",
      "Food & Bev" = "#6D7D33",
      "Pulp & Paper" = "#047C91"),
    guide = guide_legend(override.aes = list(size = 4))) +
  coord_sf(xlim = c(-97.5, -89), ylim = c(43.3, 49.3), expand = FALSE) +
  theme_void(base_family = "ssp") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.1,
                                  margin = margin(b = 15)),
        legend.position = c(0.91, 0.38),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)) +
  labs(title = "2023 Minnesota Industrial Facility Emissions")


