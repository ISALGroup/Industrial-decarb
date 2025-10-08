# Minnesota Industrial Facility Emissions Map - Figure 1
# 
# Created: 7/1/2025           
# Author: Mikey He                 
#                               
# Description: Creates a geographic visualization of Minnesota's largest
#              industrial GHG emitters, showing facility locations sized
#              by emissions and colored by industry sector. This corrected
#              version properly calculates total emissions including all
#              EPA subparts (biogenic CO2, spent liquor, etc.)
#
# Notes: The "corrected" version fixes emission calculations that were
#        previously missing biogenic and spent liquor components. This
#        is crucial for paper mills which have large biogenic emissions.
#        
#        Facility sizes are binned into 4 categories to avoid visual clutter
#        while still showing the dramatic range (25k to >1M tons CO2e)
#        
#        Only facilities with valid coordinates are mapped - some facilities
#        in the original dataset had missing lat/lon data
#
# Data Sources:   
#        - mn_facility_with_missing_coords.csv (facility locations & basic info)
#        - emission.xlsx (2023 EPA GHGRP emissions by subpart)
#        - bdry_counties.gdb (MN county boundaries from state GIS)
#
# Outputs:   
#        - Interactive map showing facility locations, sizes, and sectors
#        - Corrected emission totals including all EPA subparts
#
# Known Issues:
#        - GDB path is hardcoded and needs to be updated for your system
#        - Some facilities may still have coordinate issues
#
##################################################################

# Load required packages
library(readr)
library(dplyr)
library(ggplot2)
library(maps)          # Basic map functionality
library(showtext)      # Consistent fonts across platforms
library(sf)            # Modern spatial data handling
library(readxl)

# Set up custom font to match other figures
font_add_google("Nunito", "avenir") 
showtext_auto()

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# Load facility location data - this has the basic facility info and coordinates
mn_facilities <- read_csv("state_fact_sheets/data/raw/mn_facility_with_missing_coords.csv")

# Load the detailed emissions data - this is where the correction happens
emission_data <- read_excel("state_fact_sheets/data/raw/emission.xlsx")

# Focus on 2023 data (most recent complete year)
emission_2023 <- emission_data %>%
  filter(reporting_year == 2023)

# =============================================================================
# CORRECTED EMISSION CALCULATION
# =============================================================================
# This is the key fix - include ALL EPA subparts, not just the main ones
# Previous versions missed biogenic CO2 and spent liquor, which are huge for paper mills

# Complete list of all possible subpart columns in EPA GHGRP data
all_subpart_columns <- c(
  # Subpart C - General stationary combustion (most common)
  "carbon_dioxide_subpart_c", "biogenic_co2_subpart_c", "methane_subpart_c", "nitrous_oxide_subpart_c",
  
  # Subpart AA - Pulp and paper (big in MN!)
  "carbon_dioxide_subpart_aa", "biogenic_co2_subpart_aa", "methane_subpart_aa", "nitrous_oxide_subpart_aa",
  "spent_liquor_ch4_subpart_aa", "spent_liquor_co2_subpart_aa", "spent_liquor_n2o_subpart_aa",
  
  # Subpart G - Ammonia manufacturing
  "nitrous_oxide_subpart_g", "methane_subpart_g", "carbon_dioxide_subpart_g", "biogenic_co2_subpart_g",
  
  # Subpart HH - Municipal solid waste landfills
  "methane_subpart_hh", "biogenic_co2_subpart_hh", "nitrous_oxide_subpart_hh",
  
  # Subpart S - Lime manufacturing
  "methane_subpart_s", "carbon_dioxide_subpart_s", "nitrous_oxide_subpart_s", "biogenic_co2_subpart_s",
  
  # Subpart V - Nitric acid production
  "nitrous_oxide_subpart_v", 
  
  # Subpart X - Petrochemical production
  "biogenic_co2_subpart_x", "nitrous_oxide_subpart_x", "carbon_dioxide_subpart_x", "methane_subpart_x"
)

# Calculate CORRECTED total emissions - this is the main fix
emission_2023 <- emission_2023 %>%
  mutate(total_emissions_tons = rowSums(select(., all_of(all_subpart_columns)), na.rm = TRUE))

# Let user know what we're including
cat("Total emissions calculated from", length(all_subpart_columns), "subpart columns (including biogenic & spent liquor)\n")

# =============================================================================
# JOIN FACILITY AND EMISSION DATA
# =============================================================================

mn_facilities_with_emissions <- mn_facilities %>%
  inner_join(emission_2023, by = "facility_id") %>%
  # Handle cases where NAICS codes might be in different columns
  mutate(
    final_naics = coalesce(primary_naics.y, primary_naics.x),  # Use emission data NAICS if available
    # Apply unified sector classification - keeping it simple with 3 main categories
    sector = case_when(
      substr(as.character(as.integer(final_naics)), 1, 3) %in% c("311", "312") ~ "Food & Bev",
      substr(as.character(as.integer(final_naics)), 1, 3) %in% c("321", "322") ~ "Pulp & Paper", 
      substr(as.character(as.integer(final_naics)), 1, 3) == "325" ~ "Chemicals",
      TRUE ~ "Other"  # Catch-all for miscellaneous manufacturing
    )
  ) %>%
  # Only map facilities with valid coordinates - can't plot what we can't locate
  filter(!is.na(longitude), !is.na(latitude))

# =============================================================================
# LOAD MINNESOTA COUNTY BOUNDARIES
# =============================================================================
# Using official state GIS data for accurate boundaries

# NOTE: This path needs to be updated for your system!
gdb_path <- "state_fact_sheets/data/raw/bdry_counties.gdb"
mn_counties <- st_read(gdb_path, layer = "County_Boundaries_in_Minnesota")
mn_counties <- st_transform(mn_counties, crs = 4326)  # Standard lat/lon projection

# =============================================================================
# CATEGORIZE FACILITIES BY EMISSION SIZE
# =============================================================================
# Create meaningful size bins that capture the huge range in emissions

mn_facilities_with_emissions <- mn_facilities_with_emissions %>%
  mutate(emission_range = case_when(
    total_emissions_tons < 25000 ~ "<25k",           # Small facilities
    total_emissions_tons >= 25000 & total_emissions_tons < 250000 ~ "25k–250k",   # Medium (most common)
    total_emissions_tons >= 250000 & total_emissions_tons < 1000000 ~ "250k–1M",  # Large
    total_emissions_tons >= 1000000 ~ ">1M",         # Mega-emitters (mostly paper mills)
    TRUE ~ NA_character_
  ))

# Map emission ranges to point sizes - need dramatic differences to show the scale
size_mapping <- c(
  "<25k" = 2,      # Small dots for small emitters
  "25k–250k" = 5,  # Medium dots for the bulk of facilities  
  "250k–1M" = 10,  # Large dots for big facilities
  ">1M" = 16       # HUGE dots for the mega-emitters (paper mills with biogenic CO2)
)

# =============================================================================
# SET UP CONSISTENT COLOR SCHEME
# =============================================================================
# Using same colors as the donut charts for visual consistency across figures

sector_colors <- c(
  "Chemicals" = "#FEBC11",    # Yellow - 19 facilities (ethanol, fertilizer, etc.)
  "Food & Bev" = "#047C91",   # Blue - 15 facilities (ag processing)
  "Pulp & Paper" = "#6D7D33"  # Green - 4 facilities (but 2 are the mega-emitters!)
)

# =============================================================================
# CREATE THE MAP
# =============================================================================

corrected_map <- ggplot() +
  # Start with county boundaries as base layer
  geom_sf(data = mn_counties, fill = "#F2F2F2", color = "#BBBBBB", size = 0.3) +
  
  # Add facility points - sized by emissions, colored by sector
  geom_point(data = mn_facilities_with_emissions, 
             aes(x = longitude, y = latitude, 
                 fill = sector, size = emission_range),
             shape = 21,           # Circle with border
             color = "white",      # White border for contrast
             stroke = 0.5,         # Border thickness
             alpha = 0.65) +       # Slight transparency to see overlaps
  
  # Configure the size legend (though we're hiding it for cleaner look)
  scale_size_manual(
    name = "GHG Emissions (tons)",
    values = size_mapping,
    breaks = c("<25k", "25k–250k", "250k–1M", ">1M"),
    labels = c("<25k", "25k–250k", "250k–1M", ">1M"),
    guide = "none"  # Hide legend - size differences are obvious
  ) +
  
  # Configure the color scheme
  scale_fill_manual(
    name = "Sector",
    values = sector_colors,
    guide = "none") +  # Hide legend for cleaner look
  
  # Set map extent to focus on Minnesota
  coord_sf(xlim = c(-97.5, -89), ylim = c(43.3, 49.3), expand = FALSE) +
  
  # Clean theme
  theme_void(base_family = "avenir") +
  theme(
    plot.title = element_text(
      size = 17, 
      face = "bold", 
      hjust = 0.5,
      margin = margin(t = 8, b = 6)),
    legend.position = "none",  # Clean look without legends
    plot.margin = margin(t = 8, r = 10, b = 5, l = 10)
  ) +
  labs(title = "2023 Minnesota Industrial Facility Emissions")

# Display the corrected map
print(corrected_map)

# save
ggsave("state_fact_sheets/outputs/mn_emissions_map.png", corrected_map, width = 8, height = 6, dpi = 300)

# =============================================================================
# NOTES ON THE CORRECTION
# =============================================================================
# The key difference in this "corrected" version is including ALL EPA subparts
# in the emission calculation. Previous versions missed:
# 
# 1. Biogenic CO2 (huge for paper mills burning wood waste)
# 2. Spent liquor emissions (another paper mill source)
# 3. Various process-specific subparts (G, HH, S, V, X)
#
# This correction dramatically changes the size of paper mill facilities on the map,
# which is more accurate since they are indeed among MN's largest industrial emitters
# when you include all their emission sources.
#
# The map now properly shows the dominance of the two large paper mills in 
# northern Minnesota, which makes sense given the state's forest industry history.

