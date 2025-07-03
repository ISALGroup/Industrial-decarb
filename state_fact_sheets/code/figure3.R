# Minnesota Industrial Emissions Analysis - Figure 3
# Created: 7/1/2025              
# Author: Mikey He                 
#                               
# Description: Creates three complementary donut charts showing Minnesota's
#              emissions breakdown from different perspectives - overall state
#              emissions by sector, industrial subsector breakdown, and 
#              detailed NAICS-level analysis for key industries
#
# Notes: This script generates Figure 3 with three panels:
#        3a - Statewide emissions by economic sector (highlights industrial)
#        3b - Industrial emissions by manufacturing subsector  
#        3c - Target subsector emissions by specific NAICS codes
#        
#        The color scheme is consistent across charts to maintain visual
#        coherence and help readers track industrial categories
#
# Data Sources:   
#        - rlps_ghg_emitter_subpart_w_NAICS.xlsx (EPA GHGRP data with NAICS)
#        - Manual emissions data for Chart 3a (state-level totals)
#
# Outputs:   
#        - Combined three-panel donut chart visualization
#        - Individual chart objects (chart_3a, chart_3b, chart_3c)
#
##################################################################

# Load required packages - using showtext for consistent font rendering
library(readxl)
library(dplyr)
library(ggplot2)
library(showtext)      # Better font handling than base R
library(patchwork)     # For combining multiple plots elegantly
library(stringr)

# Set up custom font - Nunito gives a clean, modern look
font_add_google("Nunito", "avenir") 
showtext_auto()

# Main code for figure 3 - combining three donut charts with different data granularity

# =============================================================================
# CHART 3A: Minnesota Statewide Emissions by Economic Sector
# =============================================================================
# This shows the big picture - where do MN's emissions come from?
# Industrial sectors are highlighted in red tones to draw attention

emissions_3a <- data.frame(
  Sector = c(
    "Industrial-GHGRP",      # Large industrial facilities (EPA reporting)
    "Industrial-Small",      # Smaller industrial not in GHGRP
    "Electricity Generation",
    "Transportation",        # Biggest slice - no surprise there
    "Residential",
    "Commercial",
    "Agriculture",           # Surprisingly high for MN
    "Waste"),
  Emissions = c(12.3, 10.7, 25.6, 36.8, 10.5, 9.4, 30.8, 1.5)  # Million tons CO2e
)

# Color coding strategy: highlight industrial in red shades, everything else neutral gray
emissions_3a <- emissions_3a %>%
  mutate(
    FillColor = case_when(
      Sector == "Industrial-GHGRP" ~ "#EF5645",   # Bright red for large industrial
      Sector == "Industrial-Small" ~ "#C43424",   # Darker red for small industrial  
      TRUE ~ "#DCE1E5"                            # Neutral gray for non-industrial
    ),
    Fraction = Emissions / sum(Emissions),
    Label = paste0(Sector, "\n", round(Fraction * 100, 1), "%")  # Two-line labels look cleaner
  )

# Create the donut chart - starting at pi/2 puts largest slice at top
chart_3a <- ggplot(emissions_3a, aes(x = 2, y = Emissions, fill = FillColor)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar("y", start = pi / 2) +  # Start at top for visual appeal
  xlim(0.2, 2.8) +                    # Creates the donut hole
  scale_fill_identity() +             # Use our custom colors directly
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.52),  # Slight adjustment for better centering
    size = 3.5, family = "avenir", color = "black", fontface = "bold"
  ) +
  theme_void(base_family = "avenir") +
  ggtitle("3a. Minnesota Emissions Breakdown by Economic Sector") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold",
                              margin = margin(t = 5, b = 5), family = "avenir"),
    plot.margin = margin(8, 8, 8, 8)
  )

# =============================================================================
# CHART 3B: Industrial Emissions by Manufacturing Subsector  
# =============================================================================
# Now we zoom into just the industrial piece - what types of manufacturing?

df_3b <- read_excel("rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  filter(state == "MN", year == 2022) %>%
  mutate(
    # Clean up NAICS codes - sometimes primary is missing, use secondary as backup
    primary_naics = as.character(primary_naics),
    secondary_naics = as.character(secondary_naics),
    effective_naics = ifelse(!is.na(primary_naics), primary_naics, secondary_naics)
  ) %>%
  # Focus only on manufacturing (NAICS 31-33)
  filter(str_sub(effective_naics, 1, 2) %in% c("31", "32", "33"))

# Function to map NAICS codes to readable subsector names
# This is where domain knowledge comes in handy
naics_to_subsector <- function(code) {
  if (is.na(code)) return(NA)
  prefix <- substr(code, 1, 3)
  if (prefix %in% c("311", "312")) return("Food & Beverage")  # Food processing is big in MN
  if (prefix == "325") return("Chemical")                     # Includes ethanol, fertilizer
  if (prefix == "324") return("Petroleum & Coal Products")    # Refineries
  if (prefix %in% c("321", "322")) return("Pulp & Paper")    # MN's forest industry legacy
  return("Other Manufacturing")                               # Catch-all for everything else
}

# Apply the mapping and aggregate emissions by subsector
df_3b <- df_3b %>%
  filter(!is.na(co2e_emission)) %>%
  mutate(subsector = sapply(effective_naics, naics_to_subsector))

# Define which sectors to highlight with distinct colors
highlight_sectors <- c("Chemical", "Food & Beverage", "Pulp & Paper", "Petroleum & Coal Products")

plot_data_3b <- df_3b %>%
  group_by(subsector) %>%
  summarise(emission = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Lump small sectors into "Other Manufacturing" for cleaner visualization
    mapped_sector = ifelse(subsector %in% highlight_sectors, subsector, "Other Manufacturing")
  ) %>%
  group_by(mapped_sector) %>%
  summarise(emission = sum(emission), .groups = "drop") %>%
  mutate(
    pct = emission / sum(emission) * 100,
    label = paste0(mapped_sector, "\n", round(pct, 1), "%")
  )

# Color scheme that will carry through to Chart 3c for consistency
highlight_colors_3b <- c(
  "Chemical" = "#FEBC11",           # Yellow - largest industrial emitter
  "Food & Beverage" = "#047C91",    # Blue 
  "Pulp & Paper" = "#6D7D33",       # Green 
  "Petroleum & Coal Products" = "#DAE6E6",  # Light blue
  "Other Manufacturing" = "#DCE1E5"          # Neutral gray
)

plot_data_3b <- plot_data_3b %>%
  mutate(fill_color = highlight_colors_3b[mapped_sector])

chart_3b <- ggplot(plot_data_3b, aes(x = 2, y = emission, fill = fill_color)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar(theta = "y", start = pi/2) +
  scale_fill_identity() +
  theme_void(base_family = "avenir") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.8, color = "black", family = "avenir", fontface = "bold") +
  xlim(0.2, 2.8) +
  ggtitle("3b. Minnesota Industrial Emissions Breakdown by Subsector") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold",
                              margin = margin(t = 5, b = 5), family = "avenir"),
    plot.margin = margin(8, 8, 8, 8)
  )

# =============================================================================
# CHART 3C: Target Subsector Emissions by Specific NAICS Codes
# =============================================================================
# Final zoom level - which specific industries within our target subsectors?

emissions_data_3c <- read_excel("rlps_ghg_emitter_subpart_w_NAICS.xlsx")
mn_2022_data_3c <- emissions_data_3c %>%
  filter(year == 2022, state == "MN") %>%
  filter(!is.na(primary_naics), !is.na(co2e_emission))

mn_2022_data_3c <- mn_2022_data_3c %>%
  mutate(naics_code = as.character(as.integer(primary_naics)))

# These are the specific NAICS codes we want to highlight individually
# Based on emissions significance and policy relevance
target_naics_codes <- c("311221", "325193", "311313", "322120", "311224",
                        "325110", "325311", "312140", "311611", "311225")

# Map NAICS to broader sectors for color consistency with Chart 3b
mn_2022_data_3c <- mn_2022_data_3c %>%
  mutate(sector = case_when(
    substr(naics_code, 1, 3) %in% c("311", "312") ~ "Food & Beverage",
    substr(naics_code, 1, 3) %in% c("321", "322") ~ "Pulp & Paper",
    substr(naics_code, 1, 3) == "325" ~ "Chemical",
    TRUE ~ "Other"
  ))

# Focus on our three target subsectors and identify priority NAICS codes
target_subsectors <- c("Chemical", "Food & Beverage", "Pulp & Paper")
all_subsector_data <- mn_2022_data_3c %>%
  filter(sector %in% target_subsectors) %>%
  mutate(is_target_naics = naics_code %in% target_naics_codes)

# Human-readable names for the NAICS codes (because who remembers what 311221 is?)
naics_industry_names <- c(
  "311221" = "Wet Corn Milling",      # Ethanol production
  "311224" = "Soybean Processing",    # Ag processing
  "311225" = "Fats & Oils Refining",  # More ag processing
  "311313" = "Beet Sugar Mfg",        # Sugar beets are big in MN
  "311611" = "Animal Slaughter",      # Meat processing
  "322120" = "Paper Mills",           # Classic MN industry
  "325193" = "Ethyl Alcohol Mfg"      # Ethanol again - different NAICS
)

# Create display categories: show target NAICS individually, lump others
chart3c_processed <- all_subsector_data %>%
  mutate(
    display_category = case_when(
      is_target_naics == TRUE ~ naics_code,  # Show these individually
      TRUE ~ "Other"                         # Lump the rest
    ),
    display_color = case_when(
      is_target_naics == TRUE ~ sector,      # Color by parent sector
      TRUE ~ "Other"                         # Gray for lumped category
    )
  )

# Aggregate emissions by our display categories
final_chart3c_data <- chart3c_processed %>%
  group_by(display_category, display_color) %>%
  summarise(total_emissions = sum(co2e_emission, na.rm = TRUE), .groups = "drop")

final_chart3c_data <- final_chart3c_data %>%
  mutate(
    industry_name = case_when(
      display_category == "Other" ~ "Other",
      TRUE ~ naics_industry_names[display_category]
    ),
    percentage = round(total_emissions / sum(total_emissions) * 100, 1),
    chart_label = paste0(industry_name, "\n", percentage, "%")
  ) %>%
  filter(!is.na(industry_name))

# Quick check of what we're plotting
print(final_chart3c_data %>% select(display_category, industry_name, percentage, display_color))

# Same color scheme as Chart 3b for visual consistency
extended_color_scheme_3c <- c(
  "Chemical" = "#FEBC11",        # Yellow for chemical industries
  "Food & Beverage" = "#047C91", # Blue for food/ag processing
  "Pulp & Paper" = "#6D7D33",    # Green for paper mills
  "Other" = "#DCE1E5"            # Gray for everything else
)

chart_3c <- ggplot(final_chart3c_data, aes(x = 2, y = total_emissions, fill = display_color)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar("y", start = pi / -2.8) +  # Different start angle for visual variety
  xlim(0.2, 2.5) +
  scale_fill_manual(values = extended_color_scheme_3c) +
  geom_text(aes(label = chart_label), 
            position = position_stack(vjust = 0.4),  # Adjusted for smaller text
            size = 3.0, fontface = "bold", family = "avenir", color = "black") +
  theme_void(base_family = "avenir") +
  ggtitle("3c. Target Subsector Emissions Breakdown by NAICS Code") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              margin = margin(t = 5, b = 5), family = "avenir"),
    legend.position = "none",
    plot.margin = margin(8, 8, 8, 8)
  )

# =============================================================================
# COMBINE ALL THREE CHARTS
# =============================================================================
# Stack them vertically for a clean, readable layout

combined_donut_charts <- chart_3a / chart_3b / chart_3c

# Fine-tune the layout spacing
combined_donut_charts <- combined_donut_charts + 
  plot_layout(heights = c(1, 1, 1)) &  # Equal height for each panel
  theme(plot.margin = margin(8, 8, 8, 8))

# Display the final result
print(combined_donut_charts)

# Note: This creates a comprehensive view of MN industrial emissions from 
# state-level down to specific industry codes. The consistent color scheme
# helps readers track the industrial story across all three levels of detail.

