# this code imports facility kWh demand, matches facilities to EIA Balancing Authority regions and aggregates pre- and post-electrification kWh demand
# then plots
# OBQ
# Sept 26. 2025

require(plyr)
require(tidyverse)
library(readr)
library(readxl)
library(janitor)

#map and plot stuff
library(ggplot2)
#install.packages("sf")
library(sf)

# get avenir
#install.packages("showtext")
library(showtext)
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

# facility data to point objects
final_facilities_sept25 <- read_xlsx("misc report figures/data/final_facilities_sept25.xlsx")


# eventually remove and add back in actual electricity demand columns
final_facilities_sept25 <- final_facilities_sept25 %>% 
  filter(facility_id != 1010700) %>%  #remove USVI facility that later fails to match to a lower 48 balancing authority
  mutate(
    pre_MWh = runif(n = n(), min = 50, max = 100),
    post_MWh = pre_MWh * 1.25
  )

facilities_sf <- st_as_sf(final_facilities_sept25, coords = c("longitude", "latitude"), crs = 4326)

# get BA shapes
ba_polygons <- st_read("misc report figures/data/Balancing_Authorities/Balancing_Authorities.shp")

# merge w facilities
facilities_sf <- st_transform(facilities_sf, st_crs(ba_polygons))

sf_use_s2(FALSE)  # avoid s2 errors on complex polygons

# BAs sometimes overlap (previously resulted in 300+ duplicates; now dropping and keeping only first match)
# 8 facilities in AEC (PowerSouth Energy Cooperative) but no EIA sales data. AEC is actually a bunch of smaller coops which are assigned to ba_code SOCO in the EIA sales data
# re-code AEC to SOCO
# re-code EEI facility to MISO. EEI sold to GLHB which functions as part of MISO.
# re-code OVEC to PJM which it joined in 2018

facilities_with_ba <- facilities_sf %>%
  st_join(ba_polygons, join = st_intersects) %>%
  distinct(facility_id, .keep_all = TRUE) %>% 
  #mutate() EIAcode AEC to SOCO, EEI to MISO, OVEC to PJM -- also replace EIAname and EIAregion

# group by BA
ba_aggregated <- facilities_with_ba %>%
  st_drop_geometry() %>%
  group_by(EIAcode, EIAname) %>%
  summarise(
    total_pre_MWh  = sum(pre_MWh,  na.rm = TRUE),
    total_post_MWh = sum(post_MWh, na.rm = TRUE),
    n_facilities   = n()
  )

# group by BA region
ba_region_aggregated <- facilities_with_ba %>%
  st_drop_geometry() %>%
  group_by(EIAregion) %>%
  summarise(
    total_pre_MWh  = sum(pre_MWh,  na.rm = TRUE),
    total_post_MWh = sum(post_MWh, na.rm = TRUE),
    n_facilities   = n()
  )

# plot by BA region
ba_polygons_with_totals <- ba_polygons %>%
  left_join(ba_region_aggregated, by = "EIAregion")

ggplot() +
  geom_sf(data = ba_polygons_with_totals, aes(fill = total_post_MWh), color = "grey30") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()


#import Sales_Ult_Cust_2023 and filter out Part == C because that is only energy delivered, where someone else supplies the energy (A is both, B is only energy, D is both)

#first fix the fucked headers
header1 <- read_xlsx("misc report figures/data/f8612023/Sales_Ult_Cust_2023.xlsx", n_max = 1, col_names = FALSE)
header2 <- read_xlsx("misc report figures/data/f8612023/Sales_Ult_Cust_2023.xlsx", skip = 1, n_max = 1, col_names = FALSE)
header3 <- read_xlsx("misc report figures/data/f8612023/Sales_Ult_Cust_2023.xlsx", skip = 2, n_max = 1, col_names = FALSE)

combined_names <- paste(header1, header2, header3, sep = "_") %>% 
  make_clean_names()

combined_names_clean <- c(
  "data_year",
  "utility_number",
  "utility_name",
  "part",
  "service_type",
  "data_type",
  "state",
  "ownership",
  "ba_code",
  "residential_revenues_thousand_dollars",
  "residential_sales_mwh",
  "residential_customers_count",
  "commercial_revenues_thousand_dollars",
  "commercial_sales_mwh",
  "commercial_customers_count",
  "industrial_revenues_thousand_dollars",
  "industrial_sales_mwh",
  "industrial_customers_count",
  "transportation_revenues_thousand_dollars",
  "transportation_sales_mwh",
  "transportation_customers_count",
  "total_revenues_thousand_dollars",
  "total_sales_mwh",
  "total_customers_count"
)

# Now read the actual data, skipping the first three rows
eia_sales_2023 <- read_xlsx("misc report figures/data/f8612023/Sales_Ult_Cust_2023.xlsx", skip = 3, col_names = combined_names_clean)

#make numeric
num_cols <- c(
  "data_year",
  "utility_number",
  "residential_revenues_thousand_dollars",
  "residential_sales_mwh",
  "residential_customers_count",
  "commercial_revenues_thousand_dollars",
  "commercial_sales_mwh",
  "commercial_customers_count",
  "industrial_revenues_thousand_dollars",
  "industrial_sales_mwh",
  "industrial_customers_count",
  "transportation_revenues_thousand_dollars",
  "transportation_sales_mwh",
  "transportation_customers_count",
  "total_revenues_thousand_dollars",
  "total_sales_mwh",
  "total_customers_count"
)

eia_sales_2023 <- eia_sales_2023 %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.))))

# sanity check... net electricity used by industry in USA via MECS 2022 (~783,620 million kWh = 783,620,000,000 kWh = 783,620,000 MWh)

net_eia_sales_2023 <- eia_sales_2023 %>% 
  filter(part != "C") %>% 
  summarise(sum = sum(industrial_sales_mwh, na.rm = TRUE)) %>% 
  pull(sum)

# 1,009,255,634 MWh = approx 1 trillion kWh industrial electricity use. 
# confirmed here! https://www.eia.gov/energyexplained/electricity/use-of-electricity.php#:~:text=Electricity%20consumption%20in%20the%20United%20States%20was,(mostly%20to%20public%20transit%20systems)0.01%20trillion%20kWh0.2%25


########

eia_industrial_by_ba <- eia_sales_2023 %>%
  filter(part != "C") %>%
  group_by(ba_code) %>%
  summarise(
    total_industrial_mwh_2023 = sum(industrial_sales_mwh, na.rm = TRUE)
  )

ba_with_baseline <- ba_aggregated %>%
  left_join(eia_industrial_by_ba, by = c("EIAcode" = "ba_code")) %>%
  mutate(
    pct_increase = (total_post_MWh - total_pre_MWh) / total_industrial_mwh_2023 * 100
  )




