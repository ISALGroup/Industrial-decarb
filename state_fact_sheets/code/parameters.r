## State-Level Parameters ## 

#### SET-UP ####
library(readxl)
library(readr)
library(Hmisc)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)

# Import electricity prices
eia_elec <- 
  read_xlsx("state_fact_sheets/data/raw/EIA table_8.xlsx", skip = 2) |>
  slice_head(n = -48) |> # remove some stuff not needed at the end
  clean_names() |>
  mutate(
    average_price_cents_k_wh = as.numeric(average_price_cents_k_wh),
    elec_price = average_price_cents_k_wh / 100) |>
  group_by(state) |>
  dplyr::summarize(
    # weighted mean
    elec_price_mean = Hmisc::wtd.mean(elec_price, weights = sales_megawatthours, na.rm = TRUE),
    
    # weighted percentiles 
    elec_price_low  = Hmisc::wtd.quantile(elec_price, weights = sales_megawatthours,
                                   probs = 0.25, na.rm = TRUE),
    elec_price_high = Hmisc:: wtd.quantile(elec_price, weights = sales_megawatthours,
                                   probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# eia_elec <- 
#   read_csv('state_fact_sheets/data/raw/EIA Annual Industrial Electricity Prices.csv') |>
#   mutate(
#     # get the state abbreviations
#     state_full = sub(".*: ", "", description), 
#     state = state.abb[match(state_full, state.name)], 
#     
#     # convert from cents/kwh to $/kwh, 
#     `2024` = `2024`/100
#   ) |>
#   rename(elec_price = `2024`) |>
#   select(state, elec_price) |>
#   filter(!state == 'DC')

# Import NG prices 
eia_ng <- 
  read_xls('state_fact_sheets/data/raw/EIA Annual Industrial NG Prices.xls', 
           sheet = 'Data 1', skip = 2) |>
  pivot_longer(
    cols = -Date,
    names_to = "full_description",
    values_to = "price"
  ) |>
  
  # Clean state names out of column headers
  mutate(
    state_full = sub(" Natural Gas.*", "", full_description),
    state = state.abb[match(state_full, state.name)]
  ) |>
  
  # Drop any rows where abbrev didn’t match (territories, totals, etc.)
  filter(!is.na(state)) |>
  
  # Keep only the most recent non-missing value per state
  group_by(state) |>
  summarise(ng_price = last(na.omit(price)), .groups = "drop")

# merge, input other parameters 
param <- 
  left_join(eia_elec, eia_ng, by = 'state') |>
  mutate(
    r = 0.065, 
    t = 30, 
    ngboiler_om_low	= 0.03, 
    ngboiler_om_high	= 0.06, 
    eboiler_om_low	= 0.01, 
    eboiler_om_high = 0.01, 
    hthp_om_low = 0.01, 
    hthp_om_high = 0.05
  )

write_csv(param, 'state_fact_sheets/data/parameters.csv') 

