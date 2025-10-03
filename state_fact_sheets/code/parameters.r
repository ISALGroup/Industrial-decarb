## State-Level Parameters ## 

#### SET-UP ####
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(janitor)
library(glue)

# Import electricity prices
# See sources in param
eia_elec <- read_xlsx(
  "state_fact_sheets/data/raw/EIA Sales_Ult_Cust_2023.xlsx",
  sheet = "States", skip = 2, n_max = 2824, guess_max = 2824
) |>
  clean_names() |>
  select(state, thousand_dollars_16, megawatthours_17) |>
  rename(
    revenues_ind = thousand_dollars_16,
    sales_mwh_ind = megawatthours_17
  ) |>
  mutate(
    revenues_ind = na_if(revenues_ind, "."),
    revenues_ind = na_if(revenues_ind, "-"),
    sales_mwh_ind = na_if(sales_mwh_ind, "."),
    sales_mwh_ind = na_if(sales_mwh_ind, "-"),
    revenues_ind = as.numeric(revenues_ind),
    sales_mwh_ind = as.numeric(sales_mwh_ind),
    elec_price = if_else(sales_mwh_ind > 0,
                         revenues_ind / sales_mwh_ind,
                         NA_real_)
  ) |>
  group_by(state) |>
  summarize(
    elec_price_high = ifelse(all(is.na(elec_price)), NA_real_,
                             max(elec_price, na.rm = TRUE)),
    elec_price_low  = ifelse(all(is.na(elec_price)), NA_real_,
                             min(elec_price, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  filter(!state == 'DC' & !is.na(state))
  

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
    ngboiler_om_best	= 0.03, 
    ngboiler_om_worst	= 0.06, 
    eboiler_om_best	= 0.01, 
    eboiler_om_worst = 0.01, 
    hthp_om_best = 0.01, 
    hthp_om_worst = 0.05
  )

write_csv(param, 'state_fact_sheets/data/parameters.csv') 

