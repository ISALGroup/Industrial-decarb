## National-Level Parameters ## 

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
eia_elec_range <- 
  read_xlsx("national_results/data/raw/EIA table_8.xlsx", skip = 2) |>
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

# Import NG prices 
eia_ng <- 
  read_xls('national_results/data/raw/EIA Monthly NG Prices.xls', 
           sheet = 'Data 1', skip = 2) |>
  select(-starts_with(c("Percent", "United States"))) |>
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
  # let's just use the last five years
  filter(Date >= as.Date("2020-01-01")) %>%  
  # Get high/low estimates
  filter(!is.na(price)) |>
  arrange(state, Date) %>%
  group_by(state) %>%
  mutate(
    # how much is price changing year over year? treat this as a distribution
    log_return = log(price) - log(lag(price))
  ) %>%
  summarise(
    sigma = sd(log_return, na.rm = TRUE),
    ng_price_latest = last(price),
    .groups = "drop"
  ) %>%
  mutate(
    # Z score for 10/90
    z = 1.28,  
    # get 10/90 percentile in the distribution
    ng_price_low  = ng_price_latest * exp(-z * sigma),
    ng_price_high = ng_price_latest * exp( z * sigma)
  ) %>%
  select(state, ng_price_latest, ng_price_high, ng_price_low)

# merge, input other parameters 
param_range <- 
  left_join(eia_elec_range, eia_ng, by = 'state') |>
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

write_csv(param_range, 'national_results/data/parameters_fuelrange.csv') 

#### Static Prices ####

eia_elec <-
  read_csv('state_fact_sheets/data/raw/EIA Annual Industrial Electricity Prices.csv') |>
  mutate(
    # get the state abbreviations
    state_full = sub(".*: ", "", description),
    state = state.abb[match(state_full, state.name)],
    
    # convert from cents/kwh to $/kwh,
    `2024` = `2024`/100
  ) |>
  rename(elec_price = `2024`) |>
  select(state, elec_price) |>
  filter(!state == 'DC')

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

