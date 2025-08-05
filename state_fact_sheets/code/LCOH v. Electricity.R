#### Electricity Price vs. Spark Gap ####

library(readxl)
library(dplyr)
library(ggplot2)

tech_input_raw_df <- read_excel("state_fact_sheets/data/raw/lcoh_policy_modeling_input.xlsx")
lcoh_raw_df <- read_excel("state_fact_sheets/data/raw/MN_lcoh_facility.xlsx")

### --> IF USING: ####
# flip x axis 
# change to "capex subsidy" 
# combine the two charts (maybe) 


#### Set-up ####

# Parameters
r <- 0.065 # discount rate
t <- 15 # years 
elec_price <- 0.092  # $/kWh

lcoh_func <- 
  function(capex, capex_share, elec_price, change_in_elec_demand_kwh, heat_mmbtu, t, r){
    capex_adj <- capex - (capex * capex_share)
    
    numerator <- capex_adj
    denominator <- 0
    
    opex <- change_in_elec_demand_kwh * elec_price
    
    for (i in 1:t) {
      numerator <- numerator + (opex / (1 + r)^i)
      denominator <- denominator + (heat_mmbtu / (1 + r)^i)
  }
  
  return(numerator / denominator)
  }

# Get sectoral statistics for scenario 4 
scenario_4_input <-
  tech_input_raw_df |>
  filter(tech_scenario %in% c('scenario_4_worst', 'scenario_4_best')) |>
  mutate(naics_description = case_when(
    naics_description == 'Beet Sugar Manufacturing' ~ 'Beet Sugar',
    naics_description == 'Ethyl Alcohol Manufacturing' ~ 'Ethyl Alcohol',
    TRUE ~ naics_description
  )) |>
  group_by(naics_description) |>
  summarize(
    sector = min(sector), 
    capex = mean(capex), 
    change_in_elec_demand_kwh = mean(change_in_elec_demand_kwh), 
    heat_mmbtu = mean(heat_mmbtu)
  ) 

# Other plot set-up
x_vals <- seq(0, 0.1, length.out = 200)

sector_colors <- c(
  "Paper Mills" = "#6d7d33",
  "Beet Sugar" = "#047c91",
  "Ethyl Alcohol" = "#febc11"
)

#### Natural gas bar (*should be temporary*) #### 
# Reshape to long format
ng_band <- lcoh_raw_df %>%
  pivot_longer(
    cols = starts_with("scenario"),
    names_to = "scenario",
    values_to = "LCOH"
  ) %>%
  mutate(
    case = case_when(
      str_detect(scenario, "best") ~ "best",
      str_detect(scenario, "worst") ~ "worst"
    ),
    scenario_clean = str_remove(scenario, "_best|_worst"),
    
    # Extract policy labels
    policy_label = case_when(
      str_detect(scenario_clean, "30_cost_share") ~ "30% Capex\nShare",
      str_detect(scenario_clean, "50_cost_share") ~ "50% Capex\nShare",
      str_detect(scenario_clean, "100_cost_share") ~ "100% Capex\nShare",
      str_detect(scenario_clean, "low_rate") ~ "Low Elec.\nRate",
      str_detect(scenario_clean, "100_low") ~ "100% Capex + \nLow Rate",
      str_detect(scenario_clean, "no_policy") ~ "No Policy",
      str_detect(scenario_clean, "natural_gas") ~ "Baseline"
    ),
    
    # Extract scenario labels
    scenario_label = case_when(
      str_detect(scenario_clean, "scenario_1") ~ "Scenario 1",
      str_detect(scenario_clean, "scenario_2") ~ "Scenario 2",
      str_detect(scenario_clean, "scenario_3") ~ "Scenario 3",
      str_detect(scenario_clean, "scenario_4") ~ "Scenario 4",
      str_detect(scenario_clean, "natural_gas") ~ "Baseline"
    )
  ) |>
  filter(scenario_label == "Baseline") %>%
  summarise(
    ng_min = min(LCOH),
    ng_max = max(LCOH)
  )

#### Cross-sector Plot #### 
# Cross join: each row in `params` gets all x values
plot_data <- 
  scenario_4_input |>
  # calculate LCOH by sector, inputting "x" as cost of electricity 
  tidyr::crossing(x = x_vals) |>
  mutate(y = lcoh_func(capex, 0, x, change_in_elec_demand_kwh, heat_mmbtu, t, r))
  

ggplot(plot_data, aes(x = x, y = y, color = naics_description)) +
  geom_line(size = 1) +
  
  # add natural gas line 
  geom_hline(yintercept = ng_band$ng_max, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  annotate("text",
           label = "LCOH of a natural gas boiler",
           x = .01, y = ng_band$ng_max + .5,
           size = 3,
           fontface = 'italic') +
  
  geom_vline(xintercept = 0.092, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  annotate("text",
           label = "MN Price",
           x = .0955, y = 5,
           size = 3,
           fontface = 'italic') +
  
  geom_vline(xintercept = 0.042, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  annotate("text",
           label = "Ethyl Alcohol Cost Parity",
           x = .05, y = 5,
           size = 3,
           fontface = 'italic') +
  
  scale_color_manual(values = sector_colors) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity ($/kWH)",
    color = "Sector") +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )


#### Capex Share Plot #### 

capex_plot_data <- 
  scenario_4_input |>
  filter(naics_description == 'Ethyl Alcohol') |>
  slice(rep(1, 4)) |>
  mutate(capex_share = rep(c(0, .3, .5, 1)), 
         capex_share_lab = rep(c('0% Capex Share', '30% Capex Share', 
                                 '50% Capex Share', '100% Capex Share')), 
         capex_share_lab = factor(capex_share_lab, levels = c('0% Capex Share', '30% Capex Share', 
                                                              '50% Capex Share', '100% Capex Share'), ordered = TRUE)
         ) |> # creating capex share scenarios
  tidyr::crossing(x = x_vals) |>
  mutate(y = lcoh_func(capex, capex_share, x, change_in_elec_demand_kwh, heat_mmbtu, t, r))



ggplot(capex_plot_data, aes(x = x, y = y, color = capex_share_lab)) +
  geom_line(size = 1) +
  
  # add natural gas line 
  geom_hline(yintercept = ng_band$ng_max, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  annotate("text",
           label = "LCOH of a natural gas boiler",
           x = .01, y = ng_band$ng_max + .5,
           size = 3,
           fontface = 'italic') +
  
  geom_vline(xintercept = 0.092, 
             linetype = "dotted", 
             color = "black", 
             size = 0.5) +
  annotate("text",
           label = "MN Price",
           x = .0955, y = 5,
           size = 3,
           fontface = 'italic') +
  
  # geom_vline(xintercept = 0.042, 
  #            linetype = "dotted", 
  #            color = "black", 
  #            size = 0.5) +
  # annotate("text",
  #          label = "Ethyl Alcohol Cost Parity",
  #          x = .05, y = 5,
  #          size = 3,
  #          fontface = 'italic') +
  
  #scale_color_manual(values = sector_colors) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity ($/kWH)",
    color = "Scenario") +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11), 
    legend.background = element_rect(
      color = "black",   
      linewidth = 0.2    
    )
  )
