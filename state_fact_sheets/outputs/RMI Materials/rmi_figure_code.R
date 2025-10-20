

library(readr)
library(ggplot2)
library(dplyr)

elec_plot_df <- read_csv('state_fact_sheets/outputs/RMI Materials/elec_plot_data.csv')
param_rmi <- read_csv('state_fact_sheets/outputs/RMI Materials/rmi_parameters.csv')

##### ILLINOIS PLOT #######

elec_plot_df_il <- 
  elec_plot_df |>
  select(x, scenario_rank, lcoh_il) |>
  pivot_wider(
    names_from = scenario_rank, 
    names_prefix = "lcoh_",
    values_from = lcoh_il
  ) |>
  mutate(
    lcoh_Mean = (lcoh_Best + lcoh_Worst) / 2
  )

# Get the points at which the heat pump LCOH line intersects the NG LCOH 
ng_x_il_max <- with(elec_plot_df_il, approx(lcoh_Best, x, xout = param_rmi$ng_max[1]))$y
ng_x_il_min <- with(elec_plot_df_il, approx(lcoh_Worst, x, xout = param_rmi$ng_min[1]))$y
ng_x_il_mean <- (ng_x_il_max + ng_x_il_min) / 2


lcoh_v_elec_plot_il <- 
  ggplot(data = elec_plot_df_il) +
  geom_ribbon(aes(x = x, ymin = lcoh_Best, ymax = lcoh_Worst),
              fill = "grey90", alpha = 0.3) +
  # Main LCOH curve
  geom_line(aes(x = x, y = lcoh_Mean, color = "Heat Pump", linetype = "Heat Pump"),
            size = 1) +
  # geom_line(aes(x = x, y = Worst, color = "Heat Pump", linetype = "Heat Pump"),
  #           size = 1) +
  
  # NG Boiler reference line
  annotate("rect", xmin = 0, xmax = 15, ymin = param_rmi$ng_min[1], ymax = param_rmi$ng_max[1],
           fill = "grey90", alpha = 0.3) +
  geom_segment(aes(x = 0, xend = 15,
                   y = (param_rmi$ng_max[1] + param_rmi$ng_min[1]) / 2, 
                   yend = (param_rmi$ng_max[1] + param_rmi$ng_min[1]) / 2,
                   color = "Natural Gas Boiler",
                   linetype = "Natural Gas Boiler"),
               size = 0.75) +

  # Current electricity price 
  annotate("rect", xmin = param_rmi$elec_price_low[1] * 100, xmax = param_rmi$elec_price_high[1] * 100,
           ymin = -Inf, ymax = Inf,
           fill = "#FFBF00", alpha = 0.1) +
  geom_vline(xintercept = param_rmi$elec_price_mean[1] * 100, 
             color =  "#FFBF00", size = 0.5) +

  # Intersection 
  annotate("rect", xmin = ng_x_il_min, xmax = ng_x_il_max,
           ymin = -Inf, ymax = Inf,
           fill = "#FFBF00", alpha = 0.1) +
  geom_vline(xintercept = ng_x_il_mean, color = "#FFBF00", linetype = "longdash", size = 0.75) +

  # Reverse x-axis
  scale_x_reverse(limits = c(15, 0)) +
  
  # Unified legend with manual scales
  scale_color_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "#004b79",
      "Natural Gas Boiler" = "#c4d5e7"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "solid",
      "Natural Gas Boiler" = "solid"
    )
  ) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity (¢/kWH)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    #legend.position = c(0.75, 0.95),
    #legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2
    ), 
    panel.grid = element_blank() 
  )

###### MICHIGAN PLOT #####
# Get the point at which the heat pump LCOH line intersects the NG LCOH 
ng_x_intercept_mi <- with(elec_plot_df, approx(lcoh_mi, x, xout = param_rmi$ng_max[2]))$y

lcoh_v_elec_plot_mi <- 
  ggplot() +
  # Main LCOH curve
  geom_line(data = elec_plot_df,
            aes(x = x, y = lcoh_mi, color = "Heat Pump", linetype = "Heat Pump"),
            size = 1) +
  
  # NG Boiler reference line
  geom_hline(aes(yintercept = param_rmi$ng_max[2],
                 color = "Natural Gas Boiler",
                 linetype = "Natural Gas Boiler"),
             size = 0.75) +
  
  # Other vlines not in legend
  geom_vline(xintercept = param_rmi$elec_price[2] * 100, color =  "#FFBF00", size = 0.5) +
  geom_vline(xintercept = ng_x_intercept_mi, color = "#FFBF00", linetype = "longdash", size = 0.75) +
  
  # Reverse x-axis
  scale_x_reverse(limits = c(15, 0)) +
  
  # Unified legend with manual scales
  scale_color_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "#004b79",
      "Natural Gas Boiler" = "#c4d5e7"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Heat Pump" = "solid",
      "Natural Gas Boiler" = "solid"
    )
  ) +
  
  labs(
    y = "Levelized Cost of Heat ($/MMBtu)",
    x = "Cost of Electricity (¢/kWH)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.75, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.2
    ), 
    panel.grid = element_blank() 
  )

