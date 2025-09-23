library(tidyverse)

# Data
df_wide <- tribble(
  ~Sector,          ~`<80C`, ~`80-150C`, ~`150-300C`, ~`300-550C`, ~`550-1100C`, ~`>1100C`,
  "Chemicals",       100,      1800,        500,         100,         700,          0,
  "Pulp and paper",  800,      200,         300,         0,           1000,         0,
  "Food",            500,      400,         0,           0,           0,            0,
  "Iron and steel",  0,        0,           200,         0,           100,          300,
  "Cement",          0,        0,           30,          0,           0,            150
)

# Reformat for ggplot
df_long <- df_wide %>%
  pivot_longer(-Sector, names_to = "TemperatureRange", values_to = "Tbtu") %>%
  mutate(
    Sector = factor(Sector, levels = c("Chemicals", "Pulp and paper", "Food", "Iron and steel", "Cement")),
    TemperatureRange = factor(TemperatureRange,
                              levels = rev(c("<80C", "80-150C", "150-300C", "300-550C", "550-1100C", ">1100C"))
    )
  )

# Temperature colors
temp_colors <- c(
  "<80C"       = "#66e2c8",
  "80-150C"    = "#007c7a",
  "150-300C"   = "#5193b3",
  "300-550C"   = "#d9d9d9",
  "550-1100C"  = "#bdbdbd",
  ">1100C"     = "#969696"
)

# Plot
ggplot(df_long, aes(x = Sector, y = Tbtu, fill = TemperatureRange)) +
  geom_col(width = 0.8) +  # Wider spacing between bars
  scale_fill_manual(
    values = temp_colors,
    name = "Temperature Range",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    y = "Process Heat Energy Used (Tbtu)",
    x = NULL
  ) +
  scale_fill_manual(
    values = temp_colors,
    name = "Temperature Range",
    guide = guide_legend(
      reverse = TRUE,
      nrow = 1
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 30, 10)  # Add space below for long legend
  )

