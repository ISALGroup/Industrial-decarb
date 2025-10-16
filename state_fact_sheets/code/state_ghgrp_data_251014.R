# August 1. 2025
# updated Oct. 14
# EMT
# Filters GHGRP rlps data for a specific state and naics codes
# Creates emissions donut figure for state fact sheets


# Cleaned linear script (no user functions)
library(tidyverse)
library(janitor)
library(readxl)
library(scales)
library(stringr)

setwd("~/Documents/Industrial_Decarbonization/Industrial-decarb")

# ---------- CONFIG ----------
target_state <- "MN"
target_year  <- 2023
states_of_interest <- c("CA", "IL", "NY", "MD", "MI", "MN", "MA", "WI", "CO", "PA", "OR")

sector_colors <- c(
  "Food & Beverage"     = "#c43424",  # 311, 312
  "Chemicals"           = "#047c91",  # 325
  "Pulp & Paper"        = "#6d7d33",  # 322
  "Other Manufacturing" = "#DCE1E5"
)

out_dir <- "state_fact_sheets/outputs"
xfun::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- READ INPUTS (read once) ----------
ghgrp_raw_df <- read_excel("state_fact_sheets/data/raw/rlps_ghg_emitter_subpart_w_NAICS.xlsx") %>%
  clean_names() %>%
  filter(year == target_year) %>%
  mutate(naics_code = as.character(primary_naics)) %>%
  filter(str_starts(naics_code, "31") | str_starts(naics_code, "32") | str_starts(naics_code, "33")) %>%
  filter(!str_starts(naics_code, "324")) %>%
  filter(!is.na(co2e_emission))

target_naics_all <- read_excel("state_fact_sheets/data/raw/target_NAICS.xlsx") %>%
  clean_names() %>%
  mutate(naics_code = as.character(naics_code))

# ---------- MARK TARGET NAICS IN GHGRP ----------
ghgrp_with_target <- ghgrp_raw_df %>%
  left_join(target_naics_all, by = "naics_code") %>%
  mutate(in_target = !is.na(status))

# ---------- OPTIONAL: state-level progress summary (kept minimal) ----------
state_summary <- ghgrp_with_target %>%
  filter(state %in% states_of_interest) %>%
  group_by(state) %>%
  summarise(
    total_emissions = sum(co2e_emission, na.rm = TRUE),
    target_emissions = sum(co2e_emission[in_target], na.rm = TRUE),
    done_emissions = sum(co2e_emission[status == "Done"], na.rm = TRUE),
    ip_or_done_emissions = sum(co2e_emission[in_target & status %in% c("Done", "In Progress")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_target_of_total = round(100 * target_emissions / total_emissions, 1),
    pct_done_of_target = ifelse(target_emissions > 0, round(100 * done_emissions / target_emissions, 1), NA_real_),
    pct_ip_or_done_of_target = ifelse(target_emissions > 0, round(100 * ip_or_done_emissions / target_emissions, 1), NA_real_)
  ) %>%
  select(state, pct_target_of_total, pct_done_of_target, pct_ip_or_done_of_target)

# ---------- MAP SECTORS & CLEAN DESCRIPTIONS ----------
ghgrp_mapped <- ghgrp_raw_df %>%
  mutate(
    sector = case_when(
      str_starts(naics_code, "311") | str_starts(naics_code, "312") ~ "Food & Beverage",
      str_starts(naics_code, "325") ~ "Chemicals",
      str_starts(naics_code, "322") ~ "Pulp & Paper",
      TRUE ~ "Other Manufacturing"
    )
  ) %>%
  left_join(target_naics_all %>% rename(naics_description = description), by = "naics_code") %>%
  mutate(
    naics_description = if_else(is.na(naics_description), "Other Manufacturing", naics_description),
    # refine ambiguous "Other Manufacturing" descriptions using the prefix
    naics_description = case_when(
      naics_description == "Other Manufacturing" & str_starts(naics_code, "311") ~ "Other Food & Beverage Manufacturing",
      naics_description == "Other Manufacturing" & str_starts(naics_code, "312") ~ "Other Food & Beverage Manufacturing",
      naics_description == "Other Manufacturing" & str_starts(naics_code, "322") ~ "Other Pulp & Paper Manufacturing",
      naics_description == "Other Manufacturing" & str_starts(naics_code, "325") ~ "Other Chemicals Manufacturing",
      TRUE ~ naics_description
    )
  )

# ---------- OUTPUT TABLE FOR SELECTED STATE ----------
mapped_state_df <- ghgrp_mapped %>%
  filter(state == target_state) %>%
  select(naics_code, naics_description, co2e_emission, facility_id, facility_name)

output_table <- mapped_state_df %>%
  group_by(naics_description, naics_code) %>%
  summarise(co2e_total = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent_of_total = round(100 * co2e_total / sum(co2e_total), 2)) %>%
  arrange(desc(co2e_total))

# ---------- DONUT DATA (state-specific) ----------
donut_data <- mapped_state_df %>%
  group_by(naics_description) %>%
  summarise(emission = sum(co2e_emission, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(emission)) %>%
  mutate(pct = emission / sum(emission) * 100,
         slice_order = row_number())

donut_chart <- ggplot(donut_data, aes(x = 2, y = emission, fill = naics_description)) +
  geom_col(color = "white", width = 1, size = 0.3) +
  coord_polar(theta = "y", start = pi/2) +
  theme_void() +
  xlim(0.5, 2.5) +
  labs(title = paste0(target_state, " Manufacturing CO2 Emissions by NAICS (", target_year, ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(donut_chart)

# ---------- STACKED BAR: build plot data for many states ----------
# If you want to restrict to a particular modeled list of NAICS, set modeled_naics (comment out if not needed)
modeled_naics <- c(311221, 325193, 311313, 322120, 311224, 325110, 325311, 312140, 311611, 311225, 
                   325180, 325194, 322130, 311421, 325211, 311513, 311514, 311314, 311942, 311613, 
                   312120, 325120, 311423, 325312, 325212, 322110, 311411, 311615, 322291, 311511, 
                   311422, 311919, 311230) %>% as.character()

ghgrp_filtered <- ghgrp_mapped %>%
  filter(state %in% states_of_interest) %>%
  #filter(is.na(naics_code) | naics_code %in% modeled_naics) %>%
  mutate(co2e_emissions_mmt = co2e_emission / 1e6) %>%
  select(state, naics_code, naics_description, sector, co2e_emissions_mmt)

# Aggregate by state × NAICS
plot_df <- ghgrp_filtered %>%
  group_by(state, naics_description, naics_code) %>%
  summarise(total_co2e = sum(co2e_emissions_mmt, na.rm = TRUE), .groups = "drop")

# Keep top N NAICS nationally; others collapsed
top_n_categories <- 12
top_cats <- plot_df %>%
  group_by(naics_description, naics_code) %>%
  summarise(national_total = sum(total_co2e, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(national_total)) %>%
  slice_head(n = top_n_categories) %>%
  pull(naics_description)

plot_df2 <- plot_df %>%
  mutate(naics_desc_trim = if_else(naics_description %in% top_cats, naics_description, "Other in-scope sectors")) %>%
  group_by(state, naics_desc_trim) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = TRUE), .groups = "drop") %>%
  rename(naics_description = naics_desc_trim)

# Order states by total emissions
state_order <- plot_df2 %>%
  group_by(state) %>%
  summarise(state_total = sum(total_co2e, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(state_total)) %>%
  pull(state)

# Build unique descriptions and sector mapping
naics_sector_lookup <- ghgrp_filtered %>%
  distinct(naics_description, sector) %>%
  filter(!is.na(naics_description))

unique_descs <- plot_df2 %>%
  distinct(naics_description) %>%
  left_join(naics_sector_lookup, by = "naics_description") %>%
  mutate(
    sector = case_when(
      naics_description == "Other in-scope sectors" ~ "Other in-scope",
      is.na(sector) & str_detect(naics_description, "Other") ~ "Other Manufacturing",
      TRUE ~ sector
    )
  ) %>%
  arrange(sector, naics_description)

# Create color mapping per NAICS description using the sector seeds
sector_palette_seeds <- list(
  `Food & Beverage`      = c("#ef5645", "#c43424"),
  `Chemicals`            = c("#c2e4e6", "#047c91"),
  `Pulp & Paper`         = c("#c7d3a1", "#6d7d33"),
  `Other Manufacturing`  = c("#f1eeea"),
  `Other in-scope`       = c("#febc11")
)

make_shades <- function(n, light, dark = NULL) {
  if (n <= 1) return(ifelse(is.null(dark), light, dark))
  if (is.null(dark)) return(rep(light, n))
  colorRampPalette(c(light, dark))(n)
}

color_map <- unique_descs %>%
  group_by(sector) %>%
  summarise(descs = list(naics_description), .groups = "drop") %>%
  mutate(
    n = map_int(descs, length),
    shades = map2(sector, n, ~ {
      seeds <- sector_palette_seeds[[.x]]
      if (is.null(seeds)) return(rep("#f1eeea", .y))
      if (length(seeds) == 1) return(make_shades(.y, seeds[1], NULL))
      make_shades(.y, seeds[1], seeds[2])
    }),
    mapping = map2(descs, shades, ~ tibble(naics_description = .x, color = .y))
  ) %>%
  pull(mapping) %>%
  bind_rows()

# Ensure consistent ordering for legend & fill
legend_order <- unique_descs %>% pull(naics_description)
final_levels <- c(setdiff(legend_order, c("Other in-scope sectors", "Other Manufacturing")), "Other in-scope sectors", "Other Manufacturing")
color_map <- color_map %>% filter(naics_description %in% final_levels) %>%
  mutate(naics_description = factor(naics_description, levels = final_levels))

fill_colors <- set_names(color_map$color, color_map$naics_description)

# Prepare plotting dataframe and add the index used for legend + annotation
plot_df2_for_plot <- plot_df2 %>%
  left_join(tibble(naics_description = final_levels) %>% mutate(naics_index = row_number()), by = "naics_description") %>%
  mutate(
    naics_index = if_else(is.na(naics_index), as.integer(nrow(tibble(naics_description = final_levels)) + 1), naics_index),
    state = factor(state, levels = state_order),
    naics_description = factor(naics_description, levels = final_levels)
  ) %>%
  left_join(color_map, by = "naics_description") %>%
  mutate(color = coalesce(color, "#808080"))

# Create legend labels like "1 — NAICS description"
legend_labels <- set_names(
  paste0(seq_along(final_levels), " \u2014 ", final_levels),
  final_levels
)

# Reorder fill_colors to match final_levels
fill_colors <- fill_colors[final_levels]
missing_cols <- setdiff(final_levels, names(fill_colors))
if (length(missing_cols) > 0) fill_colors[missing_cols] <- "#808080"

# Compute percent of state bars for thresholding label display
plot_df2_for_plot <- plot_df2_for_plot %>%
  group_by(state) %>%
  mutate(state_total = sum(total_co2e, na.rm = TRUE),
         frac = if_else(state_total > 0, total_co2e / state_total, 0)) %>%
  ungroup()


# ---------- PLOT: stacked bar with larger text for saving ----------
# Adjust this value to scale all text up/down (1 = default, 2 = twice as large, etc.)
text_scale <- 1.8

# Derived sizes (tweak if desired)
#base_font           <- 12 
title_size          <- 24
subtitle_size       <- 20
axis_text_size      <- 20
axis_title_size     <- 20
legend_title_size   <- 24
legend_text_size    <- 18
index_text_size     <- 6   # numbers inside segments
top_label_size      <- 6 # percent labels above bars

# Recompute label_df if needed (keeps previous logic)
label_df <- plot_df2_for_plot %>%
  group_by(state) %>%
  summarise(
    total = sum(total_co2e, na.rm = TRUE),
    other_manuf = sum(total_co2e[naics_description == "Other Manufacturing"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_not_other = if_else(total > 0, round(100 * (1 - other_manuf / total), 1), NA_real_),
    label = paste0(pct_not_other, "%"),
    y_pos = total * 1.02
  )

# threshold for showing index labels inside segments
label_threshold <- 0.03

p_stacked <- ggplot(plot_df2_for_plot, aes(x = state, y = total_co2e, fill = naics_description)) +
  geom_col(width = 0.75, color = "grey90", size = 0.2) +
  # index numbers inside segments (where there's space)
  geom_text(
    data = plot_df2_for_plot %>% filter(frac >= label_threshold),
    aes(label = naics_index),
    position = position_stack(vjust = 0.5),
    size = index_text_size,
    fontface = "bold",
    color = "white",
    show.legend = FALSE
  ) +
  # top-of-bar percent labels (precomputed)
  geom_text(
    data = label_df,
    aes(x = state, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    size = top_label_size,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = fill_colors, name = "Index — Subsector", labels = legend_labels) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste0("Manufacturing CO2e Emissions by Subsector (GHGRP ", target_year, ")"),
    subtitle = "Legend shows index number — matching numbers appear on the bar segments.",
    x = NULL, y = expression("CO"[2]*"e emissions (MMT)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    plot.subtitle = element_text(size = subtitle_size),
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size),
    #axis.title.x = element_text(size = axis_title_size),
    axis.title.y = element_text(size = axis_title_size),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size)#,
    #legend.key.size = unit(1.0 * text_scale, "lines")
  )

p_stacked
# Save with larger physical size and optional scale multiplier
save_scale  <- 1   # you can bump to 1.1-1.4 if you want even larger

# save
out_file <- file.path(out_dir, paste0("ghgrp_stacked_by_naics_state_", format(Sys.Date(), "%y%m%d"), ".png"))
ggsave(out_file, p_stacked, width = 16, height = 8, dpi = 300, scale=save_scale)

# ---------- OUTPUTS ----------
writexl::write_xlsx(output_table, "state_fact_sheets/data/modified/mn_emissions_summary_250805.xlsx")
writexl::write_xlsx(naics_coverage_by_state, "state_fact_sheets/data/modified/percent_emissions_models_finished_.xlsx")
writexl::write_xlsx(output_table2, "state_fact_sheets/data/modified/mi_emissions_finished_naics_summary_250730_.xlsx")
ggsave("state_fact_sheets/outputs/mn_donut_chart_250805.png", donut_chart, width = 8, height = 6, dpi = 300)


# ------- TOOLING AROUND --------




