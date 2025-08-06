# Industrial Electrification Data Documentation

This dataset (`facility_level_long_df`) contains facility-level estimates of levelized cost of heat (LCOH) and air emissions under a range of policy and grid decarbonization scenarios. It is built from technoeconomic and emissions inputs for industrial facilities and assumes a 15-year project lifetime with a 6.5% discount rate.

## Purpose

The dataset enables analysis of how different combinations of capital expenditure subsidies, electricity price reductions, and grid emissions intensities affect:
  - the cost of clean heating technologies, and
- emissions of CO₂e, NOx, SO₂, and PM2.5 from electrification.

---
  
  ## Column Definitions
  
  ### Facility Metadata
- **facility_id**: Unique identifier from the EPA GHG Reporting Program (GHGRP).
- **facility_name**: Lowercase facility name for consistent matching.
- **state**: U.S. state where the facility is located.
- **county_fips**: 5-digit FIPS code identifying the facility’s county.
- **latitude**: Geographic latitude of the facility.
- **longitude**: Geographic longitude of the facility.
- **naics_code**: 6-digit primary industry classification code.
- **naics_description**: Description of the NAICS code.
- **sector**: Broad industrial sector grouping.
- **subregion**: EPA eGRID subregion where the facility is located.

### Emissions Baseline
- **baseline_co2e_emissions**: Total facility CO₂e emissions before electrification (kg CO₂e). Electrifiable + non-electrifiable emissions.

### Technoeconomic Inputs
- **tech_scenario**: technology scenario with 5 scenarios: 
  - baseline 
  - scenario 1 (e-boiler)
  - scenario 2 (e-boiler + energy efficiency upgrades)
  - scenario 3 (air source heat pump)
  - scenario 4 (air source heat pump + energy efficiency upgrades)
- **change_in_electricity_kwh**: change in kWh at the plant due to electrification (0 for baseline)
- **heat_mmbtu**: Annual clean heat output required (MMBtu) (electrifiable energy only).
- **capex_adj**: Adjusted capital expenditure under the policy scenario ($).
- **capex_subsidy**: Fraction of capital cost subsidized (0–1).
- **elec_price_adj**: Adjusted electricity price under the policy scenario ($/kWh).
- **opex_adj**: Adjusted operating costs under the policy scenario ($/yr).
- **lcoh**: Levelized cost of heat under the policy and grid scenario ($/MMBtu).
- **policy_label**: Label describing the scenario (e.g., "Capex subsidy: 30%, Elec: -20%").

### Grid Information
- **current_fossil_share**: Share of grid electricity currently from fossil sources in the subregion.
- **current_clean_share**: Share of grid electricity from clean sources in the subregion.
- **grid_clean_pct_scenario**: Scenario value for percent of clean electricity (e.g., 0.5 for 50%). Includes "Current Mix" as baseline.
- **clean_grid_scenario_label**: Readable label for grid mix scenario (e.g., "80% Clean Grid", "Current Grid Mix").

### Emissions Outputs
(All emissions are calculated based on electricity demand changes from electrification and assumed grid intensity scenario.)

- **emissions_total_mt_co2e**: Net facility CO₂e emissions under the scenario (metric tons CO₂e/year).
- **emissions_change_kg_nox**: Change in NOx emissions from electricity consumption (kg/year).
- **emissions_change_kg_so2**: Change in SO₂ emissions from electricity consumption (kg/year).
- **emissions_change_kg_pm25**: Change in PM2.5 emissions from electricity consumption (kg/year).


---
  
  ## Notes
  
- LCOH is calculated over a 15-year lifetime with a 6.5% discount rate.
- Emissions factors are scaled linearly with fossil generation share, assuming non-fossil sources are zero-emissions.
- The baseline includes all original facility emissions before electrification.

