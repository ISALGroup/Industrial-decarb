# NEI-GHGRP Facility Data Merger

A Python script for merging EPA emissions datasets with a focus on industrial facilities.
This tool merges facility-level data from two EPA datasets:
- **NEI (National Emissions Inventory)**: Criteria pollutant emissions
- **GHGRP (Greenhouse Gas Reporting Program)**: Greenhouse gas emissions

The script uses fuzzy string matching to identify the same facilities across both datasets, focusing on seven major industrial sectors.

This work was conducted as part of the UCSB 2035 Initiative under the Industrial Decarbonization Lab, led by Professor Eric Masanet. I was a part-time research assistant in the lab. I appreciate the opportunity to contribute to this broader research effort.

## Features

- **Automatic column detection**: Adapts to different file formats
- **Fuzzy facility matching**: Uses name, address, and location similarity
- **Industry-specific filtering**: Focuses on relevant industrial sectors
- **Robust error handling**: Continues processing even with data issues
- **Clean output format**: Standardized column names and data types

## Industrial Sectors

1. Adipic Acid Production
2. Ammonia Manufacturing
3. Industrial Wastewater Treatment
4. Petrochemical Production
5. Phosphoric Acid Production
6. Pulp and Paper Manufacturing
7. Stationary Combustion

## Requirements

```
pandas>=1.3.0
numpy>=1.21.0
openpyxl>=3.0.7
fuzzywuzzy>=0.18.0
python-Levenshtein>=0.12.2
```

## Usage

1. Place the script and data files in the same directory:
   ```
   merge_nei_ghgrp.py
   2020Facility-LevelDataforPointEmissions.xlsx
   2020_GHGRP_subpart_facilities.xlsx
   ```

2. Run the script:
   ```bash
   python merge_nei_ghgrp.py
   ```

3. Check the output file:
   ```
   final_pollutant_data.xlsx
   ```

## Output Dataset

The merged dataset includes:

### Facility Information
- Facility ID and name
- Address and geographic coordinates
- Industry sector and classification codes

### NEI Emissions (short tons/year) - converted to metric tons by multiplying 0.907
- PM2.5, PM10, NOx, SO2, VOC, CO, NH3...

### GHGRP Emissions (metric tons CO2e/year)
- CO2, CH4, N2O, Total GHG

### Data Quality Metrics
- Match confidence scores
- Individual similarity metrics

## Algorithm Details

### Matching Process
1. **Data Cleaning**: Standardize facility names and addresses
2. **Similarity Scoring**: Calculate weighted similarity scores
3. **Best Match Selection**: Choose highest-scoring matches above threshold
4. **Quality Assessment**: Assign confidence levels to matches

### Similarity Weights
- Facility Name: 40%
- Street Address: 30%
- Geographic Location: 20%
- State Match: 10%

### Confidence Levels
- **High**: Score ≥ 85 (Very likely same facility)
- **Medium**: Score 60-84 (Probable match)
- **Low**: Score < 60 (Excluded from results)

## Expected Results

- **~6,000-8,000** matched facilities
- **7 industrial sectors** represented
- **All 50 states** covered (varies by industry)
- **15+ emission types** included

## Technical Notes

### Column Detection
The script automatically detects column names to handle variations in EPA data formats:
- NEI: Maps "EIS Facility ID", "Site Name", "NAICS", etc.
- GHGRP: Maps "facility_id", "facility_name", "address1", etc.

### Data Aggregation
- **NEI**: Pivots pollutant data to create one row per facility
- **GHGRP**: Aggregates emissions by gas type and facility

### Error Handling
- Continues processing if some columns are missing
- Logs warnings for data quality issues
- Provides fallback options for aggregation failures

## Data Sources

- **NEI Data**: US EPA National Emissions Inventory (2020)
- **GHGRP Data**: US EPA Greenhouse Gas Reporting Program (2020)

Both datasets are publicly available from EPA's website.

## License

MIT License - See LICENSE file for details.

## Author
Mikey He
UCSB

