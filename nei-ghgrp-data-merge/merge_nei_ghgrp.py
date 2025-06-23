#!/usr/bin/env python3
"""
NEI-GHGRP Facility Data Merger - Pipeline
Author: Mikey He
Date: 2025

This script integrates the successful pipeline from the original working scripts I coded.

Required files in working directory:
    - 2020Facility-LevelDataforPointEmissions.xlsx
    - 2020_GHGRP_subpart_facilities.xlsx

Output:
    - final_pollutant_data.xlsx
"""

import pandas as pd
import numpy as np
from fuzzywuzzy import fuzz
import re
import os
import logging
from datetime import datetime
from tqdm import tqdm
import warnings

warnings.filterwarnings('ignore')

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class NEIGHGRPMerger:
    """
    Complete NEI-GHGRP data merger based on the working pipeline.
    
    This class implements the exact same logic as the successful
    multi-stage scripts that were previously working.
    """
    
    def __init__(self):
        # Valid subparts from the working GHGRP script
        self.valid_subparts = [
            "Pulp and Paper Manufacturing",
            "Ammonia Manufacturing", 
            "Petrochemical Production",
            "Phosphoric Acid Production",
            "Adipic Acid Production",
            "Industrial Wastewater Treatment",
            "Stationary Combustion"
        ]
        
        # Gases to remove from NEI (from working script)
        self.gases_to_remove = ["Carbon Dioxide", "Methane", "Nitrous Oxide"]
        
        # Matching thresholds from working script
        self.distance_threshold = 20.0  # km
        self.high_confidence_threshold = 70
        self.review_threshold = 50
        
        # Column name mappings to handle variations
        self.nei_emission_col = None
        self.ghgrp_emission_col = None
        
        # State mapping for standardization
        self.state_mapping = {
            'ALABAMA': 'AL', 'ALA': 'AL', 'ALASKA': 'AK', 'ARIZONA': 'AZ', 'ARIZ': 'AZ',
            'ARKANSAS': 'AR', 'ARK': 'AR', 'CALIFORNIA': 'CA', 'CALIF': 'CA', 'CAL': 'CA',
            'COLORADO': 'CO', 'COLO': 'CO', 'CONNECTICUT': 'CT', 'CONN': 'CT',
            'DELAWARE': 'DE', 'DEL': 'DE', 'FLORIDA': 'FL', 'FLA': 'FL', 'GEORGIA': 'GA',
            'HAWAII': 'HI', 'IDAHO': 'ID', 'ILLINOIS': 'IL', 'ILL': 'IL',
            'INDIANA': 'IN', 'IND': 'IN', 'IOWA': 'IA', 'KANSAS': 'KS', 'KAN': 'KS',
            'KENTUCKY': 'KY', 'KY': 'KY', 'LOUISIANA': 'LA', 'LA': 'LA',
            'MAINE': 'ME', 'MARYLAND': 'MD', 'MD': 'MD', 'MASSACHUSETTS': 'MA', 'MASS': 'MA',
            'MICHIGAN': 'MI', 'MICH': 'MI', 'MINNESOTA': 'MN', 'MINN': 'MN',
            'MISSISSIPPI': 'MS', 'MISS': 'MS', 'MISSOURI': 'MO', 'MO': 'MO',
            'MONTANA': 'MT', 'MONT': 'MT', 'NEBRASKA': 'NE', 'NEB': 'NE', 'NEVADA': 'NV',
            'NEW HAMPSHIRE': 'NH', 'N.H.': 'NH', 'N H': 'NH', 'NEW JERSEY': 'NJ', 'N.J.': 'NJ', 'N J': 'NJ',
            'NEW MEXICO': 'NM', 'N.M.': 'NM', 'N M': 'NM', 'NEW YORK': 'NY', 'N.Y.': 'NY', 'N Y': 'NY',
            'NORTH CAROLINA': 'NC', 'N.C.': 'NC', 'N C': 'NC', 'NORTH DAKOTA': 'ND', 'N.D.': 'ND', 'N D': 'ND',
            'OHIO': 'OH', 'OKLAHOMA': 'OK', 'OKLA': 'OK', 'OREGON': 'OR', 'ORE': 'OR',
            'PENNSYLVANIA': 'PA', 'PENN': 'PA', 'PA': 'PA', 'RHODE ISLAND': 'RI', 'R.I.': 'RI', 'R I': 'RI',
            'SOUTH CAROLINA': 'SC', 'S.C.': 'SC', 'S C': 'SC', 'SOUTH DAKOTA': 'SD', 'S.D.': 'SD', 'S D': 'SD',
            'TENNESSEE': 'TN', 'TENN': 'TN', 'TEXAS': 'TX', 'TEX': 'TX', 'UTAH': 'UT',
            'VERMONT': 'VT', 'VT': 'VT', 'VIRGINIA': 'VA', 'VA': 'VA', 'WASHINGTON': 'WA', 'WASH': 'WA',
            'WEST VIRGINIA': 'WV', 'W.VA.': 'WV', 'W VA': 'WV', 'WISCONSIN': 'WI', 'WIS': 'WI', 'WISC': 'WI',
            'WYOMING': 'WY', 'WYO': 'WY',
            'PUERTO RICO': 'PR', 'P.R.': 'PR', 'P R': 'PR'
        }
    
    def find_emission_column(self, df, dataset_name):
        """Find the correct emission column name in the dataset."""
        possible_names = [
            'Emission Tons', 'Emissions Tons', 'emission_tons', 'emissions_tons',
            'Emission_Tons', 'Emissions_Tons', 'ghg_quantity', 'GHG Quantity'
        ]
        
        for col_name in possible_names:
            if col_name in df.columns:
                print(f"    Found {dataset_name} emission column: '{col_name}'")
                return col_name
        
        print(f"    Warning: No emission column found in {dataset_name}")
        print(f"    Available columns: {list(df.columns)}")
        return None
    
    def run_complete_pipeline(self):
        """Run the complete pipeline from raw data to final output."""
        print("=" * 70)
        print("NEI-GHGRP Complete Data Merger Pipeline (Fixed Version)")
        print("=" * 70)
        start_time = datetime.now()
        
        try:
            # Step 1: Load and clean data
            print("\nStep 1: Loading and cleaning data...")
            nei_clean = self.clean_nei_data("2020Facility-LevelDataforPointEmissions.xlsx")
            ghgrp_clean = self.clean_ghgrp_data("2020_GHGRP_subpart_facilities.xlsx")
            
            # Detect emission column names after cleaning
            self.nei_emission_col = self.find_emission_column(nei_clean, "NEI")
            self.ghgrp_emission_col = self.find_emission_column(ghgrp_clean, "GHGRP")
            
            if not self.nei_emission_col or not self.ghgrp_emission_col:
                raise ValueError("Could not find emission columns in one or both datasets")
            
            # Step 2: Match facilities
            print("\nStep 2: Matching facilities...")
            matched_facilities = self.match_facilities(nei_clean, ghgrp_clean)
            
            # Step 3: Create final pollutant dataset
            print("\nStep 3: Creating final pollutant dataset...")
            final_data = self.create_pollutant_dataset(matched_facilities, nei_clean, ghgrp_clean)
            
            # Step 4: Save results
            output_file = "final_pollutant_data.xlsx"
            final_data.to_excel(output_file, index=False)
            
            # Print summary
            runtime = datetime.now() - start_time
            print(f"\n" + "=" * 70)
            print("PIPELINE COMPLETED SUCCESSFULLY!")
            print("=" * 70)
            print(f"Runtime: {runtime}")
            print(f"Final dataset: {len(final_data):,} facility-pollutant records")
            print(f"Unique facilities: {final_data['facility_name'].nunique():,}")
            print(f"Output saved to: {output_file}")
            
            # Show data breakdown
            if 'data_source' in final_data.columns:
                source_counts = final_data['data_source'].value_counts()
                print(f"\nData source breakdown:")
                for source, count in source_counts.items():
                    print(f"  {source}: {count:,} records")
            
            if 'pollutant_type' in final_data.columns:
                pollutant_type_counts = final_data['pollutant_type'].value_counts()
                print(f"\nPollutant type breakdown:")
                for ptype, count in pollutant_type_counts.items():
                    print(f"  {ptype}: {count:,} records")
            
            return True
            
        except Exception as e:
            print(f"\nPipeline failed: {str(e)}")
            logger.exception("Pipeline failed")
            return False
    
    def clean_nei_data(self, filename):
        """Clean NEI data following the exact working script logic."""
        print(f"  Loading NEI data from {filename}...")
        df = pd.read_excel(filename)
        print(f"    Raw NEI records: {len(df):,}")
        
        # Drop columns as in working script
        df = df.drop(columns=["State-County", "Pollutant Type"], errors="ignore")
        
        # Remove greenhouse gases as in working script
        df = df[~df["Pollutant"].str.strip().str.lower().isin([g.lower() for g in self.gases_to_remove])]
        print(f"    After removing greenhouse gases: {len(df):,}")
        
        # Find and convert emissions column
        emission_col = self.find_emission_column(df, "NEI (before cleaning)")
        if emission_col:
            # Convert emissions from short tons to metric tons (multiply by 0.907)
            df[emission_col] = df[emission_col] * 0.907
            
            # Standardize column name to 'Emission Tons' for consistency
            if emission_col != 'Emission Tons':
                df = df.rename(columns={emission_col: 'Emission Tons'})
        
        # Reorder columns as in working script
        cols_to_move = ["Facility Type", "EIS Facility ID", "NAICS"]
        other_cols = [col for col in df.columns if col not in cols_to_move]
        df = df[other_cols + cols_to_move]
        
        print(f"    Cleaned NEI dataset: {len(df):,} records")
        return df
    
    def clean_ghgrp_data(self, filename):
        """Clean GHGRP data following the exact working script logic."""
        print(f"  Loading GHGRP data from {filename}...")
        df = pd.read_excel(filename)
        print(f"    Raw GHGRP records: {len(df):,}")
        
        # Filter year == 2020
        df = df[df["year"] == 2020]
        print(f"    After year filter: {len(df):,}")
        
        # Rename columns as in working script
        rename_dict = {
            "state_name": "State",
            "latitude": "Lat", 
            "longitude": "Lon",
            "address1": "Street Address",
            "county_fips": "FIPS",
            "facility_name": "Site Name",
            "gas_name": "Pollutant",
            "ghg_quantity": "Emission Tons"  # Standardize to 'Emission Tons'
        }
        df = df.rename(columns=rename_dict)
        
        # Title case for state names
        df["State"] = df["State"].str.title()
        
        # Filter by valid subparts
        df = df[df["subpart_category"].isin(self.valid_subparts)]
        print(f"    After subpart filter: {len(df):,}")
        
        # Remove biogenic CO2
        df = df[~df["Pollutant"].str.strip().str.lower().eq("biogenic co2")]
        print(f"    After removing biogenic CO2: {len(df):,}")
        
        print(f"    Cleaned GHGRP dataset: {len(df):,} records")
        return df
    
    def clean_address(self, address):
        """Clean and standardize address strings."""
        if pd.isna(address):
            return ""
        
        address = str(address).lower()
        address = re.sub(r'[^\w\s]', ' ', address)
        address = re.sub(r'\s+', ' ', address)
        return address.strip()
    
    def clean_name(self, name):
        """Clean and standardize facility names."""
        if pd.isna(name):
            return ""
        
        name = str(name).lower()
        name = re.sub(r'[^\w\s]', ' ', name)
        name = re.sub(r'\s+', ' ', name)
        return name.strip()
    
    def standardize_state(self, state):
        """Standardize state codes."""
        if pd.isna(state):
            return ""
        
        state = str(state).strip().upper()
        
        if state in self.state_mapping:
            return self.state_mapping[state]
        elif len(state) == 2:
            return state
        else:
            return state
    
    def standardize_fips(self, fips):
        """Standardize FIPS codes."""
        if pd.isna(fips):
            return ""
        
        fips_str = re.sub(r'[^\d]', '', str(fips))
        
        if len(fips_str) <= 5:
            return fips_str.zfill(5)
        else:
            return fips_str[:5]
    
    def calculate_coordinate_distance(self, lat1, lon1, lat2, lon2):
        """Calculate distance between coordinates using Haversine formula."""
        if pd.isna(lat1) or pd.isna(lon1) or pd.isna(lat2) or pd.isna(lon2):
            return float('inf')
        
        lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
        
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
        c = 2 * np.arcsin(np.sqrt(a))
        r = 6371  # Earth radius in km
        return c * r
    
    def match_facilities(self, nei_df, ghgrp_df):
        """Match facilities between NEI and GHGRP datasets."""
        print("  Extracting unique facilities...")
        nei_facilities = nei_df.drop_duplicates('Site Name').copy()
        ghgrp_facilities = ghgrp_df.drop_duplicates('Site Name').copy()
        
        print(f"    NEI unique facilities: {len(nei_facilities):,}")
        print(f"    GHGRP unique facilities: {len(ghgrp_facilities):,}")
        
        # Clean and standardize data
        print("  Cleaning and standardizing facility data...")
        nei_facilities['Clean Address'] = nei_facilities['Street Address'].apply(self.clean_address)
        ghgrp_facilities['Clean Address'] = ghgrp_facilities['Street Address'].apply(self.clean_address)
        nei_facilities['Clean Name'] = nei_facilities['Site Name'].apply(self.clean_name)
        ghgrp_facilities['Clean Name'] = ghgrp_facilities['Site Name'].apply(self.clean_name)
        nei_facilities['Std State'] = nei_facilities['State'].apply(self.standardize_state)
        ghgrp_facilities['Std State'] = ghgrp_facilities['State'].apply(self.standardize_state)
        nei_facilities['Std FIPS'] = nei_facilities['FIPS'].apply(self.standardize_fips)
        ghgrp_facilities['Std FIPS'] = ghgrp_facilities['FIPS'].apply(self.standardize_fips)
        
        # Initialize results
        high_confidence_matches = []
        matched_nei_ids = set()
        matched_ghgrp_ids = set()
        
        print("  Finding facility matches...")
        
        # Match each NEI facility to best GHGRP facility
        for nei_idx, nei_row in tqdm(nei_facilities.iterrows(), total=len(nei_facilities), desc="Matching facilities"):
            nei_id = nei_row.get('EIS Facility ID', f"NEI_{nei_idx}")
            nei_lat = nei_row['Lat']
            nei_lon = nei_row['Lon']
            nei_state = nei_row['Std State']
            nei_fips = nei_row['Std FIPS']
            
            best_match = None
            best_score = 0
            
            # Check each GHGRP facility
            for ghgrp_idx, ghgrp_row in ghgrp_facilities.iterrows():
                ghgrp_id = ghgrp_row.get('facility_id', f"GHGRP_{ghgrp_idx}")
                
                if ghgrp_id in matched_ghgrp_ids:
                    continue
                
                ghgrp_lat = ghgrp_row['Lat']
                ghgrp_lon = ghgrp_row['Lon']
                ghgrp_state = ghgrp_row['Std State']
                ghgrp_fips = ghgrp_row['Std FIPS']
                
                # Must be same state
                if nei_state != ghgrp_state:
                    continue
                
                # Calculate distance
                try:
                    distance = self.calculate_coordinate_distance(nei_lat, nei_lon, ghgrp_lat, ghgrp_lon)
                except:
                    distance = float('inf')
                
                if distance > self.distance_threshold:
                    continue
                
                # Calculate similarities
                name_similarity = fuzz.token_sort_ratio(nei_row['Clean Name'], ghgrp_row['Clean Name'])
                address_similarity = fuzz.token_sort_ratio(nei_row['Clean Address'], ghgrp_row['Clean Address'])
                same_fips = nei_fips == ghgrp_fips
                
                # Calculate total score
                score = 0
                
                # Distance score (max 40 points)
                if distance < 1:
                    score += 40
                elif distance < 5:
                    score += 30
                elif distance < 10:
                    score += 20
                elif distance < 20:
                    score += 10
                
                # Name similarity (max 30 points)
                score += name_similarity * 0.3
                
                # Address similarity (max 20 points)
                score += address_similarity * 0.2
                
                # FIPS match (10 points)
                if same_fips:
                    score += 10
                
                # Update best match
                if score > best_score:
                    best_score = score
                    best_match = {
                        'nei_id': nei_id,
                        'ghgrp_id': ghgrp_id,
                        'nei_name': nei_row['Site Name'],
                        'ghgrp_name': ghgrp_row['Site Name'],
                        'nei_address': nei_row['Street Address'],
                        'ghgrp_address': ghgrp_row['Street Address'],
                        'nei_state': nei_row['State'],
                        'ghgrp_state': ghgrp_row['State'],
                        'nei_lat': nei_lat,
                        'nei_lon': nei_lon,
                        'ghgrp_lat': ghgrp_lat,
                        'ghgrp_lon': ghgrp_lon,
                        'name_similarity': name_similarity,
                        'address_similarity': address_similarity,
                        'distance_km': distance,
                        'same_state': nei_state == ghgrp_state,
                        'same_fips': same_fips,
                        'match_score': score
                    }
            
            # Accept high confidence matches
            if best_match and best_match['match_score'] >= self.high_confidence_threshold:
                high_confidence_matches.append(best_match)
                matched_nei_ids.add(nei_id)
                matched_ghgrp_ids.add(best_match['ghgrp_id'])
        
        print(f"    Found {len(high_confidence_matches)} high-confidence matches")
        
        # Create merged facility rows
        merged_rows = []
        
        # Process high confidence matches
        for match in tqdm(high_confidence_matches, desc="Creating matched facility records"):
            nei_row = nei_facilities[nei_facilities['EIS Facility ID'] == match['nei_id']].iloc[0] if 'EIS Facility ID' in nei_facilities.columns else nei_facilities.iloc[int(match['nei_id'].split('_')[1])]
            ghgrp_row = ghgrp_facilities[ghgrp_facilities['facility_id'] == match['ghgrp_id']].iloc[0] if 'facility_id' in ghgrp_facilities.columns else ghgrp_facilities.iloc[int(match['ghgrp_id'].split('_')[1])]
            
            merged_row = self.create_merged_facility_row(nei_row, ghgrp_row, match, "high_confidence")
            merged_rows.append(merged_row)
        
        # Add unmatched NEI facilities
        unmatched_nei_count = 0
        for nei_idx, nei_row in tqdm(nei_facilities.iterrows(), total=len(nei_facilities), desc="Adding unmatched NEI facilities"):
            nei_id = nei_row.get('EIS Facility ID', f"NEI_{nei_idx}")
            if nei_id not in matched_nei_ids:
                unmatched_row = self.create_unmatched_nei_row(nei_row)
                merged_rows.append(unmatched_row)
                unmatched_nei_count += 1
        
        # Add unmatched GHGRP facilities
        unmatched_ghgrp_count = 0
        for ghgrp_idx, ghgrp_row in tqdm(ghgrp_facilities.iterrows(), total=len(ghgrp_facilities), desc="Adding unmatched GHGRP facilities"):
            ghgrp_id = ghgrp_row.get('facility_id', f"GHGRP_{ghgrp_idx}")
            if ghgrp_id not in matched_ghgrp_ids:
                unmatched_row = self.create_unmatched_ghgrp_row(ghgrp_row)
                merged_rows.append(unmatched_row)
                unmatched_ghgrp_count += 1
        
        print(f"    Added {unmatched_nei_count} unmatched NEI facilities")
        print(f"    Added {unmatched_ghgrp_count} unmatched GHGRP facilities")
        
        merged_df = pd.DataFrame(merged_rows)
        print(f"    Total merged facilities: {len(merged_df):,}")
        
        return merged_df
    
    def create_merged_facility_row(self, nei_row, ghgrp_row, match, match_type):
        """Create merged facility row with both NEI and GHGRP data."""
        merged_row = {}
        
        # Add NEI fields with prefix
        for col in nei_row.index:
            merged_row[f'nei_{col}'] = nei_row[col]
        
        # Add GHGRP fields with prefix
        for col in ghgrp_row.index:
            merged_row[f'ghgrp_{col}'] = ghgrp_row[col]
        
        # Add match metadata
        merged_row['match_type'] = match_type
        merged_row['match_score'] = match['match_score']
        merged_row['name_similarity'] = match['name_similarity']
        merged_row['address_similarity'] = match['address_similarity']
        merged_row['distance_km'] = match['distance_km']
        
        return merged_row
    
    def create_unmatched_nei_row(self, nei_row):
        """Create row for unmatched NEI facility."""
        unmatched_row = {}
        
        # Add NEI fields with prefix
        for col in nei_row.index:
            unmatched_row[f'nei_{col}'] = nei_row[col]
        
        # Add empty GHGRP fields
        ghgrp_cols = ['facility_id', 'year', 'State', 'Lat', 'Lon', 'Street Address', 
                      'FIPS', 'Site Name', 'Pollutant', 'Emission Tons', 'subpart_category']
        
        for col in ghgrp_cols:
            unmatched_row[f'ghgrp_{col}'] = None
        
        # Add match metadata
        unmatched_row['match_type'] = "nei_only"
        unmatched_row['match_score'] = None
        unmatched_row['name_similarity'] = None
        unmatched_row['address_similarity'] = None
        unmatched_row['distance_km'] = None
        
        return unmatched_row
    
    def create_unmatched_ghgrp_row(self, ghgrp_row):
        """Create row for unmatched GHGRP facility."""
        unmatched_row = {}
        
        # Add empty NEI fields
        nei_cols = ['State', 'Pollutant', 'Emission Tons', 'Site Name', 'Street Address',
                    'Lat', 'Lon', 'FIPS', 'EIS Facility ID', 'Facility Type', 'NAICS']
        
        for col in nei_cols:
            unmatched_row[f'nei_{col}'] = None
        
        # Add GHGRP fields with prefix
        for col in ghgrp_row.index:
            unmatched_row[f'ghgrp_{col}'] = ghgrp_row[col]
        
        # Add match metadata
        unmatched_row['match_type'] = "ghgrp_only"
        unmatched_row['match_score'] = None
        unmatched_row['name_similarity'] = None
        unmatched_row['address_similarity'] = None
        unmatched_row['distance_km'] = None
        
        return unmatched_row
    
    def create_pollutant_dataset(self, merged_facilities, nei_df, ghgrp_df):
        """Create final pollutant dataset by merging facility matches with pollutant data."""
        print("  Creating comprehensive pollutant dataset...")
        
        final_rows = []
        
        # Process each merged facility
        for idx, facility_row in tqdm(merged_facilities.iterrows(), total=len(merged_facilities), desc="Processing facilities"):
            
            # Handle matched facilities (both NEI and GHGRP data)
            if facility_row['match_type'] == 'high_confidence':
                nei_facility_id = facility_row['nei_EIS Facility ID']
                ghgrp_facility_id = facility_row['ghgrp_facility_id']
                
                # Get NEI pollutant records for this facility
                nei_pollutants = nei_df[nei_df['EIS Facility ID'] == nei_facility_id]
                
                # Get GHGRP pollutant records for this facility
                ghgrp_pollutants = ghgrp_df[ghgrp_df['facility_id'] == ghgrp_facility_id]
                
                # Create records for NEI pollutants
                for _, nei_poll in nei_pollutants.iterrows():
                    row = self.create_final_pollutant_row(facility_row, nei_poll, None, 'NEI')
                    final_rows.append(row)
                
                # Create records for GHGRP pollutants
                for _, ghgrp_poll in ghgrp_pollutants.iterrows():
                    row = self.create_final_pollutant_row(facility_row, None, ghgrp_poll, 'GHGRP')
                    final_rows.append(row)
            
            # Handle NEI-only facilities
            elif facility_row['match_type'] == 'nei_only':
                nei_facility_id = facility_row['nei_EIS Facility ID']
                nei_pollutants = nei_df[nei_df['EIS Facility ID'] == nei_facility_id]
                
                for _, nei_poll in nei_pollutants.iterrows():
                    row = self.create_final_pollutant_row(facility_row, nei_poll, None, 'NEI')
                    final_rows.append(row)
            
            # Handle GHGRP-only facilities
            elif facility_row['match_type'] == 'ghgrp_only':
                ghgrp_facility_id = facility_row['ghgrp_facility_id']
                ghgrp_pollutants = ghgrp_df[ghgrp_df['facility_id'] == ghgrp_facility_id]
                
                for _, ghgrp_poll in ghgrp_pollutants.iterrows():
                    row = self.create_final_pollutant_row(facility_row, None, ghgrp_poll, 'GHGRP')
                    final_rows.append(row)
        
        final_df = pd.DataFrame(final_rows)
        print(f"    Created {len(final_df):,} facility-pollutant records")
        
        return final_df
    
    def create_final_pollutant_row(self, facility_row, nei_pollutant, ghgrp_pollutant, data_source):
        """Create final pollutant row combining facility and pollutant data."""
        row = {}
        
        # Basic facility information (prefer GHGRP, fallback to NEI)
        row['facility_name'] = facility_row.get('ghgrp_Site Name') or facility_row.get('nei_Site Name')
        row['street_address'] = facility_row.get('ghgrp_Street Address') or facility_row.get('nei_Street Address')
        row['state'] = facility_row.get('ghgrp_State') or facility_row.get('nei_State')
        row['latitude'] = facility_row.get('ghgrp_Lat') or facility_row.get('nei_Lat')
        row['longitude'] = facility_row.get('ghgrp_Lon') or facility_row.get('nei_Lon')
        row['fips'] = facility_row.get('ghgrp_FIPS') or facility_row.get('nei_FIPS')
        
        # Facility IDs
        row['nei_facility_id'] = facility_row.get('nei_EIS Facility ID')
        row['ghgrp_facility_id'] = facility_row.get('ghgrp_facility_id')
        
        # Industry classification
        row['facility_type'] = facility_row.get('nei_Facility Type')
        row['naics_code'] = facility_row.get('nei_NAICS')
        row['subpart_category'] = facility_row.get('ghgrp_subpart_category')
        
        # Pollutant data - use the standardized 'Emission Tons' column
        if nei_pollutant is not None:
            row['pollutant'] = nei_pollutant['Pollutant']
            row['emission_metric_tons'] = nei_pollutant['Emission Tons']  # NEI data in metric tons
            row['pollutant_type'] = 'Criteria Pollutant'
        elif ghgrp_pollutant is not None:
            row['pollutant'] = ghgrp_pollutant['Pollutant']
            row['emission_metric_tons'] = ghgrp_pollutant['Emission Tons']  # GHGRP data in metric tons
            row['pollutant_type'] = 'Greenhouse Gas'
        
        # Metadata
        row['data_source'] = data_source
        row['year'] = 2020
        row['match_type'] = facility_row.get('match_type')
        row['match_score'] = facility_row.get('match_score')
        
        return row


def main():
    """Main function to run the complete pipeline."""
    
    # Check required files
    required_files = [
        "2020Facility-LevelDataforPointEmissions.xlsx",
        "2020_GHGRP_subpart_facilities.xlsx"
    ]
    
    missing_files = [f for f in required_files if not os.path.exists(f)]
    if missing_files:
        print("Error: Missing required files:")
        for f in missing_files:
            print(f"  - {f}")
        print("\nPlease ensure both files are in the current directory.")
        return False
    
    # Run the complete pipeline
    merger = NEIGHGRPMerger()
    success = merger.run_complete_pipeline()
    
    if success:
        print("\nPipeline completed successfully!")
        print("Check 'final_pollutant_data.xlsx' for results.")
    else:
        print("\nPipeline failed. Check error messages above.")
    
    return success


if __name__ == "__main__":
    main()

