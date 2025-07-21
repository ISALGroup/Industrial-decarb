# Function to export facility level data

library(here)


export_facility_unit_data = function(industry, version, datasets, file_type = "unit") {
  # Create a workbook
  workbook <- createWorkbook()
  
  # Add worksheets and write data
  for (dataset_name in names(datasets)) {
    addWorksheet(workbook, dataset_name)
    writeData(workbook, dataset_name, datasets[[dataset_name]])
  }
  
  # Determine file type suffix
  file_suffix <- ifelse(file_type == "unit", "emissions_by_unit_", "emissions_by_facility_")
  
  # Construct the file name and save the workbook
  filename <- paste0(industry, "_", file_suffix, version, ".xlsx")
  saveWorkbook(workbook, here("Output", filename), overwrite = TRUE)
}