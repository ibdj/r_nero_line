library(tidyverse)
library(janitor)
library(lubridate)

#### Importing and merging all files ####

# Define the path to the folder containing your CSV files
folder_path <- "/Users/ibdj/Library/CloudStorage/OneDrive-GrønlandsNaturinstitut/General - BioBasis/BioBasis_Nuuk_2022/NEROline_2022/r_nero_line/r_nero_line"

# List all CSV files in the directory that match the pattern "*data.csv"
file_list <- list.files(path = folder_path, pattern = "*data.csv", full.names = TRUE)

# The colomns to beincluded
selected_columns <- c("year", "date", "vt_section","plot","plot_id", "taxon_code","raunkiaer_value","fertile","cfr","remarks") 

# Function to read and select specific columns
read_and_select <- function(file) {
  data <- read_csv(file, col_types = cols_only(!!!setNames(rep("?", length(selected_columns)), selected_columns)))
  data$raunkiaer_value <- as.numeric(data$raunkiaer_value)
  data$fertile <- as.numeric(data$fertile)
  data$date <- as.Date(data$date)
  #data$veg_type <- as.factor(data$veg_type)
  data$taxon_code <- as.factor(data$taxon_code)
  data$cfr <- as.factor(data$cfr)
  return(data)
}

# Read and merge all CSV files into a single data frame
merged_data <- file_list |> 
  lapply(read_and_select) |> 
  bind_rows()

# View the merged data frame
print(merged_data)


#### filter to see or remove nas #####
nas <- merged_data |> 
  filter(taxon_code == "na" | raunkiaer_value == -9999)

#### Prepping to be GEM ready #####

# Index of vegetation types to join them by vt_section
index_veg_type <- read_csv("index_veg_type.csv") |> 
  mutate(veg_type = as.factor(veg_type))

# Index of coordinates to join them by plot_id
index_coordinates <- read_csv("index_coordinates.csv") |> 
  mutate(certainty = as.factor(certainty))

# Index of species to join tjem by taxon_code
index_species <- read_csv("index_species.csv")

# final step of removing nas, -9999 and 0s (NAs and -9999 will indicate missing plots and 0s will be species no longer present in plot)
wo_nas <- merged_data |> 
  filter(taxon_code != "na",
         raunkiaer_value != -9999,
         raunkiaer_value != 0)

data_analysis <- wo_nas |> 
  left_join(index_veg_type, by = "vt_section") |> 
  left_join(index_coordinates, by = "plot_id") |> 
  filter(!(taxon_code %in% c("stecan", "camdis"))) |> 
  left_join(index_species, by = "taxon_code")

write_rds(data_analysis, file = "nero_line_organised_raw_data.rds")

#### Making the final dataset (With correct names, removing the unknown species)                    ####
BioBasis_Nuuk_Raunkiaer_NERO_line <- wo_nas |> 
  left_join(index_veg_type, by = "vt_section") |> 
  left_join(index_coordinates, by = "plot_id") |> 
  filter(!(taxon_code %in% c("stecan", "camdis"))) |> 
  left_join(index_species, by = "taxon_code") |> 
  select(date,year,vt_section,plot,plot_id,veg_type,species,taxon_code,cfr,raunkiaer_value,fertile,remarks,lon,lat,certainty) |> 
  rename_with(~ paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.))), everything()) |> 
  mutate("Veg_type" = str_to_sentence(Veg_type))

# Final data checking
summary(BioBasis_Nuuk_Raunkiaer_NERO_line) # General summary, check min and max values for all variables
str(BioBasis_Nuuk_Raunkiaer_NERO_line) # Check correct formats
veg_type <- unique(BioBasis_Nuuk_Raunkiaer_NERO_line$Veg_type) # Check the vegetation types
georef_error <- BioBasis_Nuuk_Raunkiaer_NERO_line |> # Check that all observations have coordinates
  filter(is.na(Certainty))

# Write the final dataset and save in the GEM data base folder in OneDrive
# write.csv(BioBasis_Nuuk_Raunkiaer_NERO_line,"/Users/ibdj/Library/CloudStorage/OneDrive-GrønlandsNaturinstitut/General - BioBasis/03_GEM_Database/NERO line/BioBasis_Nuuk_Raunkiaer_NERO_line.csv")


#### Making a distinct taxon_code list for name matching ####

veg_type <- BioBasis_Nuuk_Raunkiaer_NERO_line |> 
  distinct(Veg_type)

index_taxon_code <- merged_data |> 
  distinct(taxon_code)

#write_csv(index_taxon_code,"index_taxon_code")

##### Making a date entry sheet (with some ekstra lines for new observations) ####

additional_lines_for_data_sheet <- read_csv("additional_lines_for_data_sheet.csv", col_types = cols(taxon_code = col_character(), raunkiaer_value = col_double())) |>  
  mutate(date =as.Date(""),
         fertile = as.integer(""),
         year = as.numeric(""),
         veg_type = "",
         remarks = "")

data_gem_field <- merged_data |> 
  distinct(plot_id,taxon_code,vt_section,plot) |> 
  mutate(date = as.Date(""),
         fertile = as.integer(""),
         year = as.numeric(""),
         veg_type = "",
         remarks = "",
         vt_section = as.numeric(""),
         raunkiaer_value = as.numeric(""),
         plot = as.numeric(""))

compare_df_cols(additional_lines_for_data_sheet, data_gem_field)

field_data_format <- data_gem_field  |>  
  rbind(additional_lines_for_data_sheet)  |>  
  arrange(plot_id, taxon_code) 

 write_excel_csv(field_data_format, "nero_line_field_datasheet")

             