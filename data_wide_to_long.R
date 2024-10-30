# Load required libraries
library(sf)      # For reading shapefiles
library(tidyr)   # For reshaping the data
library(dplyr)   # For data manipulation
library(stringr) # For padding sequences
library(purrr)   # For mapping or tracking functions

# Set the working directory
wd <- "\\\\s-fre-nas1\\Data$\\cedge\\Historical_Pesticide_Project\\NS_NL_historic_pesticides\\RawData\\"
setwd(wd)  # Set the working directory

# Define the shapefile path
shapefile_path <- paste0(wd, "NL_NS_historic_pesticid_Project.shp")

# Read the shapefile
wide_data <- st_read(shapefile_path)

# Display column names to confirm exact names
print(colnames(wide_data))

# Create `app_area` as a sequential identifier for each `year`
shapefile_data_unique <- wide_data %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(app_area_seq = row_number(),
         app_area = paste0(year, "_", str_pad(app_area_seq, 3, pad = "0"))) %>%
  ungroup()

# Need to expand rows based on `num_apps` and create `app_id` to avoid errors use "mutate", then "unnest" to expand to separate rows
shapefile_data_expanded <- shapefile_data_unique %>%
  rowwise() %>%
  mutate(
    app_id = list(paste0(app_area, "_", seq_len(num_apps)))  # List of IDs for each app
  ) %>%
  unnest(app_id) %>%  # Expands each `app_area` to multiple rows based on `num_apps`
  ungroup()

# Select fields to keep in the shapefile and CSV
shapefile_data_expanded <- shapefile_data_expanded %>%
  select(app_id, app_area, num_apps, block_1, block_2, block_3, block_4, year, province, map_nam, 
         insect_1, insect_2, insect_3, insect_4, act_ing_1, act_ing_2, act_ing_3, act_ing_4, 
         app_type, rate_1, rate_2, rate_3, rate_4, vol_Lha1, vol_Lha2, vol_Lha3, vol_Lha4, 
         tank_mix_1, tank_mix_2, tank_mix_3, tank_mix_4, form_type1, form_type2, form_type3, 
         form_type4, brand_1, brand_2, brand_3, brand_4, aircraft, guidance, app_sys, 
         reference, Shape_Leng, Shape_Area)

# Pivot data to long format for specific attributes for the csv write
csv_data <- shapefile_data_expanded %>%
  pivot_longer(
    cols = starts_with(c("insect_", "act_ing_", "rate_", "vol_Lha", "form_type", "tank_mix", "brand_")),
    names_to = c(".value", "group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  select(app_id, app_area, year, insect, act_ing, rate, vol_Lha, form_type, tank_mix, brand)

# Write the transformed data to a CSV file
write.csv(csv_data, "attribute_data_long.csv", row.names = FALSE)

# View the final long-format data
print(csv_data)
