# Load required libraries
library(sf)      # For reading shapefiles
library(tidyr)   # For reshaping the data
library(dplyr)   # For data manipulation
library(stringr) # For padding sequences

# Set the working directory holding the wide version shapefile in the correct spatial reference system
wd <- "\\\\s-fre-nas1\\Data$\\cedge\\Historical Pesticide Project\\NS_NL_historic_pesticides\\RawData\\"
setwd(wd)  # Set the working directory

# Define the shapefile path
shapefile_path <- paste0(wd, "NL_NS_historic_pesticid_Project.shp")

# Read the shapefile
wide_data <- st_read(shapefile_path)

# View the structure of the data
str(wide_data)

# Create a sequential identifier based on the 'year' field and place into shapefile structure
shapefile_data_unique <- wide_data %>%
  arrange(year) %>%                        # Sort data by year and link to next
  group_by(year) %>%                       # Group by 'year' and link to next
  mutate(seq_number = row_number(),        # Create sequential number within each year
         app_id = paste0(year, "_", str_pad(seq_number, 3, pad = "0"))) %>%  # Create 'year_001' format and link forward
  ungroup() %>%
  
  # Select the fields in the shapefile along with the new unique ID
  select(app_id, num_apps, block_1, block_2, block_3, block_4, year, province, map_nam, 
         insect_1, insect_2,insect_3,insect_4, act_ing_1, act_ing_2, act_ing_3, act_ing_4, 
         app_type, rate_1, rate_2, rate_3, rate_4, vol_Lha1, vol_Lha2, vol_Lha3, vol_Lha4, 
         tank_mix_1, tank_mix_2, tank_mix_3, tank_mix_4, form_type1, form_type2, form_type3, 
         form_type4, brand_1, brand_2, brand_3, brand_4, aircraft, guidance, app_sys, 
         reference, Shape_Leng, Shape_Area)

# Create a long format csv using pivot
csv_data <- shapefile_data_unique %>%
  pivot_longer(cols = starts_with(c("insect", "act_ing", "rate", "vol_Lha", "form_type", "tank_mix", "brand")),
    names_to = c(".value", "group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  select(app_id, year, insect, act_ing, rate, vol_Lha, form_type, tank_mix, brand)

# Write out a CSV file with selected columns (problem here, columns removed so not available to write to csv?)
csv_data <- shapefile_data_unique %>%
  select(app_id, year, insect, act_ing, rate, vol_Lha, form_type, tank_mix, brand)  # Replace with the actual columns to export

# Write the selected data to a CSV file, change the name!
write.csv(csv_data, "attribute_data.csv", row.names = FALSE)

# View the resulting long data from the shapefile
print(shapefile_data_unique)

# View the written CSV data
print(csv_data)
