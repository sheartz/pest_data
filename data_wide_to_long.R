# Load required libraries
library(sf)      # For reading shapefiles
library(tidyr)   # For reshaping the data
library(dplyr)   # For data manipulation
library(stringr) # For padding sequences
library(purrr)   # For mapping or tracking functions
library(mapview) # In case we need map views

# Set the working directory
wd <- "\\\\s-fre-nas1\\Data$\\cedge\\Historical_Pesticide_Project\\NS_NL_historic_pesticides\\RawData\\"
setwd(wd)  # Set the working directory

# Define the shapefile path
shapefile_path <- paste0(wd, "NL_NS_historic_pesticid_Project.shp")

# Read the shapefile and assign 'wide_data' name
wide_data <- st_read(shapefile_path)

# Display column names to confirm exact names and structure of header
print(colnames(wide_data))

# Create `app_area` as a sequential identifier for each `year`, add to shapefile
shapefile_data_unique <- wide_data %>% # Assign wide_data to shapefile_data_unique
  arrange(year) %>% # organize
  group_by(year) %>% # group by year
  # Finally use group sequence number to create app_area
  mutate(app_area_seq = row_number(),
         app_area = paste0(year, "_", str_pad(app_area_seq, 3, pad = "0"))) %>%
  ungroup()

# Expand rows based on `num_apps` and create `app_id`
shapefile_data_expanded <- shapefile_data_unique %>%
  rowwise() %>%
  mutate(
    app_id = list(paste0(app_area, "_", seq_len(num_apps)))  # List of IDs for each app
  ) %>%
  unnest(app_id) %>%  # Expands each `app_area` to multiple rows based on `num_apps`
  ungroup()



# Isolate only the columns we need for pivoting (to prevent unintended duplication)
# Include only the "app_id", "app_area" and fields for pivoting
shapefile_data_expanded <- shapefile_data_expanded %>%
  select(app_id, app_area, year, starts_with("insect"), starts_with("act_ing"), 
         starts_with("rate"), starts_with("vol_Lha"), starts_with("form_type"),
         starts_with("tank_mix"), starts_with("brand"))

# Error generated in the csv development in pivot_longer, can't find specific fields, "vol_Lha" and "form_type"
#remove those columns from the operation and the file works.

# Use `pivot_longer()` to move specified columns to long format
csv_data <- shapefile_data_expanded %>%
  pivot_longer(
    cols = starts_with(c("insect", "act_ing", "rate", "vol_Lha", "form_type", "tank_mix", "brand")),
    names_to = c(".value", "group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  # Select the columns for the data write
  select(app_id, app_area, year, insect, act_ing, rate, vol_Lha, form_type, tank_mix, brand)



# Write the transformed data to a CSV file
write.csv(csv_data, "attribute_data_long.csv", row.names = FALSE)

# View the final long-format csv
#print(csv_data)





