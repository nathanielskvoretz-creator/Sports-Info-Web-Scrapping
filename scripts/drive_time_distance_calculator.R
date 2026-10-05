# 1. Install and load free packages
library(tidygeocoder)
library(osrm)
library(tidyverse)

Freelance_Photos_Info_NCAA_Members <- read_csv("data/input/Freelance Photos Info - NCAA Members.csv")

# 2. Input your current address 
my_current_address <- "7932 Wilson Ridge Ln, Mint Hill, NC 28227" 

# 3. Load your dataset
ncaa_members <- Freelance_Photos_Info_NCAA_Members |> 
  separate_wider_delim(cols = Website, delim = "\n", names = c("uni_url", "ath_url"), too_few = "align_start") |> 
  filter(!is.na(ath_url)) |> 
  select(Name:State)

#filtered df to feed into add_travel_metrics functions
search_ncaa_members <- ncaa_members |> 
  filter(State %in% c("NC", "SC")) 
  
# 4. Step 1: Geocode your home address to coordinates (Long/Lat)
home_coords <- geo(address = my_current_address, method = 'osm', lat = latitude, long = longitude)


# Define a clean, robust function that accepts home coordinates and updates your dataframe
add_travel_metrics <- function(data_frame, home_lon, home_lat) {
    
  # 1. Clean the destination dataframe to remove any NA coordinates
  #Note: we create the geocoded df inside the function to be able to re-run
  ncaa_members_geocoded <- {{ data_frame }} |> 
    mutate(search_address = paste(Name, State, sep = ", ")) |> 
    geocode(address = search_address, method = 'arcgis', lat = latitude, long = longitude) |> 
    filter(!is.na(longitude) & !is.na(latitude)) # Drop any schools that couldn't be found to prevent errors
  
  # 2. Convert destinations to a raw numeric matrix
  dst_matrix <- ncaa_members_geocoded |> 
    select(longitude, latitude) |> 
    as.matrix()
  
  # 3. Query the OSRM API explicitly requesting BOTH duration and distance metrics
  matrix_list <- osrmTable(
    src     = data.frame(lon = home_lon, lat = home_lat),
    dst     = dst_matrix,
    measure = c("duration", "distance")  # CRITICAL FIX: Forces server to return distance data
  )
  
  # 4. Extract data arrays and bind them directly into the valid schools rows
  ncaa_members_geocoded <- ncaa_members_geocoded |> 
    mutate(
      drive_time_minutes = round(as.vector(matrix_list$durations), 1),
      distance_miles     = round(as.vector(matrix_list$distances) / 1609.34, 1) # Converts meters to miles
    ) |> 
    arrange(drive_time_minutes)
  
  # 5. Return the populated dataframe sorted from closest to farthest
  return(ncaa_members_geocoded)
}

ncaa_members_updated <- add_travel_metrics(
  data_frame = search_ncaa_members, 
  home_lon   = home_coords$longitude, 
  home_lat   = home_coords$latitude
)

local_ncaa_members<- ncaa_members_updated |> 
  filter(drive_time_minutes <= 65)

search_filter <- local_ncaa_members$Name

find_missing(ncaa_members_updated, ncaa_members, by = Name)

