library(dplyr)
library(tidygeocoder)
library(osrm)
library(tidyverse)
library(arcgeocoder)
library(geosphere)

httr::set_config(httr::config(http_version = 0)) 

FreeFreelance_Photos_Info_NCAA_Members <- read_csv("data/input/Freelance Photos Info - NCAA Members.csv")
#location name should be renamed search_name
cities <- read_csv("data/input/SUB-IP-EST2025-POP - SUB-IP-EST2025-POP.csv", 
                   skip = 3, 
                   col_names = c("search_name", "2020_q2", "2020_q3", "2021", "2022", "2023", "2024", "2025")
                   )

ncaa_members <- Freelance_Photos_Info_NCAA_Members |> 
  separate_wider_delim(cols = Website, delim = "\n", names = c("uni_url", "ath_url"), too_few = "align_start") |> 
  filter(!is.na(ath_url)) |> 
  select(Name:State)

ncaa_members_filter <- ncaa_members |> 
  filter(State %in% c("NC")) 

cities <- cities |> 
  filter(`2025` > 5000) |> 
  select(search_name)

# 1. Create a list of candidate "home bases" to test
candidate_locations <- data.frame(
  search_name = c(
    "Charlotte, NC", "Raleigh, NC", "Greensboro, NC", 
    "Durham, NC", "Winston-Salem, NC", "Fayetteville, NC", 
    "Cary, NC", "Wilmington, NC", "High Point, NC", "Concord, NC", 
    "Chapel Hill, NC", "Troy, NC"
  )
)

ncaa_members_geocoded <- ncaa_members %>%
  mutate(search_address = paste(Name, State, sep = ", ")) |> 
  geocode(address = search_address, method = 'arcgis', lat = latitude, long = longitude) |> 
  filter(!is.na(latitude)) |>   # Drop any schools that couldn't be found to prevent errors
  select(Name:search_address, longitude, latitude)

  
#Create Weights
ncaa_members_geocoded <- ncaa_members_geocoded |>
  mutate(weight = case_when(
    Division == "I-FBS"   ~ 3,  # Highest weight for D1
    Division == "I-FCS"   ~ 2.8, 
    Division == "\\bI\\b" ~ 2.6,
    Division == "II\\b"   ~ 2,  # Medium weight for D2
    Division == "III"   ~ 1,  # Standard weight for D3
    TRUE ~ 1               # Fallback just in case
  ))


# 2. Geocode your candidates
  candidates_geocoded <- cities |>
    geocode(address = search_name, method = 'arcgis', lat = lat, long = lon) |>
    filter(!is.na(lat))
  
  calculate_weighted_location <- function(drive_time, src_batch = 5, dst_batch = 40) {
    
    # 1. Isolate the target coordinates
    src_df <- candidates_geocoded |> select(lon, lat)
    dst_df <- ncaa_members_geocoded |> select(longitude, latitude)
    
    num_src <- nrow(src_df)
    num_dst <- nrow(dst_df)
    
    # Initialize an empty matrix matching the total final dimensions
    final_durations <- matrix(NA, nrow = num_src, ncol = num_dst)
    
    message(paste("Starting grid processing:", num_src, "candidates x", num_dst, "NCAA members."))
    
    # 2. Outer Loop: Chunk through candidate sources
    for (i in seq(1, num_src, by = src_batch)) {
      i_end <- min(i + src_batch - 1, num_src)
      src_chunk <- src_df[i:i_end, , drop = FALSE]
      
      # 3. Inner Loop: Chunk through NCAA destination schools
      for (j in seq(1, num_dst, by = dst_batch)) {
        j_end <- min(j + dst_batch - 1, num_dst)
        dst_chunk <- dst_df[j:j_end, , drop = FALSE]
        
        success <- FALSE
        attempts <- 1
        max_attempts <- 3
        
        # Auto-retry loop for safety
        while (!success && attempts <= max_attempts) {
          batch_matrix <- tryCatch({
            osrmTable(
              src = src_chunk,
              dst = dst_chunk,
              measure = "duration"
            )
          }, error = function(e) {
            Sys.sleep(4)
            return(NULL)
          })
          
          if (!is.null(batch_matrix)) {
            success <- TRUE
          } else {
            attempts <- attempts + 1
          }
        }
        
        # Halt gracefully if a complete grid lock happens
        if (!success) {
          stop(paste("Server repeatedly rejected grid block: Sources", i, "-", i_end, "| Destinations", j, "-", j_end))
        }
        
        # Map the sub-matrix values directly into their correct global indices
        final_durations[i:i_end, j:j_end] <- batch_matrix$durations
        
        # Tiny pause to protect your IP from rate limits
        Sys.sleep(0.5)
      }
      message(paste("Successfully completed candidate row batch up to index:", i_end))
    }
    
    # 4. Filter and process using your original budget parameters
    time_budget_mins <- drive_time
    in_range_matrix <- final_durations <= time_budget_mins
    in_range_matrix[is.na(in_range_matrix)] <- FALSE
    
    # Execute matrix multiplication score calculation
    candidates_geocoded$weighted_score <- as.vector(in_range_matrix %*% ncaa_members_geocoded$weight)
    candidates_geocoded$schools_in_range <- rowSums(in_range_matrix)
    
    # Rank the best locations
    best_locations <- candidates_geocoded |> 
      arrange(desc(weighted_score), desc(schools_in_range))
    
    return(best_locations)
  }
  

# Call your function using a gentle batch constraint
  final_results <- calculate_weighted_location(
    drive_time = 60, 
    src_batch = 5, 
    dst_batch = 40
  )

  library(geosphere)
  library(dplyr)
  
  calculate_fast_weighted_location <- function(max_distance_miles = 40) {
    
    # 1. Extract coordinate matrices
    src_coords <- candidates_geocoded |> select(lon, lat) |> as.matrix()
    dst_coords <- ncaa_members_geocoded |> select(longitude, latitude) |> as.matrix()
    
    # 2. Instantly calculate a 4901 x 1082 matrix of distances in meters
    message("Computing 5.3 million distances locally...")
    dist_matrix_meters <- distm(src_coords, dst_coords, fun = distHaversine)
    
    # 3. Convert meters to miles (1 meter = 0.000621371 miles)
    dist_matrix_miles <- dist_matrix_meters * 0.000621371
    
    # 4. Check budget constraint
    in_range_matrix <- dist_matrix_miles <= max_distance_miles
    in_range_matrix[is.na(in_range_matrix)] <- FALSE
    
    # 5. Matrix multiplication scoring
    candidates_geocoded$weighted_score <- as.vector(in_range_matrix %*% ncaa_members_geocoded$weight)
    candidates_geocoded$schools_in_range <- rowSums(in_range_matrix)
    
    # Sort final locations
    best_locations <- candidates_geocoded |> 
      arrange(desc(weighted_score), desc(schools_in_range))
    
    return(best_locations)
  }
  
  # Run the lightning-fast math-based approach
  final_results <- calculate_fast_weighted_location(max_distance_miles = 45)
  

# Attempt a single, tiny 1-to-1 route request
osrmRoute(src = c(-73.98, 40.75), dst = c(-74.00, 40.73))
