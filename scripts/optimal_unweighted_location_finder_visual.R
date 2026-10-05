# Install if you don't have them: install.packages(c("sf", "leaflet", "osrm"))
library(dplyr)
library(sf)
library(leaflet)
library(osrm)
library(progress)
library(tidyverse)
library(tidygeocoder)

Freelance_Photos_Info_NCAA_Members <- read_csv("data/input/Freelance Photos Info - NCAA Members.csv")

#Import List and Member Details
ncaa_members <- Freelance_Photos_Info_NCAA_Members |> 
  separate_wider_delim(cols = Website, delim = "\n", names = c("uni_url", "ath_url"), too_few = "align_start") |> 
  filter(!is.na(ath_url)) |> 
  select(Name:State)

#Filter
ncaa_members_search <- ncaa_members |> 
  filter(Conference %in% c("Atlantic Coast Conference", "Southeastern Conference", "Big 12 Conference", "Big Ten Conference")) 

ncaa_members_geocoded <- ncaa_members_search |> 
  mutate(search_address = paste(Name, State, sep = ", ")) |> 
  geocode(address = search_address, method = 'arcgis', lat = latitude, long = longitude) |> 
  filter(!is.na(latitude)) # Drop any schools that couldn't be found to prevent errors

pull_map_data <- function(df, drive_time){
  
  isochrone_list <- list()
  total_schools <- nrow({{ df }})
  
  # Clear console to start fresh
  cat("\014") 
  
  for (i in 1:total_schools) {
    
    # 1. Print the waiting message ONLY for the first school
    if (i == 1) {
      message("Loop started. Negotiating the initial OSRM connection... please wait.")
      # Force R to print this message immediately
      flush.console() 
    }
    
    school_name <- {{ df }}$Name[i]
    lon <- {{ df }}$longitude[i]
    lat <- {{ df }}$latitude[i]
    
    suppressWarnings(suppressMessages({
      iso <- tryCatch({
        osrmIsochrone(
          loc = c(lon, lat),
          breaks = drive_time, 
          n = 200 
        )
      }, error = function(e) NULL)
    }))
    
    if (!is.null(iso)) {
      iso$School <- school_name
      isochrone_list[[i]] <- iso
    }
    
    Sys.sleep(1)
    
    # 2. Set up the progress bar AFTER the slow first loop finishes
    if (i == 1) {
      # Clear the "waiting" message from the screen
      cat("\014") 
      
      # Initialize the bar
      pb <- progress_bar$new(
        format = "  Downloading [:bar] :current/:total schools | Elapsed: :elapsed | ETA: :eta",
        total = total_schools,
        clear = FALSE,   
        width = 85
      )
      
      # Tick it to 1/45 instantly
      pb$tick(1) 
    } else {
      # 3. For all other schools, just advance the bar normally
      pb$tick()
    }
  }
  
  
  
  # 3. Let yourself know when it's safe to map
  message("\nAll downloads complete! Binding map data...")
  all_isochrones <- do.call(rbind, isochrone_list)
  
  return(all_isochrones)
}

distance_time_map_data <- pull_map_data(ncaa_members_geocoded, drive_time = 75)

# 4. Build the interactive map!
leaflet() |>
  # Add a clean, light base map
  addProviderTiles(providers$CartoDB.Positron) |>
  
  # Add the isochrones
  addPolygons(
    data = distance_time_map_data,
    color = "#2c7fb8",      # The outline color
    weight = 0,             # Thin borders so it doesn't get cluttered
    fillOpacity = 0.1,     # The magic number! Low opacity makes overlaps darker
    popup = ~School         # Clicking a bubble tells you which school it belongs to
  ) |>
  
  # Add tiny red dots for the exact locations of the schools
  addCircleMarkers(
    data = ncaa_members_geocoded,
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 1,
    popup = ~Name
  )

