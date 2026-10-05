library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(httr)
library(writexl)
library(tidyverse)

Freelance_Photos_Info_NCAA_Members <- read_csv("data/input/Freelance Photos Info - NCAA Members.csv")

ncaa_members_calender_parsed <- Freelance_Photos_Info_NCAA_Members |> 
  separate_wider_delim(cols = Website, delim = "\n", names = c("uni_url", "ath_url"), too_few = "align_start") |> 
  filter(!is.na(ath_url)) |> 
  mutate(
    short_name = str_remove_all(uni_url, "^www[.]|[.]edu|https://www[.]|[/]$"),
    base_url = str_remove_all(ath_url, "^\\W+|https://www[.]|^https://|^www.|(?<=[.]com|[.]org)\\S+|[/]landing\\S+|index\\S+"),
    base_url = str_remove(base_url, "[/]$"),
    url = str_replace_all(base_url, "^", "https://www."),
    parsed_url = map(url, parse_url),
    hostname = map_chr(parsed_url, "hostname"),
    hostname_url = str_replace_all(hostname, "^", "https://"),
    
    # Check for the exact URL and replace it; otherwise, keep the existing hostname_url
    hostname_url = case_when(
      hostname_url == "https://www.central.edu"    ~ "https://athletics.central.edu",
      hostname_url == "https://www.greenville.edu" ~ "https://greenvillepanthers.com",
      hostname_url == "https://www.rose-hulman.edu" ~ "https://athletics.rose-hulman.edu",
      hostname_url == "https://www.westminster.edu" ~ "https://athletics.westminster.edu",
      hostname_url == "https://www.whitman.edu" ~ "https://whitmanblues.com",
      hostname_url == "https://www.wofford.edu" ~ "https://woffordterriers.com",
      .default = hostname_url
    ),
    
    calender = str_replace_all(hostname_url, "$", "/calendar.ashx/calendar.rss?start_date=")
  )

filtered_ncaa_members <- ncaa_members_calender_parsed |> 
  filter(Name %in% search_filter)

get_sidearm_calendar <- function(base_url, host_name = NA_character_, start_date = "2026-07-26", end_date = "2027-12-31") {
  
  # 1. DYNAMIC YEAR SETUP: Extract the years and start month from the arguments
  start_year <- as.integer(substr(start_date, 1, 4))
  end_year <- as.integer(substr(end_date, 1, 4))
  start_month <- as.integer(substr(start_date, 6, 7))
  
  rss_url <- paste0(base_url, "/calendar.ashx/calendar.rss?start_date=", start_date, "&end_date=", end_date)
  
  empty_tbl <- tibble(
    host = character(), date = character(), time = character(), 
    game = character(), location = character(), raw_text = character(), 
    sport = character(), team = character()
  )
  
  xml_text <- tryCatch({
    res <- httr::GET(rss_url, httr::add_headers(`Connection` = "close"), httr::timeout(10))
    if (httr::status_code(res) == 200) httr::content(res, as = "text", encoding = "UTF-8") else NULL
  }, error = function(e) NULL)
  
  if (is.null(xml_text) || nchar(xml_text) == 0) return(empty_tbl)
  
  feed <- tryCatch(read_xml(xml_text), error = function(e) NULL)
  if (is.null(feed)) return(empty_tbl)
  
  xml_ns_strip(feed) 
  events <- xml_find_all(feed, "//item")
  
  if (length(events) == 0) return(empty_tbl)
  
  calendar_data <- map_df(events, function(node) {
    raw_title <- xml_find_first(node, "title") |> xml_text() |> str_trim()
    
    # --- LOCATION EXTRACTION (RSS + HTML Hybrid) ---
    raw_desc <- xml_find_first(node, "description") |> xml_text() |> str_trim()
    
    # Strategy A: Parse the description as HTML and look for the exact CSS class you found
    desc_html_loc <- NA_character_
    if (!is.na(raw_desc) && str_detect(raw_desc, "<")) {
      parsed_desc <- tryCatch(read_html(raw_desc), error = function(e) NULL)
      if (!is.null(parsed_desc)) {
        loc_node <- xml_find_first(parsed_desc, "//*[contains(@class, 'sidearm-calendar-table-cell-event-location')]")
        if (!is.na(loc_node)) {
          desc_html_loc <- xml_text(loc_node) |> str_trim()
        }
      }
    }
    
    # Strategy B: Aggressive XML Tag Search (ignores namespaces like <s:location> or <ev:location>)
    loc_tag <- xml_find_first(node, ".//*[local-name()='location']") |> xml_text() |> str_trim()
    fac_tag <- xml_find_first(node, ".//*[local-name()='facility']") |> xml_text() |> str_trim()
    
    # Strategy C: Regex fallback if it's stored as plain text in the description
    desc_regex_loc <- str_extract(raw_desc, "(?i)Location:\\s*(.*?)(?=\\r|\\n|<br|$)") |>
      str_remove("(?i)^Location:\\s*") |>
      str_remove_all("<[^>]+>") |> 
      str_trim()
    
    final_location <- case_when(
      !is.na(desc_html_loc) & desc_html_loc != "" ~ desc_html_loc,
      !is.na(loc_tag) & loc_tag != "" ~ loc_tag,
      !is.na(fac_tag) & fac_tag != "" ~ fac_tag,
      !is.na(desc_regex_loc) & desc_regex_loc != "" ~ desc_regex_loc,
      TRUE ~ NA_character_
    )
    # -----------------------------------------------
    
    # 2. EXTRACT DATE & APPLY DYNAMIC YEAR
    extracted_date <- str_extract(raw_title, "^\\d{1,2}/\\d{1,2}")
    year_date <- NA_character_
    
    if (!is.na(extracted_date)) {
      event_month <- as.integer(str_extract(extracted_date, "^\\d{1,2}"))
      event_year <- if (event_month < start_month) end_year else start_year
      year_date <- format(as.Date(paste0(event_year, "-", extracted_date), format = "%Y-%m/%d"), "%Y-%m-%d")
    }
    
    extracted_time <- str_extract(raw_title, "(?i)\\d{1,2}:\\d{2}\\s*[AP]M|TBD|All Day")
    
    clean_game_info <- raw_title
    if (!is.na(extracted_date)) clean_game_info <- str_remove(clean_game_info, fixed(extracted_date))
    if (!is.na(extracted_time)) clean_game_info <- str_remove(clean_game_info, extracted_time)
    
    tibble(
      host     = host_name,
      date     = year_date,
      time     = extracted_time,
      game     = str_trim(clean_game_info),
      location = final_location,  
      raw_text = raw_title
    )
  }) |> 
    filter(!str_detect(game, "\\sat\\s")) |> 
    mutate(
      sport = case_when(
        str_detect(game, regex("badminton", ignore_case = TRUE)) ~ "BAD",
        str_detect(game, "Baseball|BASE|\\sBB|\\sBSB") ~ "BASE",
        str_detect(game, "BOWL|Bowling") ~ "BOWL",
        str_detect(game, "Beach Volleyball|BVB") ~ "BVB",
        str_detect(game, "Cheerleading") ~ "CHEER",
        str_detect(game, "Cricket|cricket") ~ "CRKT",
        str_detect(game, "EQ|Equestrian|equestrian") ~ "EQU",
        str_detect(game, "Football|FB") ~ "FB",
        str_detect(game, "Fencing|FEN") ~ "FENCE",
        str_detect(game, "Field Hockey|FH") ~ "FH",
        str_detect(game, "Gymnastics|GYM") ~ "GYM",
        str_detect(game, "Handball") ~ "HAND",
        str_detect(game, "Ice Skating") ~ "ISKATE",
        str_detect(game, "Judo") ~ "JUDO",
        str_detect(game, "Lightweight Crew") ~ "LCREW",
        str_detect(game, "Lightweight Football") ~ "LFB",
        str_detect(game, "Archery") ~ "ARCH",
        str_detect(game, "(?<![Ww]omen's\\s)[Bb]asketball|MBB|M Basketball") ~ "MBB",
        str_detect(game, "Men's Cheerleading|M Cheerleading") ~ "MCHEER",
        str_detect(game, "Crew") ~ "CREW",
        str_detect(game, "Men's Golf|MGOLF|\\sGOLF|M Golf") ~ "MGOLF",
        str_detect(game, "Men's Gymnastics|M Gymnastics") ~ "MGYM",
        str_detect(game, "Men's Ice Hockey|Women's Ice Hockey|\\sMIH|IH|Ice Hockey|\\sWIH") ~ "IH",
        str_detect(game, "(?<![Ww]omen's\\s)[Ll]acrosse|Men's Lacrosse|MLAX|M Lacrosse|\\sLAX") ~ "MLAX",
        str_detect(game, "(?<![Ww]omen's\\s)[Ss]occer|Men's Soccer|MSOC|\\sSOC|SCCR") ~ "MSOC",
        str_detect(game, "(?<![Ww]omen's\\s)[Tt]ennis|Men's Tennis|MTEN|\\sTEN") ~ "MTEN",
        str_detect(game, "Men's Volleyball|MVB|M Volleyball|\\sVB") ~ "MVB",
        str_detect(game, "Pistol") ~ "PIST",
        str_detect(game, "Polo") ~ "POLO",
        str_detect(game, "Rodeo") ~ "RDO",
        str_detect(game, "Rifle|RIFLE") ~ "RIFL",
        str_detect(game, "Rowing|ROW") ~ "ROW",
        str_detect(game, "Rugby") ~ "RUG",
        str_detect(game, "Sailing|SAIL") ~ "SAIL",
        str_detect(game, "Softball|SB|SBALL") ~ "SB",
        str_detect(game, "Skiing|SKI") ~ "SKI",
        str_detect(game, "Soft Pitch Softball|SPSB") ~ "SPSB",
        str_detect(game, "Squash") ~ "SQSH",
        str_detect(game, "SWIM|[Ss]wimming [&] [Dd]iving|[Ss]wim [&] [Dd]ive|\\sSD|S/D") ~ "SWIM",
        str_detect(game, "[Tt]rack [&] [Ff]ield|T[&]F|TRACK|\\sMT|\\sWT") ~ "TRACK",
        str_detect(game, "Triathalon") ~ "TRI",
        str_detect(game, "(Volleyball|VB)") ~ "VB", 
        str_detect(game, "(?<![Mm]en's\\s)[Bb]asketball|Women's Basketball|WBB|W Basketball") ~ "WBB",
        str_detect(game, "Women's Golf|WGOLF|W Golf") ~ "WGOLF",                     
        str_detect(game, "(?<![Mm]en's\\s)[Ll]acrosse|Women's Lacrosse|WLAX|W Lacrosse|\\sLAX") ~ "WLAX",
        str_detect(game, "Water Polo|WP") ~ "WP",
        str_detect(game, "Wrestling|WREST") ~ "WREST",
        str_detect(game, "Water Skiing|WS") ~ "WSK",
        str_detect(game, "(?<![Mm]en's\\s)[Ss]occer|Women's Soccer|WSOC|\\sSOC|SCCR") ~ "WSOC",
        str_detect(game, "(?<![Mm]en's\\s)[Tt]ennis|Women's Tennis|WTEN|\\sTEN") ~ "WTEN",
        str_detect(game, "Cross Country|XC") ~ "XC",
        .default = NA_character_
      ),
      team = str_extract(game, "(?<=\\s{2}).*|(?<=\\bvs\\s).*"),
      team = str_remove(team, "\\([Ee][Xx][Hh][.]?(ibition)?\\)| - Exh(ibition)?|\\bEX\\b|\\s-\\s.*$"),
      time = case_when(
        is.na(time) ~ "TBD",
        str_detect(time, "(?i)[AP]M") ~ format(strptime(time, "%I:%M %p"), "%H:%M"),
        TRUE ~ time
      )
    ) 
  
  return(calendar_data)
}

all_filtered_ncaa_games <- filtered_ncaa_members |> 
  mutate(
    game_data = map2(
      hostname_url, 
      short_name,
      ~get_sidearm_calendar(base_url = .x, host_name = .y, start_date = "2026-07-26", end_date = "2027-12-31"),
      .progress = "Scraping NCAA Calendars"
    )
  ) |> 
  unnest(game_data, keep_empty = TRUE) |> 
  filter(!is.na(game)) |> 
  select(date, time, host, sport, team, Name, raw_text, calender) |> 
  arrange(date)

#Find missing schools
find_missing <- function(df, source_df, by){
  ref_tbl <- {{ source_df }} |> 
    group_by(pick(
      {{ by }})
      ) |> 
    summarise()
  
  summarised_df <- {{ df }} |> 
    group_by(pick (
      {{ by }})
      ) |> 
    summarise()
  
  missing <- anti_join(ref_tbl, summarised_df)
  print(missing)
}

missing_schools <- find_missing(all_filtered_ncaa_games, filtered_ncaa_members, by = c(Name, hostname_url))

write_xlsx(all_filtered_ncaa_games, "data/output/all_filtered_NCAA_games.xlsx")
