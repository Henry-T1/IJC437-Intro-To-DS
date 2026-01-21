# Introduciton to Data Science coursework - download only script 

############################################################
# 0. Packages
############################################################

# Required Packages:
# openmeteo, tidyverse, lubridate
# openAQ download (if needed)
pak::pkg_install("openaq/openaq-r@*release")
#

library(openaq)
library(openmeteo)
library(tidyverse)
library(lubridate)

#API Key
set_api_key("YOUR_API_KEY")

############################################################
############################################################
# 2.2 Gathering Data
############################################################
############################################################

############################################################
# Air Quality 
############################################################

# Sensor parameters
list_parameters()

# parameters of interest = pm2.5, NO2  (id=2,5)

Sheffield_sensor_locations <- list_locations(coordinates=                
                                               c(latitude=53.3811, # Coordinates of Sheffield City Centre
                                                 longitude=-1.4701),
                                             radius=25000, parameters_id = c(2,5)) # Max radius 

Sheffield_sensor_locations <- Sheffield_sensor_locations %>% 
  filter(datetime_first<="2024-01-01")  # Avoiding sensors with only a small range of data


CFR <- list_location_sensors(865) %>% select(c('id', 'parameters_id')) %>% mutate(location="chesterfield_roadside")
BG <- list_location_sensors(2323) %>% select(c('id', 'parameters_id')) %>% mutate(location="barnsley_gawber")
Lb <- list_location_sensors(2337) %>% select(c('id', 'parameters_id')) %>% mutate(location="ladybower")
Ts <- list_location_sensors(2454) %>% select(c('id', 'parameters_id')) %>% mutate(location="tinsley")
DG <- list_location_sensors(2508) %>% select(c('id', 'parameters_id')) %>% mutate(location="devonshire")
LG <- list_location_sensors(2551) %>% select(c('id', 'parameters_id')) %>% mutate(location="chesterfield_loundsley")
BR <- list_location_sensors(3527) %>% select(c('id', 'parameters_id')) %>% mutate(location="barnsley_road")

# CFR, BG, Lb..... = unique ids for the parameters at each site

Combined_sensor_id <- bind_rows(CFR, BG, Lb,  # all unique parameter ids
                                Ts, DG, LG,
                                BR)

# filtering out not needed parameters 

Combined_sensor_id <- Combined_sensor_id %>%
  filter(parameters_id %in% c(2,5)) %>% rename("sensor_id"=id) # parametr id from list_parameter( )

# Turning the Ids into a vector so can be passed through function

sensor_ids <- Combined_sensor_id$sensor_id
sensor_ids_character <- as.character(sensor_ids)
str(sensor_ids_character)

# Function to download the data 

# GENERATED WITH ASSISTANCE FROM CHATGPT!!

# Main loop

download_openaq_data <- function(sensor_ids_character,
                                 start_date = as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                                 end_date   = as.POSIXct("2025-12-31 00:00:00", tz = "UTC"),
                                 output_file = "Overall_SY_air_quality.csv",
                                 save_every = 20) {
  
  dates <- seq(start_date, end_date, by = "1 month")
  total_slices <- length(dates) - 1
  
  all_data <- list()
  all_data_df <- NULL
  slice_count <- 0
  
  for (sensor in sensor_ids_character) {
    cat("\n=== Starting sensor:", sensor, "===\n")
    
    for (i in 1:total_slices) {
      
      slice_start <- dates[i]
      slice_end   <- dates[i + 1] - 1
      
      page <- 1
      
      repeat {
        temp <- tryCatch({
          list_sensor_measurements( # Core function from openaq
            sensors_id     = sensor,
            datetime_from  = slice_start,
            datetime_to    = slice_end,
            limit          = 1000,
            page           = page )}, 
          error = function(e) {
          if (grepl("429|500", e$message)) {
            cat("API limit/server error. Retrying in 5 seconds...\n") #Retry rather than aborting after API errors
            Sys.sleep(5)
            return(NULL)}
            else stop(e)})
        
        if (is.null(temp) || nrow(temp) == 0) break
        
        temp$sensor_id <- sensor
        all_data[[length(all_data) + 1]] <- temp
        
        page <- page + 1
        Sys.sleep(1) }
      
      slice_count <- slice_count + 1
      
      cat(sprintf("Processed slice %d/%d (Sensor %s)\n",
                  slice_count,
                  total_slices * length(sensor_ids),
                  sensor))
      
      if (slice_count %% save_every == 0 && length(all_data) > 0) {# Save in monthly slices
        temp_df <- dplyr::bind_rows(all_data)
        if (!is.null(all_data_df)) {
          temp_df <- dplyr::bind_rows(all_data_df, temp_df)}
        write.csv(temp_df, output_file, row.names = FALSE)
        cat("Progress saved after slice", slice_count, "\n")
      
        all_data_df <- temp_df
        all_data <- list()}}}
  
  if (length(all_data) > 0) {
    temp_df <- dplyr::bind_rows(all_data)
    if (!is.null(all_data_df)) {
      temp_df <- dplyr::bind_rows(all_data_df, temp_df)}
    write.csv(temp_df, output_file, row.names = FALSE)
    all_data_df <- temp_df
    cat("Final progress saved.\n")}
  
  return(all_data_df)}

result <- download_openaq_data()

# End of function. Result: all sensor data between 2016 and 2025 for parameters 2 and 5 

# RAW AIR QUALITY DATA FILE 

overall_SY_aq <- read.csv("overall_SY_air_quality.csv")

############################################################
#Meteorology
############################################################

# Weather download model
# GENERATED WITH ASSISTANCE FROM CHATGPT!!
weather_locations <- Sheffield_sensor_locations %>%  # Obtaining coordinates for the sensors 
  select(name, latitude, longitude) 

download_chunk_weather <- function(lat, lon, chunk_start, chunk_end, vars) {
attempt <- 1
  
repeat {
res <- tryCatch({
      weather_history( # Core function from openmeteo
        location = c(lat, lon),
        start = format(chunk_start, "%Y-%m-%d"),
        end   = format(chunk_end,   "%Y-%m-%d"),
        hourly = vars,
        timezone = "Europe/London")}, 
      error = function(e) NULL)
    
    if (!is.null(res)) return(res)
    
    attempt <- attempt + 1
    cat("Weather API error. Retrying in 10s (Attempt ", attempt, ")...\n", sep = "")# Retry rather than abort
    Sys.sleep(10)}}

download_weather_data <- function(weather_locations,
                                  variables,
                                  start_date,
                                  end_date,
                                  output_file = "combined_weather_SY.csv",
                                  save_every = 100) {
  
 dates <- seq(start_date, end_date, by = "month") #Monthly time slices 
  total_slices <- length(dates) - 1
  
  all_data_list <- list()
  all_data_df   <- NULL
  slice_count   <- 0
  
  for (idx in seq_len(nrow(weather_locations))) {
    
    site_name <- weather_locations$name[idx]
    lat       <- weather_locations$latitude[idx]
    lon       <- weather_locations$longitude[idx]
    safe_site <- gsub("[/ ]", "_", site_name) 
    
    cat("\n=== Starting site:", safe_site, "===\n")
    
    for (s in seq_len(total_slices)) {
      
      slice_start <- dates[s]
      slice_end   <- dates[s + 1] - 1
      
      tmp <- download_chunk_weather(lat, lon, slice_start, slice_end, variables)
      
      if (nrow(tmp) > 0) {
        tmp$site      <- safe_site
        tmp$latitude  <- lat
        tmp$longitude <- lon
        all_data_list[[length(all_data_list) + 1]] <- tmp}
      
      slice_count <- slice_count + 1
    
  if (slice_count %% save_every == 0 && length(all_data_list) > 0) {
        temp_df <- dplyr::bind_rows(all_data_list)
        if (!is.null(all_data_df)) {
          temp_df <- dplyr::bind_rows(all_data_df, temp_df)}
        
        write.csv(temp_df, output_file, row.names = FALSE)
        cat("Saved progress at slice", slice_count, "\n")
        
        all_data_df   <- temp_df
        all_data_list <- list()}
      Sys.sleep(2)}}
  
  if (length(all_data_list) > 0) {
    final_df <- dplyr::bind_rows(all_data_list)
    if (!is.null(all_data_df)) {
      final_df <- dplyr::bind_rows(all_data_df, final_df) }
    all_data_df <- final_df}
 
  all_data_df$datetime <- lubridate::parse_date_time(
    all_data_df$datetime,
    orders = c("ymd HMS", "ymd HM", "ymd"))
  
  write.csv(all_data_df, output_file, row.names = FALSE)
  cat("\nFinal weather data saved to:", output_file, "\n")
  
  return(all_data_df)}

start_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
end_date   <- as.POSIXct("2025-12-31 00:00:00", tz = "UTC")

variables <- c( #selected variables inputted through function
  "temperature_2m",
  "windspeed_10m",
  "precipitation",
  "direct_radiation")

combined_weather_sites <- download_weather_data(
  weather_locations = weather_locations,
  variables=variables,
  start_date=start_date,
  end_date=end_date,
  output_file="combined_weather_SY.csv",
  save_every=100)

# RAW WEATHER FILE
weather_data <- read.csv("combined_weather_SY.csv")


############################################################
############################################################
# 2.3 Structuring Data
############################################################
############################################################

# Raw air quality
overall_SY_aq <- read.csv("data/overall_SY_air_quality.csv")

# Raw meteorology
weather_data <- read.csv("data/combined_weather_SY.csv")

############################################################
# Air Quality
############################################################


# 1) Select only relevant AQ parameetrs
Overall_SY_aq<- overall_SY_aq %>% 
  select('value', 'parameter_id', 'parameter_name', 
         'datetime_from', 'datetime_to', 'sensor_id')

#2) Rejoining observations to locations
just_id <- Combined_sensor_id %>% 
  select(sensor_id, location)

aq_location <- overall_SY_aq %>% 
  left_join(just_id, by="sensor_id")


#3) Addressing non midnight values, negative values and cleaning
aq_clean <- aq_location %>% select(value,parameter_name, datetime_from, location) %>% 
  mutate(
    datetime_from = ifelse(
      nchar(datetime_from) == 10,
      paste0(datetime_from, " 00:00:00"),
      datetime_from),
    datetime_from = ymd_hms(datetime_from, tz = "UTC")) %>% 
  na.omit() %>% 
  filter(value>=0)

# Clean AQ file
write.csv(aq_clean, "Air_Quality_Clean.csv")
#

head(aq_clean, 10) # Table 1

############################################################
# Meteorology
############################################################

#1) standardising times, removing NA and unifying location names
weather_clean <- weather_data %>% 
  mutate(
    datetime = ifelse( # If timestamp is missing hours add 00:00:00 - no potential to add incorrectly, mindight is the only missing timestamp
      nchar(datetime) == 10,
      paste0(datetime, " 00:00:00"),
      datetime),
    datetime = ymd_hms(datetime, tz = "UTC")) %>% 
  na.omit() %>% 
  rename("datetime_from"=datetime, "location"=site) %>% 
  mutate(location =
           case_when(
             location == "Chesterfield_Loundsley_Green" ~ "chesterfield_loundsley", 
             location=="Sheffield_Barnsley_Road"~ "barnsley_road", 
             location=="Sheffield_Devonshire_Green"~ "devonshire", 
             location=="Sheffield_Tinsley"~"tinsley",
             location=="Chesterfield_Roadside"~"chesterfield_roadside",
             location=="Barnsley_Gawber"~"barnsley_gawber",
             location=="Ladybower"~"ladybower",
             TRUE~location))


weather_clean$datetime_from <- as.POSIXct(weather_clean$datetime_from)
weather_clean <- weather_clean %>% select(-c(latitude, longitude))
View(head(weather_clean, 10))

# Clean Weather file 
write.csv(weather_clean, "Meteorology_clean.csv")

head(weather_clean, 10) # Table 2

############################################################
# Combined Data
############################################################
#1) join based on shared columns
join_aq_weather <- inner_join(
  weather_clean, aq_clean, by=c("location", "datetime_from"))

#2) Pivot so no2 and pm25 have their own columns 
join_aq_weather <- join_aq_weather %>% pivot_wider(names_from=parameter_name, # distinct columns for each parameter
                                                   values_from=value) 

#3) Clean, remove NAs
join_aq_weather <- na.omit(join_aq_weather)

#4) Filter to restrict analysis to >2023

join_aq_weather_filtered <- join_aq_weather %>% filter(datetime_from>="2023-01-01")

# Clean combined file
write.csv(join_aq_weather_filtered, "Clean_combined_obs.csv") 

head(join_aq_weather_filtered, 10) # Table 3

############################################################
# Cleaned Files
############################################################

# Air quality
aq_clean <- read.csv("aq_clean.csv")
#Meteorology
weather_clean <- read.csv("weather_clean.csv")
#Combined
join_aq_weather_filtered <- read.csv("join_aq_weather_filtered.csv")

############################################################
#END
############################################################
