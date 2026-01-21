# Introduciton to Data Science coursework

############################################################
# 0. Packages
############################################################

# Required Packages:
# openmeteo, ggplot2, tidyverse, sf, randomForest, 
# patchwork, FactoMineR, factoextra, scales, lubridate

# openAQ download (if needed)
pak::pkg_install("openaq/openaq-r@*release")
#

library(openaq)
library(openmeteo)
library(ggplot2)
library(tidyverse)
library(sf)
library(randomForest)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(scales)
library(lubridate)

#API Key
set_api_key("b012a62cf391274dc5f7901f9b6484359fd703a376910daac2bb9edbee432a73")

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
############################################################
# 2.4 Exploring Data
############################################################
############################################################

############################################################
# 2.4.1 Exploratory data analysis
############################################################


# EDA Air quality  
# 1) Summary statistics - Table 4
air_quality_summary <- aq_clean %>%   
  group_by(location, parameter_name) %>% 
  summarise(mean=mean(value),sd=sd(value), min=min(value),
            max=max(value), count=n())

air_quality_summary$mean <- round(air_quality_summary$mean, 2)
air_quality_summary$sd <- round(air_quality_summary$sd, 2)

#2) Monthly observation counts - Figure 3 
# Calculating counts 
air_quality_month_counts <- aq_clean %>% 
  mutate(
    month=floor_date(datetime_from, "month")) %>% 
      group_by(location, parameter_name, month) %>% 
      summarise(count=n(), groups="drop")

# Converting suscript
air_quality_month_counts$parameter_name <- 
  factor(air_quality_month_counts$parameter_name,
         levels=c("no2", "pm25"),
         labels=c("NO[2]","PM[2.5]"))

# Visualisation appropriate labels
site_labels <- c(
  "barnsley_gawber"="Barnsley Gawber",
  "barnsley_road"="Barnsley Road",
  "chesterfield_loundsley"="Chesterfield Loundsley",
  "chesterfield_roadside"="Chesterfield Roadside",
  "devonshire"="Devonshire",
  "ladybower"="Ladybower",
  "tinsley"="Tinsley")

# Producing line graphs
obs_line <- ggplot(air_quality_month_counts, 
                   aes(x = month, y = count, colour = parameter_name)) +
  geom_line() +
  facet_wrap(~location, ncol = 2, scales = "free_y", labeller=labeller(location=site_labels)) +
  scale_colour_discrete(name="Parameter Name", labels=scales::label_parse())+ # from scales, allows subscript on legend
  theme_minimal()+
  labs(x="Month", y="Observation Count")+
  theme(axis.text = element_text(size=12),
  axis.title = element_text(size=14, face="bold"),
  strip.text=element_text(size=13))

print(obs_line)

# Final Figure 3
ggsave("obs_line.png", plot=obs_line, width=11.7, height=8.3, dpi=300)
#

# 3) Average monthly mean no2 and pm25 - Figure 2
# Calculating means
aq_mean_month <- aq_clean %>%
  mutate(month = month(datetime_from, label = TRUE)) %>%
  group_by(month, location, parameter_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# Subscript labels
aq_mean_month_units <- aq_mean_month %>%
  mutate(
    parameter_unit = dplyr::recode(
      parameter_name,
      "no2"="NO[2]",
      "pm25"="PM[2.5]"))

# Producing line graphs
air_quality_month_plot <- ggplot(aq_mean_month_units, aes(x=month, y=mean_value, colour=location, group=location))+
geom_line()+
  scale_color_brewer(palette="Dark2", name="Location", # Colour blind safe palette
                     labels = c(
                       "barnsley_gawber"="Barnsley Gawber",
                       "barnsley_road"="Barnsley Road",
                       "chesterfield_loundsley"="Chesterfield Loundsley Green",
                       "chesterfield_roadside"="Chesterfield Roadside",
                       "devonshire"="Devonshire",
                       "ladybower"="Ladybower",
                       "tinsley"="Tinsley"))+
  facet_wrap(~parameter_unit, labeller=label_parsed)+
  labs(x="Month",
    y = expression(bold("Mean Concentration ("*mu*"g/m"^3*")")))+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"),
        strip.text=element_text(size=12),
        legend.position="bottom",
        legend.text=element_text(size=12))

# Final Figure 2
ggsave("aq_months.png",plot=air_quality_month_plot, width=10, height = 8, dpi=300)
#

############################################################

# EDA Meteorology

#1) Summary Statistics - Table 5

weather_summary <- weather_clean %>% group_by(location) %>% 
  summarise(mean_temp=mean(hourly_temperature_2m), 
            sd_temp=sd(hourly_temperature_2m),
           mean_wind=mean(hourly_windspeed_10m),
           sd_wind=sd(hourly_windspeed_10m),
           mean_precipitation=mean(hourly_precipitation),
           sd_precipitation=sd(hourly_precipitation),
           mean_radiation=mean(hourly_direct_radiation),
           sd_radiation=sd(hourly_direct_radiation),
           n_obs=n())

#2) Plots for monthly parameter averages - Figure 4

# Calculating averages

weather_monthly_averages <- 
  weather_clean %>% mutate(
    month=month(datetime_from, label=TRUE)) %>% 
  group_by(location, month) %>% 
  summarise(mean_temp=mean(hourly_temperature_2m),
            mean_wind=mean(hourly_windspeed_10m),
            mean_precipitation=mean(hourly_precipitation),
            mean_radiation=mean(hourly_direct_radiation))

# Four plots need to be produced because of different scales, but will be rejoined 

# Temperature
Temp_plot <- ggplot(weather_monthly_averages, aes(x=month, y=mean_temp, 
                                     colour=location, group=location))+
  geom_line()+
  scale_color_brewer(palette="Dark2",name="Location", 
                       labels = c(
    "barnsley_gawber"="Barnsley Gawber",
    "barnsley_road"="Barnsley Road",
    "chesterfield_loundsley"="Chesterfield Loundsley Green",
    "chesterfield_roadside"="Chesterfield Roadside",
    "devonshire"="Devonshire",
    "ladybower"="Ladybower",
    "tinsley"="Tinsley"))+
  labs(x="Month")+
  ylab("Mean Hourly Temperature (\u00B0C)")+ # Degrees celsius 
  theme_minimal()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14, face="bold"))

print(Temp_plot)

# Wind speed

Wind_plot <- ggplot(weather_monthly_averages, aes(x=month, y=mean_wind, 
                                     colour=location, group=location))+
  geom_line()+
  scale_color_brewer(palette="Dark2",name="Location", 
                     labels = c(
                       "barnsley_gawber"="Barnsley Gawber",
                       "barnsley_road"="Barnsley Road",
                       "chesterfield_loundsley"="Chesterfield Loundsley Green",
                       "chesterfield_roadside"="Chesterfield Roadside",
                       "devonshire"="Devonshire",
                       "ladybower"="Ladybower",
                       "tinsley"="Tinsley"))+
  labs(x="Month")+
  ylab("Mean Hourly Windspeed (Km/h)")+
  theme_minimal()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14, face="bold"))
  
# Precipitation

Prec_plot <- ggplot(weather_monthly_averages, aes(x=month, y=mean_precipitation, 
                                     colour=location, group=location))+
  geom_line()+
  scale_color_brewer(palette="Dark2",
                     name="Location", 
                     labels=c(
                       "barnsley_gawber"="Barnsley Gawber",
                       "barnsley_road"="Barnsley Road",
                       "chesterfield_loundsley"="Chesterfield Loundsley Green",
                       "chesterfield_roadside"="Chesterfield Roadside",
                       "devonshire"="Devonshire",
                       "ladybower"="Ladybower",
                       "tinsley"="Tinsley"))+
  labs(x="Month")+
  ylab("Mean Hourly Precipitation (mm)")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"))

# Radiation

Rad_plot <- ggplot(weather_monthly_averages, aes(x=month, y=mean_radiation, 
                                     colour=location, group=location))+
  geom_line()+
  scale_color_brewer(palette="Dark2",
                     name="Location", 
                     labels=c(
                       "barnsley_gawber"="Barnsley Gawber",
                       "barnsley_road"="Barnsley Road",
                       "chesterfield_loundsley"="Chesterfield Loundsley Green",
                       "chesterfield_roadside"="Chesterfield Roadside",
                       "devonshire"="Devonshire",
                       "ladybower"="Ladybower",
                       "tinsley"="Tinsley"))+
  labs(x="Month",
  y=expression(bold("Mean Hourly Direct Radiation (W/m"^2*")")))+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"))

# Combining plots using patchwork

meteorology_plots <- 
  (Temp_plot|Wind_plot)/
  (Rad_plot|Prec_plot)+
  plot_layout(guides="collect")&
  theme(
    legend.position = "bottom",
    legend.text=element_text(size=12),
    plot.margin=margin(10, 20, 10, 10) ,
    axis.title.y=element_text(margin=margin(r = 10))) #shuffle margins

print(meteorology_plots)

# Final figure 4
ggsave("meteorology_plot.png", plot=meteorology_plots, width=14,
       height=10,
       dpi=300)
#

############################################################

# EDA combined data

#1) Complete observation counts - Table 6

obs_counts <- join_aq_weather_filtered %>% 
  group_by(location) %>% 
  summarise(count=n())

# Visual friendly location names

obs_counts <- obs_counts %>%
  mutate(location=dplyr::case_when(
    location=="barnsley_gawber"~"Barnsley Gawber",
    location=="barnsley_road"~"Barnsley Road",
    location=="chesterfield_roadside"~"Chesterfield Roadside",
    location=="chesterfield_loundsley" ~"Chesterfield Loundsley",
    location=="devonshire"~"Devonshire",
    location=="ladybower"~"Ladybower",
    location=="tinsley"~"Tinsley",
    TRUE ~ location))

print(obs_counts)

#2) Correlation matrix - Table 7

# selecting numerical variables 

cor_data <- join_aq_weather_filtered %>% 
  select(hourly_temperature_2m, hourly_windspeed_10m, hourly_precipitation,
         hourly_direct_radiation, pm25, no2)

# Performing correlation analysis and rounding for readability

cor_matrix <- cor(cor_data, method="pearson", use="complete.obs")
cor_matrix <- round(cor_matrix, 2)

# Final Table 7
View(cor_matrix)
#

# 3) Histograms - Figure 5

# Pivot longer (takes all columns and forms one column, allows facet wrapping)
join_aq_weather_long <- join_aq_weather_filtered %>% 
  select(-c(location, datetime_from)) %>% 
  pivot_longer(everything(),
               names_to="parameter",
               values_to="value")

# Changing variable names for visual, inclduing appropriate units
join_aq_weather_long_unit <- join_aq_weather_long %>%
  mutate(
    parameter_unit = dplyr::recode(
      parameter,
      "hourly_temperature_2m"="Temperature~(degree*C)",
      "hourly_windspeed_10m"="Wind~Speed~(Km/h)",
      "hourly_precipitation"="Precipitation~(mm)",
      "hourly_direct_radiation"="Direct~Radiation~(W/m^2)",
      "no2"="NO[2]~(mu*g/m^3)",
      "pm25"="PM[2.5]~(mu*g/m^3)"))

# Producing histograms
combined_distb <- ggplot(join_aq_weather_long_unit, aes(x=value))+
  geom_histogram(bins=40)+
  facet_wrap(~parameter_unit, 
             scales="free_x", 
             labeller=label_parsed)+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"),
        strip.text=element_text(size=13))+
  labs(x="Parameter",
       y="Count")
print(combined_distb)

# Final Figure 5
ggsave("combined_distb.png", plot=combined_distb, width=12,
       height=8, dpi=300)
# 

############################################################
# 2.4.2 Principal Component Analysis
############################################################

# 1) Selecting Variables

pca_data <- join_aq_weather_filtered %>% select(hourly_temperature_2m, 
                                       hourly_windspeed_10m, 
                                       hourly_precipitation, 
                                       hourly_direct_radiation, 
                                       pm25, 
                                       no2)
pca_data_renamed <- pca_data

# 2) Reanming for later visualisation

colnames(pca_data_renamed) <- c("Temperature", "Windspeed", "Precipitation",
                                   "Direct Radiation", "PM2.5", "NO2")
# 3) Performing PCA

pca_scaled <- prcomp(pca_data_renamed, scale=TRUE) # scaling the data

# 4) Producing loadings plot - Figure 7

# Loadings

aq_loading <- data_frame(
  variable=colnames(pca_data_renamed),
  PC1=pca_scaled$rotation[,1],
  PC2=pca_scaled$rotation[,2])

aq_loading <- data_frame(
  variable=colnames(pca_data_renamed),
  PC1=pca_scaled$rotation[,1],
  PC2=pca_scaled$rotation[,2],
  PC3=pca_scaled$rotation[,3])

aq_loading <- aq_loading %>% mutate(
  across(where(is.numeric), round, 3))

# Plotting
basic_lp <- fviz_pca_var(pca_scaled)+
  theme(axis.text=element_text(size=14),
        axis.title = element_text(size=14, face="bold"))+
  ggtitle("")

# Final Figure 7
ggsave("basic_lp.png",plot=basic_lp, 
       width=8, height=6,
       dpi=300)
#


# Not used but interesting - Loadings for each combination of PC 1-3
lp1 <- fviz_pca_var(pca_scaled, col.var="cos2",
             gradient.cols=c("#00AFBB", "#E7B800", 
                             "#FC4E07"),axes=c(1,2))

lp2 <- fviz_pca_var(pca_scaled, col.var="cos2",
             gradient.cols=c("#00AFBB", "#E7B800", 
                             "#FC4E07"),axes=c(2,3))

lp3 <- fviz_pca_var(pca_scaled, col.var="cos2",
             gradient.cols=c("#00AFBB", "#E7B800", 
                             "#FC4E07"),axes=c(1,3))

combined_loading_plots <- 
  (lp1|lp2|lp3)+
  plot_layout(guides="collect")
print(combined_loading_plots)
var <- get_pca_var(pca_scaled)
#

# 5) Deciding Number of PCs - Figure 6

summary(pca_scaled)

# Converting to data frame
pca_values <- as.data.frame((pca_scaled$x))

# Producing a Scree Plot 
scree_plot_pca <- fviz_eig(pca_scaled, addlabels=TRUE, ylim=c(0,50))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14, face="bold"))+
  ggtitle("")
print(scree_plot_pca)

# Final Figure 6
ggsave("scree_plot_pca.png", scree_plot_pca, width=8, 
       height=6, dpi=300)
#

# Majority of variance explained by three PCs

pca_data_three <- pca_values[,1:3] # majority of variance explained by 3 PCA


############################################################
# 2.4.3 - K meanns clustering
############################################################

#1) Decide how many clusters to look at
n_clusters <- 6

#2) Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

#3) Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(pca_data_three, centers = i, nstart = 50, iter.max=1000)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

#4) Produce an elbow plot - Figure 8

km_elbow_plot <- tibble(k = 1:n_clusters, wss = wss) %>%
  ggplot(aes(k, wss)) +
  geom_line() +
  geom_hline(
    yintercept=wss, 
    linetype = 'dashed', 
    col=c(rep('#000000',3),'#FF0000', rep('#000000', 2)))+
  geom_point(size = 4) +
  scale_x_continuous(breaks = 1:n_clusters) +
  labs(x = "Number of Clusters (k)", 
       y = "Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()+
  theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=14, face="bold"))

print(km_elbow_plot)

# Final Figure 8
ggsave("km_elbow.png", km_elbow_plot, width=8, height=6, dpi=300)
#

# Majoirty of variance explained by 4 clusters

#5) Fitting the model for 4 clusters

k <- 4
set.seed(123)
km.out <- kmeans(pca_data_three, centers=k, nstart=50, iter.max=1000)

############################################################
# 2.4.4 Cluster Characterisation
############################################################

#1) Assigning cluster labels

clustered_data <- join_aq_weather_filtered

clustered_data$cluster_id <- factor(km.out$cluster)

#2) Summarising clusters 

cluster_summary <-  clustered_data %>% 
  group_by(cluster_id) %>%
  summarise(n_obs=n(),
            across(where(is.numeric), mean, na.rm = TRUE))

#3) Characetrising clusters - Table 8

cluster_levels <- cluster_summary %>%
  mutate(across(
      where(is.numeric) &!n_obs,~ cut(., #selecting numeric variables except count
        breaks=quantile(., probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), # convert columns to categories, breaks variable into thirds 
        labels=c("Low", "Medium", "High"), # Assigns low medium and high to each tertile
        include.lowest=TRUE))) # NA on lowest means if not included

# Final Table 8
print(cluster_levels)
#

############################################################
# 2.4.5 Site Cluster Assignments
############################################################

#1) Calculating number of observations at each cluster at each site 

cluster_obs_site <- clustered_data %>% 
  group_by(location, cluster_id) %>% 
  summarise(count=n())

#2) Calculating  proportion of observations at each cluster

cluster_props_site <- cluster_obs_site %>% 
  group_by(location) %>% 
  mutate(prop= count / sum(count))

#3) Rearranging locations to a more logical order

cluster_props_site$location <- factor(cluster_props_site$location,
                                      levels = c("barnsley_road", "tinsley", "devonshire",
                                                 "chesterfield_roadside","barnsley_gawber","chesterfield_loundsley","ladybower"))
#4) Plotting cluster proportions - Figure 9
cluster_prop <- ggplot(cluster_props_site, aes(x = location, y = prop, fill = cluster_id)) +
  geom_bar(stat="identity", width=0.8)+
  scale_fill_brewer(palette="Set2")+ # colour blind firendly palette
  scale_x_discrete(labels=c("Barnsley\n Road", "Tinsley", "Devonshire",
                            "Chesterfield \nRoadside","Chesterfield\n Loundsley", "Barnsley\n Gawber",
                            "Ladybower"))+
  labs(x="Location", y="Cluster Assignment Proportion", fill="Cluster ID")+
  theme_minimal()+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title=element_text(size=16, face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=14, face="bold"),
        legend.text =element_text(size=20),
        axis.title.x=element_text(margin=ggplot2::margin(t=10)),
        axis.title.y=element_text(margin=ggplot2::margin(r=10)))


print(cluster_prop)

# Final figure 9
ggsave("cluster_prop.png", plot=cluster_prop, width=14, height=10, dpi=300)        
#

############################################################
# 3.1.4 - Random Forest Classification
############################################################
  
clustered_data

#1) Randomly shuffling data

aq_weather_modelling <- clustered_data[
    sample(1:nrow(clustered_data)),] 

#2) Selecting predictor and target variables 

aq_weather_modelling_ready <- aq_weather_modelling %>% 
    select(cluster_id, hourly_temperature_2m, hourly_windspeed_10m,
           hourly_precipitation, hourly_direct_radiation, pm25, no2) %>% 
    mutate(cluster_id=factor(cluster_id))

#3) Creating test and train data

train.size=0.7 # 70:30 split
  
aq_train <- aq_weather_modelling_ready[
    1:(train.size*nrow(aq_weather_modelling_ready)),]
  
aq_test <- aq_weather_modelling_ready[
    (nrow(aq_train)+1):nrow(aq_weather_modelling_ready),]

#4) Training the model  
rf_model <- randomForest(
    cluster_id~hourly_temperature_2m+ hourly_windspeed_10m+
      hourly_precipitation+ hourly_direct_radiation+ no2+ pm25, 
    data=aq_train)
  
print(rf_model)
  
#5) Evaluating model accuracy using test data
rf_model_predictions <- predict(rf_model, aq_test)
  
rf_model_classification_error <- mean(
    rf_model_predictions != aq_test$cluster_id)

print(rf_model_classification_error)

# Model Accuracy  
print(paste('Accuracy', 1-rf_model_classification_error))
#
 
#6) Visualising accuracy- Figure 10

model_confusion_matrix <- table(aq_test$cluster_id,
                                  rf_model_predictions)
  
print(model_confusion_matrix)
  
model_confusion_matrix_df <- as.data.frame(model_confusion_matrix)
  colnames(model_confusion_matrix_df) <- c("Actual", "Predicted", "Freq")
  
Rf_heatmap <- ggplot(model_confusion_matrix_df, aes(x=Predicted, y=Actual))+
    geom_tile(aes(fill=Freq), colour="white")+
    geom_text(aes(label=Freq), vjust=0.5, fontface="bold", size=5)+
    scale_fill_gradient(low="lightblue", high="steelblue")+
    labs(x="Predicted Label", 
         y="Actual Label")+
    theme_minimal() +
  theme(axis.text=element_text(size=14),
                          axis.title=element_text(size=14, face="bold"))
print(Rf_heatmap)
# Final Figure 10
ggsave("rf_heatmap.png", Rf_heatmap, width=8, height=6, dpi=300)
  

############################################################
############################################################
#END
############################################################
############################################################

  

