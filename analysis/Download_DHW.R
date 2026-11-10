library(lubridate)
library(tidyverse)
library(dplyr)
library(maps)
library(purrr)
library(readxl)
library(stringr)

setwd('C:\\Local_Files\\PAN_psbA\\analysis')


### Read in marinegeo file to get site coordinates 
coords<- read_excel("C:\\Local_Files\\SCTLD_samples\\Survey_Data\\MarineGEO_Caribbean_coral_data_entry_template.xlsx", sheet = 'SITES')
#check 
nrow(coords)
names(coords)

# change formatting 
coords$`Site Name` <- gsub(" ", "_", coords$`Site Name`)
coords$`Site Name`<- tolower(coords$`Site Name`)
coords$`Site Longitude`<-round(as.numeric(coords$`Site Longitude`), digits = 5)
colnames(coords) <- gsub(" ", "_", colnames(coords))
colnames(coords)<- tolower(colnames(coords))

#chosing sites
pattern <- "stri_point|juan_point|crawl_cay|rhino_reef"

#filter for sites
coords <- coords %>%
  dplyr::filter(str_detect(site_name, pattern))
#check
View(coords)

# set range date
dates <- seq.Date(
  from = as.Date("2022-01-01"),
  to   = as.Date("2023-06-01"),
  by   = "day"
)

# Download DHW 
#install.packages('furrr')
library(furrr)
plan(multisession, workers = 8)

safe_read <- safely(read.csv)

# Create a grid of all site/date combinations
site_dates <- expand.grid(
  site = unique(coords$site_name),
  date = dates,
  stringsAsFactors = FALSE
)

# Add latitude/longitude to the grid
site_dates <- site_dates %>%
  left_join(coords %>% select(site_name, site_latitude, site_longitude),
            by = c("site" = "site_name"))
site_dates <- site_dates %>%
  rename(latitude = site_latitude,
         longitude = site_longitude)

# download dhw data from NOAA CRW ----------------------------------------------------------------------

# Parallel download with safe_read
# future_map - map but in parallel 
# input in fxn is each row number 
results_list <- future_map(1:nrow(site_dates), function(i) {
  row <- site_dates[i, ]
  url <- paste0(
    "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_DHW[(",
    as.Date(row$date), "T12:00:00Z)][(", row$latitude, ")][(", row$longitude, ")]"
  )
  out <- safe_read(url)

    # pull non-error results 
  if (!is.null(out$result)) {
    df <- out$result
      
      # remove row with just labels 
    df <- df %>% filter(!CRW_DHW %in% c("Celsius weeks", "degree_C"))
      
      # and attach metadata 
    df$date <- as.Date(row$date)
    df$site <- row$site
    df$latitude <- row$latitude
    df$longitude <- row$longitude
    return(df)
  } else {
    return(NULL)
  }
})
combined_dhw_df <- bind_rows(results_list)

tail(combined_dhw_df,11)

length(unique(combined_dhw_df$site))


# run again 2023-2025 ------------------------------------------------------------------------------------
# set range date
# (cant run all at once)
later_dates <- seq.Date(from = as.Date("2023-06-02"),
                        to   = as.Date("2025-01-01"),
                        by   = "day")

# Create a grid of all site/date combinations
site_dates2 <- expand.grid(
  site = unique(coords$site_name),
  date = later_dates,
  stringsAsFactors = FALSE)

# Add latitude/longitude to the grid
site_dates2 <- site_dates2 %>%
  left_join(coords %>% select(site_name, site_latitude, site_longitude),
            by = c("site" = "site_name"))
site_dates2 <- site_dates2 %>%
  rename(latitude = site_latitude,
         longitude = site_longitude)

results_list2 <- future_map(1:nrow(site_dates), function(i) {
  row <- site_dates2[i, ]
  url <- paste0(
    "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_DHW[(",
    as.Date(row$date), "T12:00:00Z)][(", row$latitude, ")][(", row$longitude, ")]"
  )
  out <- safe_read(url)
  
  # pull non-error results 
  if (!is.null(out$result)) {
    df <- out$result
    
    # remove row with just labels 
    df <- df %>% filter(!CRW_DHW %in% c("Celsius weeks", "degree_C"))
    
    # and attach metadata 
    df$date <- as.Date(row$date)
    df$site <- row$site
    df$latitude <- row$latitude
    df$longitude <- row$longitude
    return(df)
  } else {
    return(NULL)
  }
})
combined_dhw_df2 <- bind_rows(results_list2)

tail(combined_dhw_df2,11)

# merge
dhw_all<-rbind(combined_dhw_df, combined_dhw_df2)

# export 
write.csv(dhw_all,'crw_dhw.csv')

# Download SST ---------------------------------------------------------------------

sites<-unique(coords$site_name)

# set range date
later_dates <- seq.Date(from = as.Date("2022-01-01"),
                        to   = as.Date("2025-01-01"),
                        by   = "day")

# Create a grid of all site/date combinations
site_dates2 <- expand.grid(
  site = sites,
  date = later_dates,
  stringsAsFactors = FALSE)

# Add latitude/longitude to the grid
site_dates2 <- site_dates2 %>%
  left_join(coords %>% select(site_name, site_latitude, site_longitude),
            by = c("site" = "site_name"))
site_dates2 <- site_dates2 %>%
  rename(latitude = site_latitude,
         longitude = site_longitude)

results_list3 <- future_map(1:nrow(site_dates2), function(i) {
  row <- site_dates2[i, ]
  url <- paste0(
    "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?CRW_SST[(",
    as.Date(row$date), "T12:00:00Z)][(", row$latitude, ")][(", row$longitude, ")]"
  )
  out <- safe_read(url)
  
  # pull non-error results 
  if (!is.null(out$result)) {
    df <- out$result
    
    #   # remove row with just labels 
    # df <- df %>% filter(!CRW_DHW %in% c("Celsius weeks", "degree_C"))
    
    # and attach metadata 
    df$date <- as.Date(row$date)
    df$site <- row$site
    df$latitude <- row$latitude
    df$longitude <- row$longitude
    return(df)
  } else {
    return(NULL)
  }
})
combinedsst <- bind_rows(results_list3)

# filter out rows with units 
combinedsst<-combinedsst %>%
  filter(!time == "UTC")

tail(combinedsst,11)

summary(combinedsst)

# export
write.csv(combinedsst, "sst.csv")

