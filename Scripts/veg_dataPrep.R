library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#load data
veg <- read.csv('Data/ltrm_vegsrs_data.csv') %>%
  dplyr::mutate(Year = format(mdy(DATE), '%Y')) %>%
  dplyr::mutate(Month = month(mdy(DATE), label = T)) %>%
  dplyr::mutate(DATE = as.Date(format(mdy(DATE), '%Y-%m-%d'))) %>%
  dplyr::filter(!is.na(EAST_15))

#convert UTM to lat lon
utm_crs <- 3261

#Convert to sf object
utm_sf <- st_as_sf(veg, coords = c('EAST_15', 'NORTH_15'), crs = utm_crs)

#transform to geographic coordinates
latlon_sf <- st_transform(utm_sf, crs = 4326)

#make into data frame
latlon_df <- as.data.frame(st_coordinates(latlon_sf))
colnames(latlon_df) <- c('lng', 'lat')

#combine with water quality data
veg <- cbind(veg, latlon_df)

#Save data...
write.csv(veg, 'Data/Pool8_AqVeg_SRS.csv')
