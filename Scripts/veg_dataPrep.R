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

#load common names
names <- read.csv('Data/ltrm_veg_sinfo_19082026.csv')

#add names to veg
veg <- left_join(veg, names, by = c('SPPCD' = 'Species.Code'))

#convert UTM to lat lon
utm_crs <- 32615

#Convert to sf object
utm_sf <- st_as_sf(veg, coords = c('EAST_15', 'NORTH_15'), crs = utm_crs)

#transform to geographic coordinates
latlon_sf <- st_transform(utm_sf, crs = 4326)

#make into data frame
latlon_df <- as.data.frame(st_coordinates(latlon_sf))
colnames(latlon_df) <- c('lng', 'lat')

#combine with water quality data
veg <- cbind(veg, latlon_df)

#get percent frequency for each year by barcode
veg.freq <- read.csv('Data/ltrm_vegsrs_data.csv')

#Save data...
write.csv(veg, 'Data/Pool8_AqVeg_SRS.csv')

#make a column of rowSums...just need to see anything greater than 0 counts as a 1...
#start with unique barcodes
codes <- unique(veg.freq$BARCODE)

#now get all represented species
species <- unique(veg.freq$SPPCD)

#make a column of presence/absence
present <- c()
for(i in 1:nrow(veg.freq)){
  if(sum(veg.freq[i,65:70])){
    present[i] = 1
  }
  else{
    present[i] = 0
  }
}
veg.freq$present <- present

#convert from long to wide format
#make easier dataframe
long.df <- data.frame(date = veg.freq$DATE, 
                      barcode = veg.freq$BARCODE,
  species = veg.freq$SPPCD, 
                      value = veg.freq$present)

#remove duplicate rows
long.df.dup <- long.df[!duplicated(long.df),]

#Still a random duplicate but not duplicate?
long.df.dup <- long.df.dup[-66693,]

#convert to wide format
wide.df <- spread(long.df.dup, key = species, value = value, fill = 0)

#fix dates
wide.df$date <- format(mdy(wide.df$date), '%Y-%m-%d')

#order by date
wide.df <- wide.df[order(wide.df$date),]

#add year
wide.df$year <- format(ymd(wide.df$date), '%Y')

#Now get unique years
years <- unique(wide.df$year)

pct.freq<- c()
for(i in 1:length(years)){
  yeari <- wide.df[wide.df$year == years[i],]
  
  #make column sums...
  freq <- (colSums(yeari[,4:ncol(yeari)-1])/nrow(yeari))*100
  
  #rebuild data frame
  x <- data.frame(year = rep(years[i], length(freq)), species = names(freq), 
                  pct.freq = round(freq, 2))
  pct.freq <- rbind(pct.freq, x)
}

#add species names
pct.freq <- left_join(pct.freq, names, by = c('species' = 'Species.Code'))

#save the file
write.csv(pct.freq, 'Data/veg_pctFreq_bySpecies.csv')
