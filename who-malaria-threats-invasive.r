# display data from WHO Malaria Threats Map
# 2021-02-11 andy south
# for quick demo at DSME meeting

# data downloaded from https://apps.who.int/malaria/maps/threats/?theme=invasive
# from the xls, data sheet saved as a csv

folder <- "data-raw\\who-malaria-threats-map\\"
filename <- "MTM_INVASIVE_VECTOR_SPECIES_20210207.csv"
filename <- paste0(folder, filename)

dfwho <- read.csv(filename)

library(sf)
library(mapview)

#convert to spatial format
sfwho <- st_as_sf(dfwho, coords=c('LONGITUDE','LATITUDE'), crs=4326)

#interactive map
mapview(sfwho, zcol='INVASIVE_STATUS')
