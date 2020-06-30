# hosp-viewer-SA-v01/server.r
# andy south 2020-06
# simple first version


#cran_packages <- c("leaflet","remotes")
cran_packages <- c("mapview")
lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))


# library(remotes)
# library(leaflet)
# library(ggplot2)
# library(patchwork) #for combining ggplots
# 
# if(!require(afrihealthsites)){
#   remotes::install_github("afrimapr/afrihealthsites")
# }

#library(afrihealthsites)
library(mapview)


#global variables

urldata <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/health_system_za_hospitals_v1.csv" 

# read in data
dfsa <- read.csv(urldata)
# convert to spatial format
sfsa <- sf::st_as_sf(dfsa, coords = c("Long", "Lat"), crs = 4326, na.fail=FALSE)


# Define a server for the Shiny app
function(input, output) {

  ######################################
  # mapview interactive leaflet map plot
  
  output$serve_healthsites_map <- renderLeaflet({

    mapplot <- mapview::mapview(sfsa, 
                                zcol='Category', 
                                label=paste(sfsa$Name,"(",sfsa$Category,")"))

    #return map to be plotted
    mapplot@map

    })



}
