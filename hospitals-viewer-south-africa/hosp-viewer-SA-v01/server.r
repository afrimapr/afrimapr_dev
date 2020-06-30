# hosp-viewer-SA-v01/server.r
# andy south 2020-06
# simple first version


cran_packages <- c("mapview")
lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(mapview)

#global variables

urldata <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/health_system_za_hospitals_v1.csv" 

# read in data
dfsa <- read.csv(urldata)
# convert to spatial format
sfsa <- sf::st_as_sf(dfsa, coords = c("Long", "Lat"), crs = 4326, na.fail=FALSE)

function(input, output) {

  ######################################
  # mapview interactive leaflet map plot
  
  output$serve_healthsites_map <- renderLeaflet({

    mapplot <- mapview::mapview(sfsa, 
                                zcol='Category',                               # which column sets point colour
                                layer.name='Category',                         # title for the legend
                                label=paste(sfsa$Name,"(",sfsa$Category,")"))  # label displayed on mouse hover

    #return map to be plotted
    mapplot@map

    })

}
