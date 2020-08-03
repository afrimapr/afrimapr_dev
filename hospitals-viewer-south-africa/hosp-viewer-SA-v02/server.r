# hosp-viewer-SA-v02/server.r
# andy south 2020-02
# v02 based on healthsites viewer


cran_packages <- c("leaflet","remotes")
lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))


library(remotes)
library(leaflet)
library(ggplot2)
library(patchwork) #for combining ggplots

if(!require(afrihealthsites)){
  remotes::install_github("afrimapr/afrihealthsites")
}

library(afrihealthsites)
#library(mapview)


#global variables

# to try to allow retaining of map zoom, when type checkboxes are selected
zoom_view <- NULL

urldata <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/health_system_za_hospitals_v1.csv" 

# read in data
dfsa <- read.csv(urldata)
# convert to spatial format
sfsa <- sf::st_as_sf(dfsa, coords = c("Long", "Lat"), crs = 4326, na.fail=FALSE)

function(input, output) {
  
  ######################################
  # mapview interactive leaflet map plot
  
  output$serve_healthsites_map <- renderLeaflet({
    
    #TODO remove repetition of this filter here and in table_raw
    #but currently this uses sf & the table uses df - but could put sf in the table
    type_column <- "Category"
    type_filter <- input$sel_fac_types
    type_truefalse <- tolower(sfsa[[type_column]]) %in% tolower(type_filter)
    sfsa_sel <- sfsa[type_truefalse,]    
    
    # only try to plot map if there are any data
    if (nrow(sfsa_sel) > 0)
    {
      mapplot <- mapview::mapview(sfsa_sel, 
                                  zcol='Category',                               # which column sets point colour
                                  layer.name='Category',                         # title for the legend
                                  cex = input$attribute_to_size_points,          # set point size by one of numeric columns
                                  alpha = 0.1,                                   # point borders light, but present to show light colours
                                  # label displayed on mouse hover
                                  #label=paste0(sfsa$Name,"(",sfsa$Category,")") )
                                  label=paste(sfsa_sel$Name, input$attribute_to_size_points,"=",sfsa_sel[[input$attribute_to_size_points]] ) )  
      
      # to retain zoom if only types have been changed
      if (!is.null(zoom_view))
      {
        mapplot@map <- leaflet::fitBounds(mapplot@map, lng1=zoom_view$west, lat1=zoom_view$south, lng2=zoom_view$east, lat2=zoom_view$north)
      }    
      
      #return map to be plotted
      mapplot@map
      
    } else
    {
      return(NULL)
    }
    
    
  })



  #########################################################################
  # trying to detect map zoom as a start to keeping it when options changed
  observeEvent(input$serve_healthsites_map_bounds, {

    #print(input$serve_healthsites_map_bounds)

    #save to a global object so can reset to it
    zoom_view <<- input$serve_healthsites_map_bounds
  })

  ####################################################################
  # perhaps can just reset zoomed view to NULL when country is changed
  # not needed here no change of countries
  # observe({
  #   input$country
  #   zoom_view <<- NULL
  # })


  ################################################################################
  # for UI selectable list of facility categories
  output$select_fac_type <- renderUI({

    
    fac_types <- unique(sfsa[['Category']])

    checkboxGroupInput("sel_fac_types", label = ("Facility categories"),
                       choices = fac_types,
                       selected = fac_types,
                       inline = FALSE)
  })

    
  ################################################################################
  # for UI selectable radio buttons for numeric columns (to size points by)
  output$select_size_attribute <- renderUI({
    
    columns_numeric <- which(unlist(lapply(dfsa,is.numeric)))
    
    #exclude Long, Lat & DistrictEstimatedPopulation columns
    columns_numeric2 <- columns_numeric[-which(names(columns_numeric) %in% c('Lat','Long','DistrictEstimatedPopulation'))]

    
    radioButtons("attribute_to_size_points", label = "Size points by :",
                 choices = names(columns_numeric2),
                 selected = names(columns_numeric2)[1])
    
  })  
  
  
  

  ########################
  # barplot of facility types
  output$plot_fac_types <- renderPlot({



    gg2 <- afrihealthsites::facility_types("South Africa",
                                           datasource = sfsa,
                                           plot = TRUE,
                                           type_filter = input$sel_fac_types,
                                           type_column = "Category") #,
                                           #ggcolour_h=c(185,360)
                                           #brewer_palette = "BuPu" )

    gg2




  })

  #######################
  # table of raw data
  output$table_raw <- DT::renderDataTable({

    type_column <- "Category"
    type_filter <- input$sel_fac_types
    
    type_truefalse <- tolower(dfsa[[type_column]]) %in% tolower(type_filter)
    dfsa_sel <- dfsa[type_truefalse,]
    
    DT::datatable(dfsa_sel, options = list(pageLength = 50))
    
  })




}
