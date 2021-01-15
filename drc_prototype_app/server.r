#afrimapr_dev/drc_prototype_app/server.r

#display DRC health catchments and facilities for 3 provinces from GRID3, over 60 popn from worldpop

cran_packages <- c("leaflet","remotes")
lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))


#library(remotes)
library(leaflet)
#library(ggplot2)
#library(patchwork) #for combining ggplots
library(mapview)

# if(!require(afrihealthsites)){
#   remotes::install_github("afrimapr/afrihealthsites")
# }
#library(afrihealthsites)



#global variables

# to try to allow retaining of map zoom, when type checkboxes are selected
zoom_view <- NULL
# when country is changed I want whole map to change
# but when input$hs_amenity or input$selected_who_cats are changed I want to retain zoom
# perhaps can just reset zoomed view to NULL when country is changed


# load presaved data
#load("cod_rast6080_10km.rda")
load("sfg3area6080keep5.rda")
#load("sfg3zones.rda")
load("sfg3zonelineskeep5.rda")
load("sfg3facilities.rda")

#to protect against potential problems with rgdal versions e.g. on shinyapps
sf::st_crs(sfg3area6080keep5) <- 4326
#sf::st_crs(sfg3zones) <- 4326
sf::st_crs(sfg3zonelineskeep5) <- 4326
sf::st_crs(sfg3facilities) <- 4326


# I could subset points & save as object to increase load speed
# sfhs <- afrihealthsites::afrihealthsites("cod", datasource='healthsites', plot=FALSE, 
#                                          hs_amenity=c('clinic','doctors','pharmacy','hospital'),
#                                          admin_level=1,
#                                          admin_names=c('Lomami','Haut-Lomami','Tanganyika')) #'Haut-Katanga'


# Define a server for the Shiny app
function(input, output) {

  ######################################
  # mapview interactive leaflet map plot
  output$serve_healthsites_map <- renderLeaflet({

    
    #sf1 <- sfg3area6080keep5
 
    # if zone choice is selected, select rows
    if (input$cboxzones)
    {
      sfg3area6080keep5 <- sfg3area6080keep5[which(sfg3area6080keep5$zone_sante %in% input$selected_zone_names),]
      sfg3zonelineskeep5 <- sfg3zonelineskeep5[which(sfg3zonelineskeep5$zone_sante %in% input$selected_zone_names),]
      sfg3facilities <- sfg3facilities[which(sfg3facilities$zone_sante %in% input$selected_zone_names),]      
    } 

    # to set length of colour palette to length of data by interpolation partly to avoid warnings from mapview
    # colorRampPalette() returns a function that accepts the number of categories
    col.regions <- grDevices::colorRampPalette(hcl.colors(n=6, palette="Lajolla"))
 
    #plot areas (smaller)
    mapplot <- mapview(sfg3area6080keep5, zcol='numover60s', 
                       label=paste(sfg3area6080keep5$aire_sante," popn.>60:",sfg3area6080keep5$numover60s),
                       layer.name="estimated popn >60 (WorldPop)",
                       lwd = 1,
                       col.regions=col.regions,
                       alpha.regions=0.8
                       )
    
    #plot zones (bigger)
    #found that mouseover always gave the zones output (not what I want) irrespective of order ?? try changing it to lines
    
    #mapplot <- mapplot + mapview(sfg3zonelines, zcol="zone_sante", color = "darkred", col.regions = "blue", alpha.regions=0, lwd = 0.5, legend=FALSE)

    #mapplot <- mapplot + mapview(sfg3zonelines, zcol="zone_sante", color = "darkred", alpha.regions=0, lwd = 2, legend=FALSE)
    mapplot <- mapplot + mapview(sfg3zonelineskeep5, zcol="zone_sante", color = "darkred", alpha.regions=0, lwd = 2, legend=FALSE)
    
    
    #grid3 facilities
    mapplot <- mapplot + mapview(sfg3facilities, zcol="type", cex=4, alpha=0,
                                 layer.name="health facilities (Grid3)",
                                 label=paste0("grid3 facility:",sfg3facilities$fosa_nom))
    
    #healthsites facilities - maybe make them optional ?
    # mapplot <- mapplot + mapview(sfhs, cex=3, alpha=0, layer.name="healthsites.io",
    #                              col.regions=hcl.colors(n=4, palette="Reds", rev=FALSE),
    #                              zcol='amenity', label=paste0("healthsites facility:",sfhs$name))
    
    #add selected admin regions
    #mapview::mapview(sfadmin_sel, zcol="shapeName", color = "darkred", col.regions = "blue", alpha.regions=0.01, lwd = 2, legend=FALSE)
    
    
    # breaks <- c(0,1,10,100,1000,10000,105000)
    # 
    # #palname <- "Oslo"
    # palname <- "Lajolla"
    # 
    # mapplot <- mapview::mapview(rast6080_10km, col.regions=hcl.colors(n=length(breaks)-1, palette=palname, rev=FALSE), at=breaks )   
    # 
    # mapplot <- mapplot + mapview(sfg3area6080, zcol='numover60s')
    
    
    # mapplot <- afrihealthsites::compare_hs_sources(input$country,
    #                                                datasources=c('healthsites','who'),
    #                                                plot='mapview',
    #                                                plotshow=FALSE,
    #                                                hs_amenity=input$hs_amenity,
    #                                                type_column = input$who_type_option, #allows for 9 broad cats
    #                                                who_type=input$selected_who_cats,
    #                                                admin_level=input$cboxadmin,
    #                                                admin_names=input$selected_admin_names)

    # to retain zoom if only types have been changed
    # if (!is.null(zoom_view))
    # {
    #   mapplot@map <- leaflet::fitBounds(mapplot@map, lng1=zoom_view$west, lat1=zoom_view$south, lng2=zoom_view$east, lat2=zoom_view$north)
    # }


    #important that this returns the @map bit
    #otherwise get Error in : $ operator not defined for this S4 class
    mapplot@map

    })

  #########################################################################
  # trying to detect map zoom as a start to keeping it when options changed
  # observeEvent(input$serve_healthsites_map_bounds, {
  # 
  #   #print(input$serve_healthsites_map_bounds)
  # 
  #   #save to a global object so can reset to it
  #   zoom_view <<- input$serve_healthsites_map_bounds
  # })

  ####################################################################
  # perhaps can just reset zoomed view to NULL when country is changed
  # hurrah! this works, is it a hack ?
  # observe({
  #   input$country
  #   zoom_view <<- NULL
  # })


  ###################################
  # to update map without resetting everything use leafletProxy
  # see https://rstudio.github.io/leaflet/shiny.html
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # BUT I don't quite know how to use with a mapview map ...
  # observe({
  #   #pal <- colorpal()
  #   # leafletProxy("map", data = filteredData()) %>%
  #   #   clearShapes() %>%
  #   #   addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #   #              fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #   #  )
  # })



  ################################################################################
  # dynamic selectable list of health zones
  output$select_zones <- renderUI({
    
    #sort for alphabetical order
    zone_names <- sort(unique(sfg3zonelineskeep5$zone_sante))
    
    #should I allow multiple regions or just one ?

    selectInput("selected_zone_names", label = NULL, #label = h5(""),
                choices = zone_names,
                selected = zone_names[1],
                size=5, selectize=FALSE, multiple=TRUE)
  })  
  
  ################################################################################
  # dynamic selectable list of admin regions for selected country [&later admin level]
  output$select_admin <- renderUI({

    # get selected country name
    #input$country

    # get categories in who for this country
    # first get the sf object - but maybe later don't need to do that
    # TODO? add a function to afriadmin package to return just the cats
    sfadmin <- afriadmin::afriadmin(input$country, datasource = 'geoboundaries', plot = FALSE)

    #sort for alphabetical order
    admin_names <- sort(unique(sfadmin$shapeName))

    #should I allow multiple regions or just one ?
    #problem doing this as checkboxGroupInput is that it takes up loads of space
    #better  MVP may be to offer selectInput() with just one regions selectable
    #or selectInput even with multiple selections allowed takes less space
    #checkboxGroupInput("selected_admin_names", label = NULL, #label = h5("who-kemri categories"),
    selectInput("selected_admin_names", label = NULL, #label = h5("who-kemri categories"),
                       choices = admin_names,
                       selected = admin_names[1],
                       size=5, selectize=FALSE, multiple=TRUE)
  })


  #######################
  # table of grid3 health areas data
  output$table_areas <- DT::renderDataTable({

    # drop the geometry column - not wanted in table
    sf1 <- sf::st_drop_geometry(sfg3area6080keep5)

    # if zone choice is selected, select rows
    if (input$cboxzones)
    {
      sf1 <- sf1[which(sf1$zone_sante %in% input$selected_zone_names),]
    }     
    
    # names(sfg3area6080keep5)
    # [1] "province"   "zs_uid"     "zone_sante" "as_uid"     "aire_sante" "nom_alt"    "note"       "source"     "edite_date" "area_sqkm" 
    # [11] "Shape_Leng" "Shape_Area" "ID"         "numover60s" "density"    "ncells"     "geometry"      
    
    # remove some other columns
    #toinclude <- c("zone_sante","aire_sante","source","edite_date","numover60s","ncells")
    toinclude <- c("zone_sante","aire_sante","source","numover60s")
    
    sf1 <- sf1[,which(names(sf1) %in% toinclude)]
    
    #sort so that highest popns appear top
    sf1 <- sf1[order(sf1$numover60s, decreasing=TRUE),]
    
    DT::datatable(sf1, options = list(pageLength = 50))

  })

  #######################
  # table of grid3 facilities data
  output$table_facilities <- DT::renderDataTable({
    
    # drop the geometry column - not wanted in table
    sf1 <- sf::st_drop_geometry(sfg3facilities)
    
    # if zone choice is selected, select rows
    if (input$cboxzones)
    {
      sf1 <- sf1[which(sf1$zone_sante %in% input$selected_zone_names),]
    }    
    
    # names(sfg3facilities)
    # [1] "province"   "zs_uid"     "zone_sante" "as_uid"     "aire_sante" "as_nom_alt" "fosa_uid"   "fosa_nom"   "fosa_nom2"  "type"      
    # [11] "type_abr"   "source"     "date_utc"   "lat"        "lon"        "geometry"   
    
    # remove some other columns
    toinclude <- c("fosa_nom", "type", "zone_sante","aire_sante","source","date_utc")
    
    sf1 <- sf1[,which(names(sf1) %in% toinclude)]
    
    DT::datatable(sf1, options = list(pageLength = 50))
    
  })  
  
  ###############################
  # table of raw healthsites data
  output$table_raw_hs <- DT::renderDataTable({

    sfhs <- afrihealthsites::afrihealthsites(input$country, datasource = 'healthsites', hs_amenity = input$hs_amenity, plot = FALSE,
                                             admin_level=input$cboxadmin,
                                             admin_names=input$selected_admin_names)

    # drop the geometry column and few others - not wanted in table
    sfhs <- sf::st_drop_geometry(sfhs)
    sfhs <- sfhs[, which(names(sfhs)!="iso3c" & names(sfhs)!="country")]

    DT::datatable(sfhs, options = list(pageLength = 50))
  })


}
