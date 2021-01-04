#afrimapr_dev/drc_prototype_app/server.r

#display DRC health catchments and facilities for 3 provinces from GRID3, over 60 popn from worldpop

cran_packages <- c("leaflet","remotes")
lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))


library(remotes)
library(leaflet)
#library(ggplot2)
#library(patchwork) #for combining ggplots
library(mapview)

# if(!require(afrihealthsites)){
#   remotes::install_github("afrimapr/afrihealthsites")
# }
# 
# library(afrihealthsites)



#global variables

# to try to allow retaining of map zoom, when type checkboxes are selected
zoom_view <- NULL
# when country is changed I want whole map to change
# but when input$hs_amenity or input$selected_who_cats are changed I want to retain zoom
# perhaps can just reset zoomed view to NULL when country is changed


# load presaved data
#load("cod_rast6080_10km.rda")
load("sfg3area6080.rda")
load("sfg3zones.rda")
load("sfg3facilities.rda")

#to protect against potential problems with rgdal versions e.g. on shinyapps
sf::st_crs(sfg3area6080) <- 4326
sf::st_crs(sfg3zones) <- 4326
sf::st_crs(sfg3facilities) <- 4326

sfg3zonelines <- sf::st_cast(sfg3zones,"MULTILINESTRING")



# Define a server for the Shiny app
function(input, output) {

  ######################################
  # mapview interactive leaflet map plot
  output$serve_healthsites_map <- renderLeaflet({

    
    sf1 <- sfg3area6080
 
    # if zone choice is selected, select rows
    if (input$cboxzones)
    {
      sf1 <- sf1[which(sf1$zone_sante %in% input$selected_zone_names),]
      sfg3zonelines <- sfg3zonelines[which(sfg3zonelines$zone_sante %in% input$selected_zone_names),]
      sfg3facilities <- sfg3facilities[which(sfg3facilities$zone_sante %in% input$selected_zone_names),]      
    } 

 
    #plot areas (smaller)
    mapplot <- mapview(sf1, zcol='numover60s', 
                       label=paste(sf1$aire_sante," popn.>60:",sf1$numover60s),
                       layer.name="estimated popn >60 (WorldPop)",
                       lwd = 1,
                       col.regions=hcl.colors(n=6, palette="Lajolla"),
                       alpha.regions=0.8
                       )
    
    #plot zones (bigger)
    #found that mouseover always gave the zones output (not what I want) irrespective of order ?? try changing it to lines
    
    #mapplot <- mapplot + mapview(sfg3zonelines, zcol="zone_sante", color = "darkred", col.regions = "blue", alpha.regions=0, lwd = 0.5, legend=FALSE)

    mapplot <- mapplot + mapview(sfg3zonelines, zcol="zone_sante", color = "darkred", alpha.regions=0, lwd = 2, legend=FALSE)
    
    
    #facilities
    mapplot <- mapplot + mapview(sfg3facilities, zcol="type", cex=3, alpha=0,
                                 layer.name="health facilities (Grid3)",
                                 label=paste(sfg3facilities$fosa_nom))
    
    
    
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
  observeEvent(input$serve_healthsites_map_bounds, {

    #print(input$serve_healthsites_map_bounds)

    #save to a global object so can reset to it
    zoom_view <<- input$serve_healthsites_map_bounds
  })

  ####################################################################
  # perhaps can just reset zoomed view to NULL when country is changed
  # hurrah! this works, is it a hack ?
  observe({
    input$country
    zoom_view <<- NULL
  })


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
  # dynamic selectable list of who facility categories for selected country
  output$select_who_cat <- renderUI({

    # get selected country name
    #input$country

    # get categories in who for this country
    # first get the sf object - but later don't need to do that
    # TODO add a function to afrihealthsites package to return just the cats
    sfwho <- afrihealthsites::afrihealthsites(input$country, datasource = 'who', plot = FALSE)

    #who_cats <- unique(sfwho$`Facility type`)
    # allowing for 9 cat reclass
    who_cats <- unique(sfwho[[input$who_type_option]])

    #"who-kemri categories"
    checkboxGroupInput("selected_who_cats", label = NULL, #label = h5("who-kemri categories"),
                       choices = who_cats,
                       selected = who_cats,
                       inline = FALSE)
  })

  ################################################################################
  # dynamic selectable list of health zones
  output$select_zones <- renderUI({
    
    #sort for alphabetical order
    zone_names <- sort(unique(sfg3zones$zone_sante))
    
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

  ########################
  # barplot of facility types
  output$plot_fac_types <- renderPlot({


    #palletes here set to match those in map from compare_hs_sources()

    gg1 <- afrihealthsites::facility_types(input$country,
                                    datasource = 'healthsites',
                                    plot = TRUE,
                                    type_filter = input$hs_amenity,
                                    #ggcolour_h=c(0,175)
                                    brewer_palette = "YlGn",
                                    admin_level=input$cboxadmin,
                                    admin_names=input$selected_admin_names )

    gg2 <- afrihealthsites::facility_types(input$country,
                                           datasource = 'who',
                                           plot = TRUE,
                                           type_filter = input$selected_who_cats,
                                           type_column = input$who_type_option, #allows for 9 broad cats
                                           #ggcolour_h=c(185,360)
                                           brewer_palette = "BuPu",
                                           admin_level=input$cboxadmin,
                                           admin_names=input$selected_admin_names )

    # avoid error for N.Africa countries with no WHO data
    if (is.null(gg2))
    {
      gg1

    } else
    {
      #set xmax to be the same for both plots
      #hack to find max xlim for each object
      #TODO make this less hacky ! it will probably fail when ggplot changes
      max_x1 <- max(ggplot_build(gg1)$layout$panel_params[[1]]$x$continuous_range)
      max_x2 <- max(ggplot_build(gg2)$layout$panel_params[[1]]$x$continuous_range)
      #set xmax for both plots to this
      gg1 <- gg1 + xlim(c(0,max(max_x1,max_x2, na.rm=TRUE)))
      gg2 <- gg2 + xlim(c(0,max(max_x1,max_x2, na.rm=TRUE)))

      #set size of y plots to be dependent on num cats
      #y axis has cats, this actually gets max of y axis, e.g. for 6 cats is 6.6
      max_y1 <- max(ggplot_build(gg1)$layout$panel_params[[1]]$y$continuous_range)
      max_y2 <- max(ggplot_build(gg2)$layout$panel_params[[1]]$y$continuous_range)

      #setting heights to num cats makes bar widths constant between cats
      gg1 / gg2 + plot_layout(heights=c(max_y1, max_y2)) #patchwork
    }



  })

  #######################
  # table of grid3 health areas data
  output$table_areas <- DT::renderDataTable({

    # drop the geometry column - not wanted in table
    sf1 <- sf::st_drop_geometry(sfg3area6080)

    # if zone choice is selected, select rows
    if (input$cboxzones)
    {
      sf1 <- sf1[which(sf1$zone_sante %in% input$selected_zone_names),]
    }     
    
    # names(sfg3area6080)
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
