#game3-from-scratch-static-map.r

#try an alternative approach where plot a static map not leaflet
#and detect coords to find which polygons selected

#initially just allow countries to be coloured in

#oooo this memoru hex game looks like it will be really useful, e.g. has timer too
#https://github.com/dreamRs/memory-hex

#starting from a more basic example
#https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html

#TODO i think calculation is triggered twice when mouse clicked
#(possibly because updating the map retriggers click)

#working that list of countries built up from clicks

# TODO
# add random generation of country names
# fix 'play again' button


library(shiny)
#library(ggplot2)
library(afrilearndata)
library(sf)
library(glue) #?may be needed by time module

source("modules/time-module.R")
source("modules/welcome-module.R")


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 600,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click"
           )
    )
  ),
  
  time_UI("timer"),
  
  fluidRow(
    column(width = 12,
           h4("Click info"),
           verbatimTextOutput("click_info")
    ),

  )
)

server <- function(input, output, session) {
  
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  # store the list of clicked polygons in a vector
  clickedpolys <- shiny::reactiveValues( ids = vector() )
  # index of next country for user to click
  country_id_next <- 1
  clicked_country <- ""
  num_to_do <- 3 #num countries to locate
  
  shareurl <- "https://twitter.com/intent/tweet?text=I%20located%2010%20%20countries%20in%20{time}%20seconds%20!%20And%20you%20?%20%23rstats%20%23rspatial%20Play%20here:&url=https://dreamrs.shinyapps.io/memory-hex"
  
  
  output$plot1 <- renderPlot({
    
    #ggplot(sfafricountries) + geom_sf()
    
    plot(st_geometry(sfafricountries))
    
    # need to react to click
    # but want calculation to be done first
    # add clicked countries
    if( is.null( clickedpolys$ids ) ){
      # check for required values, if true, then the issue
      # is "silent". See more at: ?req
      req( clickedpolys$ids )
      } else {
      plot(st_geometry(sfafricountries)[clickedpolys$ids], add=TRUE, col='red')    
    }
    
    # if( !is.null( input$plot1_click ) ){
    #   
    #   sfpoint <- st_sfc(st_point(c(input$plot1_click$x, input$plot1_click$y)))
    #   st_crs(sfpoint) <- 4326
    #   
    #   #find which polygon clicked on
    #   poly_index <- st_within(sfpoint,sfafricountries)[[1]]
    #   #get value out of list
    #   poly_index <- poly_index[[1]]     
    # 
    #   # add clicked countries
    #   plot(st_geometry(sfafricountries)[poly_index], add=TRUE, col='red')
    #   
    #   #problem is that this seems to trigger this renderPlot again with NULL click
    #   #so the red polygon gets covered
    #   #need to be careful about reactivity
    # }

  })
  
  # this was orginally to print click info into a box
  # i'm kind of using for something else
  # to detect which country has been clicked
  output$click_info <- renderPrint({
    #input$plot1_click
    #c(input$plot1_click$x,input$plot1_click$y)
    
    if( !is.null( input$plot1_click ) ){

    # if( is.null( input$plot1_click ) ){
    #     # check for required values, if true, then the issue
    #     # is "silent". See more at: ?req
    #     req( input$plot1_click )
    #   } else {      
      
      sfpoint <- st_sfc(st_point(c(input$plot1_click$x, input$plot1_click$y)))
      st_crs(sfpoint) <- 4326

      #find which polygon clicked on
      poly_index <- st_within(sfpoint,sfafricountries)[[1]]
      #get value out of list
      poly_index <- poly_index[[1]]
      #which country name at that index
      clicked_country <- sfafricountries$name[poly_index]

      # store clicked country if it hasn't been clicked before
      #if ( ! (poly_index %in% clickedpolys$ids))
      
      # store clicked country if it is correct
      # (might no longer be necessary to check existing list as above)
      if ( poly_index == country_id_next &
           ! (poly_index %in% clickedpolys$ids) )  
      {
        clickedpolys$ids <<- c( clickedpolys$ids, poly_index )  
        
        # change next country to choose
        # later make this random, for now increment
        country_id_next <<- country_id_next + 1
      }
    }

    #c(poly_index, clicked_country)
    cat("target:",sfafricountries$name[country_id_next], 
        " clicked:", clicked_country,
        " all clicked:", clickedpolys$ids)  
    
  })
  
  
  #ending the game, copied from 
  #https://github.com/dreamRs/memory-hex/blob/a11625a149027daa46a1fdf764e08da4cb3b4f08/server.R
  observe({
    # allfound <- all_found(results_mods_parse$all)
    # if (isTRUE(allfound)) {
    # my first effort stop when set num countries reached
    
    if (length(clickedpolys$ids) >= num_to_do) {      
      showModal(modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h2(
            tags$span(icon("trophy"), style = "color: #F7E32F;"),
            "Well done !",
            tags$span(icon("trophy"), style = "color: #F7E32F;")
          ),
          tags$h4(paste("You located",num_to_do,"countries in")),
          tags$h1(isolate(timer()), "seconds!"),
          tags$br(), tags$br(),
          tags$a(
            href = glue(shareurl, time = isolate(timer())),
            icon("twitter"), "Tweet your score !", 
            class = "btn btn-info btn-lg"
          ),
          tags$br(), tags$br(),
          
          tags$p("Check out what else we are up to at ",
                 tags$a(href = "https://afrimapr.github.io/afrimapr.website/", "afrimapr")),
          
          tags$br(), tags$br(),
          actionButton(
            inputId = "reload",
            label = "Play again !",
            style = "width: 100%;"
          )
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    }
  })  

  observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)  

}

## run shinyApp ##
shiny::shinyApp( ui = ui, server = server)
