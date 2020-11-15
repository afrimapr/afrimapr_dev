#game3-from-scratch-static-map.r

#try an alternative approach where plot a static map not leaflet
#and detect coords to find which polygons selected

#initially just allow countries to be coloured in

#oooo this memoru hex game looks like it will be really useful, e.g. has timer too
#https://github.com/dreamRs/memory-hex

#starting from a more basic example
#https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html

library(shiny)
library(ggplot2)
library(afrilearndata)
library(sf)



ui <- fluidPage(
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click"
           )
    )
  ),
  fluidRow(
    column(width = 6,
           h4("Click info"),
           verbatimTextOutput("click_info")
    ),

  )
)

server <- function(input, output) {
  
  # store the list of clicked polygons in a vector
  clickedpolys <- shiny::reactiveValues( ids = vector() )
  
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
  
  output$click_info <- renderPrint({
    input$plot1_click
    #c(input$plot1_click$x,input$plot1_click$y)
    
    #if( !is.null( input$plot1_click ) ){

    if( is.null( input$plot1_click ) ){
        # check for required values, if true, then the issue
        # is "silent". See more at: ?req
        req( input$plot1_click )
      } else {      
      
      sfpoint <- st_sfc(st_point(c(input$plot1_click$x, input$plot1_click$y)))
      st_crs(sfpoint) <- 4326

      #find which polygon clicked on
      poly_index <- st_within(sfpoint,sfafricountries)[[1]]
      #get value out of list
      poly_index <- poly_index[[1]]
      #which country name at that index
      clicked_country <- sfafricountries$name[poly_index]

      # store clicked country if it hasn't been clicked before
      if ( ! (poly_index %in% clickedpolys$ids))
      {
        clickedpolys$ids <- c( clickedpolys$ids, poly_index )        
      }

      #c(poly_index, clicked_country)
    }
    
  })
  

}

## run shinyApp ##
shiny::shinyApp( ui = ui, server = server)
