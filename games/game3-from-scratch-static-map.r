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



ui <- fluidPage(
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 6,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 6,
           h4("Brushed points"),
           verbatimTextOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    
    #ggplot(mtcars2, aes(wt, mpg)) + geom_point()
    ggplot(sfafricountries) + geom_sf()
    
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    # nearPoints(mtcars2, input$plot1_click, addDist = TRUE)
    #nearPoints(sfafricountries, input$plot1_click, addDist = TRUE)
    input$plot1_click
    #c(input$plot1_click$x,input$plot1_click$y)
  })
  
  output$brush_info <- renderPrint({
    #brushedPoints(mtcars2, input$plot1_brush)
    brushedPoints(sfafricountries, input$plot1_brush)
  })
}

## run shinyApp ##
shiny::shinyApp( ui = ui, server = server)
