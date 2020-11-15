# Welcome module ----------------------------------------------------------
#https://github.com/dreamRs/memory-hex/blob/a11625a149027daa46a1fdf764e08da4cb3b4f08/modules/welcome-module.R

welcome_UI <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Welcome to the afrimapr country game !"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Click on the countries as their names come up."),
      tags$p("How quickly can you do them all ?"),
      tags$p("Incorrect clicks are not penalised, only time matters."),
      tags$p("Click button below to play !")
    ), 
    footer = actionButton(
      inputId = ns("play"),
      label = "Play !",
      icon = icon("play"),
      style = "width: 100%"
    )
  )
}

welcome <- function(input, output, session) {
  
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = welcome_UI(id))
  
  observeEvent(input$play, {
    removeModal()
  })
  
  return(reactive(input$play))
}