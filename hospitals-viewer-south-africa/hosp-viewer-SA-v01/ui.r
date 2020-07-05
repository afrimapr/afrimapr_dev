# hosp-viewer-SA-v01/server.r
# andy south 2020-06
# simple first version

cran_packages <- c("shiny","leaflet")

lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(leaflet)


fluidPage(

  headerPanel('Hospitals in South Africa'),

  p("A simple demo viewer made with R from data collated by DSFSI"),

  sidebarLayout(

    sidebarPanel( width=3,
  
      p("data from",
                    a("Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa", href="https://github.com/dsfsi/covid19za", target="_blank")),
  
      p("UI by ", a("afrimapr", href="http://www.afrimapr.org", target="_blank"),
        ": creating R building-blocks to ease use of open health data in Africa"),
  
      p("Open source ", a("R code", href="https://github.com/afrimapr/afrimapr_dev/tree/master/hospitals-viewer-south-africa/hosp-viewer-SA-v01", target="_blank")),
      
      # dynamic who category selection
      #uiOutput("select_who_cat"),
  
      p("active development July 2020, v01\n"),
  
      p("Input and suggestions ", a("welcome", href="https://github.com/afrimapr/suggestions_and_requests", target="_blank")),
      #  "Contact : ", a("@southmapr", href="https://twitter.com/southmapr", target="_blank")),
  
      p(tags$small("Disclaimer : Data used by afrimapr are sourced from published open data sets. We provide no guarantee of accuracy.")),
  
    ),
  
    
    mainPanel(
  
      # display the map
      leafletOutput("serve_healthsites_map", height=1000)
  
    )
  )
)


