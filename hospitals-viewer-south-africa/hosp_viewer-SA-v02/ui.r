# hosp-viewer-SA-v02/ui.r
# andy south 2020-02
# v02 based on healthsites viewer

cran_packages <- c("shiny","leaflet","remotes")

lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(leaflet)
library(remotes)

if(!require(afrihealthsites)){
  remotes::install_github("afrimapr/afrihealthsites")
}

library(afrihealthsites)
# library(mapview)


fluidPage(

  headerPanel('Hospitals in South Africa'),
  
  p("data from",
    a("Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa", href="https://github.com/dsfsi/covid19za", target="_blank")),

  sidebarLayout(

  sidebarPanel( width=3,

                # allow selection of column to size points
                uiOutput("select_size_attribute"),
                
                # allow selection of facility categories
                uiOutput("select_fac_type"),
                
                
                p("UI by ", a("afrimapr", href="http://www.afrimapr.org", target="_blank"),
                  ": creating R building-blocks to ease use of open health data in Africa"),
                
                p("Open source ", a("R code", href="https://github.com/afrimapr/afrimapr_dev/tree/master/hospitals-viewer-south-africa/hosp-viewer-SA-v01", target="_blank")),
                
                p("active development July 2020, v02\n"),
                
                p("Input and suggestions ", a("welcome", href="https://github.com/afrimapr/suggestions_and_requests", target="_blank")),
                #  "Contact : ", a("@southmapr", href="https://twitter.com/southmapr", target="_blank")),
                
                p(tags$small("Disclaimer : Data used by afrimapr are sourced from published open data sets. We provide no guarantee of accuracy.")),
                
  ),

  mainPanel(

    #when just had the map
    #leafletOutput("serve_healthsites_map", height=1000)

    #tabs
    tabsetPanel(type = "tabs",
                tabPanel("map", leafletOutput("serve_healthsites_map", height=800)),
                tabPanel("facility types", plotOutput("plot_fac_types", height=600)),
                tabPanel("raw data", DT::dataTableOutput("table_raw"))
                #tabPanel("about", NULL)
    )
  )
)
)


