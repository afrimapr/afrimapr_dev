#afrimapr_dev/drc_prototype_app/ui.r

#display DRC health zones and their areas and facilities for 3 provinces from GRID3, over 60 popn calculated from WorldPop gridded data

cran_packages <- c("shiny","leaflet","remotes")

lapply(cran_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(leaflet)
library(remotes)

# if(!require(afrihealthsites)){
#   remotes::install_github("afrimapr/afrihealthsites")
# }
# 
# library(afrihealthsites)
# library(mapview)


fluidPage(

  headerPanel('DRC health boundaries, facilities & popn over 60'),

  p("prototype NOT for decision support"),
  p("data for 3 provinces from ",
    a("GRID3 via HDX", href="https://data.humdata.org/dataset/drc-health-data", target="_blank"), 
    "over 60 popn from ",
    a("WorldPop ", href="https://www.worldpop.org/geodata/summary?id=50153", target="_blank")
    ),
  
  # p("Identify health facilities from the two main Africa-wide sources of open data. Potential to add other data for facilities, populations at risk & boundaries.
  #     Allows detailed comparison to allow improvement & inform vaccine rollout.", a("Background paper ", href="https://wellcomeopenresearch.org/articles/5-157", target="_blank")),

  sidebarLayout(

  sidebarPanel( width=3,

    #p(tags$strong("There are 2 main sources for locations of > 100k hospital and health facilities in Africa. Neither is perfect.
    #  This app allows detailed comparison to inform pandemic response and allow improvement.")),

    # p("data from",
    #               a("healthsites.io", href="https://www.healthsites.io", target="_blank"),
    #               " & ",
    #               a("KEMRI Wellcome", href="https://www.nature.com/articles/s41597-019-0142-2", target="_blank"),
    #               " / ",
    #               a("WHO", href="https://www.who.int/malaria/areas/surveillance/public-sector-health-facilities-ss-africa/en/", target="_blank")),

    p("by ", a("afrimapr", href="http://www.afrimapr.org", target="_blank"),
      ": creating R building-blocks to ease use of open health data in Africa"),


    #selectInput('country', 'Country', afcountries$name, size=10, selectize=FALSE, multiple=TRUE, selected="Angola"),
    #miss out Western Sahara because no healthsites or WHO
    # selectInput('country', 'Country', choices = sort(afcountries$name[!afcountries$name=="Western Sahara"]),
    #             size=5, selectize=FALSE, multiple=TRUE, selected="Angola"),
    #reduced countries list for vaccine rollout planning
    # selectInput('country', 'Country', choices = c('Democratic Republic of the Congo','Uganda','Liberia','Guinea','Sierra Leone'),
    #             size=5, selectize=FALSE, multiple=TRUE, selected='Democratic Republic of the Congo'),


    #selection by health zones
    checkboxInput("cboxzones", "Select health zones",value=FALSE),
    conditionalPanel(
      condition = "input.cboxzones",

      uiOutput("select_zones")
    ),


    # checkboxGroupInput("hs_amenity", label = "healthsites categories",
    #                    choices = list("hospital"="hospital", "clinic"="clinic", "doctors"="doctors", "pharmacy"="pharmacy", "unlabelled"="", "dentist" = "dentist"),
    #                    selected = c("hospital","clinic","doctors","pharmacy")),

    #who cats whether to display raw or 9 broad
    # selectInput("who_type_option", label = "WHO-KEMRI categories",
    #             choices = list("raw" = "Facility type", "reclassified to 9" = "facility_type_9"),
    #             selected = 1),

    # dynamic who category selection
    #uiOutput("select_who_cat"),

    p("active development January 2021, v0.1\n"),

    #p("Contact : ", a("@southmapr", href="https://twitter.com/southmapr", target="_blank")),
    p("Open source ", a("R code", href="https://github.com/afrimapr/afrimapr_dev/drc_prototype_app", target="_blank")),

    #p("\nWHO data Sub-Sahara only, symbols shown smaller, rings indicate overlap",
    #  a(",  blog post", href="https://afrimapr.github.io/afrimapr.website/blog/2020/healthsites-app/", target="_blank")),

    p("Input and suggestions ", a("welcome", href="https://github.com/afrimapr/suggestions_and_requests", target="_blank")),
    #  "Contact : ", a("@southmapr", href="https://twitter.com/southmapr", target="_blank")),
    #p("admin boundary data from ", a("geoboundaries", href="https://www.geoboundaries.org/", target="_blank")),

    p(tags$small("Disclaimer : Data used by afrimapr are sourced from published open data sets. We provide no guarantee of accuracy.")),

  ),

  mainPanel(

    #when just had the map
    #leafletOutput("serve_healthsites_map", height=1000)

    #tabs
    tabsetPanel(type = "tabs",
                tabPanel("map", leafletOutput("serve_healthsites_map", height=800)),
                #tabPanel("facility types", plotOutput("plot_fac_types", height=600)),
                tabPanel("health area table", DT::dataTableOutput("table_areas")),
                tabPanel("facilities table", DT::dataTableOutput("table_facilities"))
                #tabPanel("WHO data", DT::dataTableOutput("table_raw_who"))
                #tabPanel("about", NULL)
    )
  )
)
)


# navbarPage("healthsites in Africa, from healthsites.io and WHO", id="main",
#            tabPanel("map", leafletOutput("serve_healthsites_map", height=1000)) )
#            #tabPanel("map", mapviewOutput("serve_healthsites_map", height=1000)) )
#            #tabPanel("Data", DT::dataTableOutput("data")),
#            #tabPanel("Read Me",includeMarkdown("readme.md")))
