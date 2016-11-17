library(shiny)
library(brapi)
library(brapps)
library(shinydashboard)
library(leaflet)

is_server = TRUE
dbCrop = "sweetpotato"
dbName = "sweetpotatobase"
dbUrl = "sweetpotatobase.org"
dbBrapi = "brapi/v1"
dbPort = "80"

Sys.setenv(BRAPI_DBNAME = dbName) #TODO this should be done in response to user selection

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "HIDAP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Environment", tabName = "env_dashboard", icon = icon("map-o")


               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "env_dashboard",
              fluidRow(
                column(width = 8,
                       tabBox(width = NULL,
                           tabPanel("Map",
                             leafletOutput("mapLocs")
                             ),
                           tabPanel("Report",
                              htmlOutput("rep_loc")
                            )
                         )
                )
                ,
                column(width = 4,
                       tabBox(width = NULL, title = "Site",
                              tabPanel("Histogram",
                                       plotOutput("histogram")
                              ),
                              tabPanel("Info",
                                       htmlOutput("siteInfo")
                              ), tabPanel("Fieldtrials",
                                       htmlOutput("site_fieldtrials")
                              ),
                              tabPanel("Genotypes",
                                       htmlOutput("site_genotypes")
                              )

                       )
                )
              ),
              fluidRow(
                column(width = 12,
                  box(width = NULL,
                    title = "Location table",
                    #p(class = 'text-center', downloadButton('locsDL', 'Download Filtered Data')),
                    DT::dataTableOutput("tableLocs")
                    #locationsUI("location")
                  )
                )
              )
      )

  )
)
)

############################################################

sv <- function(input, output, session) ({
  values <- shiny::reactiveValues(crop = "sweetpotato",
                                  amode = "brapi",
                                  is_server = is_server)
   brapps::locations(input, output, session, values = values)
})

shinyApp(ui, sv)

