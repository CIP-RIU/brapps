
#' locations_ui
#'
#' @param id string
#' @param title string
#' @import shiny
#' @author Reinhard Simon
#' @return shiny taglist
#' @export
locations_ui <- function(id = "mapLocation", title = "Locations UI"){
  shinydashboard::tabItem(tabName = id,
    h2(title),
    fluidRow(
      column(width = 8
             ,
             shinydashboard::tabBox(width = NULL, id = id,
                    tabPanel("About",
                             HTML("This module provides a geographic summary of the breeding program(s) in the selected database.
                                  ")
                    ),
                    tabPanel("Source", value = "map_source",
                             fluidRow(
                               column(width = 3,
                                      shiny::uiOutput("ui_map_src_type")
                               ),
                               column(width = 3,
                                      shiny::uiOutput("ui_map_src_filter")
                               )
                               # ,
                               # column(width = 6,
                               #        shiny::uiOutput("ui_map_src_fieldbook")
                               # )
                             )
                    ),
                    tabPanel("Map",
                             shiny::radioButtons("ui_map_track", "Choose what to display",
                                                 choices = c("studies", "locations", "seasons" #, "genotypes"
                                                             ),
                                                 inline = TRUE),
                             leaflet::leafletOutput("mapLocs")
                    )
                    # ,
                    # tabPanel("List of studies",
                    #
                    #          HTML("<h1>Under development!</h1>")
                    # )
             )
      )
      ,
      column(width = 4,
             tabBox(width = NULL, title = "Site"
                    ,

                    tabPanel("Info",
                             htmlOutput("siteInfo")
                    )
                    ,
                    tabPanel("Altitude",
                             plotOutput("hist_alt")
                    )

                    # ,
                    # tabPanel("Fieldtrials",
                    #          htmlOutput("site_fieldtrials")
                    # )
                    # # TODOD
                    # ,
                    # tabPanel("Genotypes",
                    #          htmlOutput("site_genotypes")
                    # )

             )
      )
    )
    # ,
    #
    #
    # fluidRow(
    #   column(width = 8
    #          ,
    #          box(width = NULL,
    #              title = "Location table"
    #              ,
    #              #p(class = 'text-center', downloadButton('locsDL', 'Download Filtered Data')),
    #              DT::dataTableOutput("tableLocs")
    #              #locationsUI("location")
    #          )
    #   )
    # )
  )
}
