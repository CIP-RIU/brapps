
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
                             HTML("")
                    ),
                    tabPanel("Source",
                             HTML("")
                    ),
                    tabPanel("Map",
                             leaflet::leafletOutput("mapLocs")
                    )
                    # ,
                    # tabPanel("Report",
                    #          htmlOutput("rep_loc")
                    #          #HTML("<h1>Under development!</h1>")
                    # )
             )
      )
      # ,
      # column(width = 4,
      #        tabBox(width = NULL, title = "Site"
      #               ,
      #               tabPanel("Histogram",
      #                        plotOutput("histogram")
      #               )
      #               ,
      #               tabPanel("Info",
      #                        htmlOutput("siteInfo")
      #               )
      #               ,
      #               tabPanel("Fieldtrials",
      #                        htmlOutput("site_fieldtrials")
      #               )
      #               # TODOD
      #               ,
      #               tabPanel("Genotypes",
      #                        htmlOutput("site_genotypes")
      #               )
      #
      #        )
      # )
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
