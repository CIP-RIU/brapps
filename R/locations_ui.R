climSource <- function() {
  HTML("Data source for the the average annual temperature and
                                  annual total rainfall are interpolated data from WorldClim.org.")
}


help_loc <- function() {
  tagList(
    p("This module provides a geographic summary of
                              the breeding program(s) in the selected database."),
    HTML("<ol>
         <li>Select the tab 'Source'</li>
         <li>Select a data source.</li>
         <li>Optionally select a filter</li>
         <li>Select the tab 'Map'</li>
         <li>The map shows a marker or an aggregation of markers
         for each distinct item according to the chosen category in the radiobox. Clicking on an
         individual marker will show a popup with its name. If locations are chosen
         then on the far right more details will be shown.
         </li>
         <li>Use the mouse to move and zoom on the map.
         </li>
         </ol>
         ")
  )
}


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
                            help_loc()

                    ),

                    tabPanel("Source", value = "map_source",


                             fluidRow(
                               column(width = 3,
                                      #actionButton("btn_demo_locs", "Demo this module."),
                                      shiny::uiOutput("ui_map_src_type")
                               ),
                               column(width = 3,
                                      shiny::uiOutput("ui_map_src_filter")
                               )

                             )

                    ),


                    tabPanel("Map",
                             #rintrojs::introBox(
                             shiny::radioButtons("ui_map_track", "Choose what to display",
                                                 choices = c("locations", "studies", "seasons" #, "genotypes"
                                                             ),
                                                 inline = TRUE),
                             leaflet::leafletOutput("mapLocs")
                             # ,
                             #
                             # data.step = 2,
                             # data.intro = "Then switch to the map tab. You can select which
                             # statistics on locality related data are shown. Zooming in on circles
                             # disaggregate to distinct location markers. Clicking on an individual marker will display
                             # its name. If locations were selected site characteristics are shown
                             # in the far right box."
                             # )
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

                    tabPanel("Site Info",
                             climSource(),
                             htmlOutput("siteInfo")
                    )
                    ,
                    tabPanel("Site Altitude",
                             plotOutput("hist_alt")
                    )
                    ,
                    tabPanel("Site Envelope",
                             climSource(),
                             plotOutput("chart_env")
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
