visuals <- function(){

  fluidRow(
    column(width = 12,
           shinydashboard::tabBox(width = NULL, #selected = "Map",
                  id = "tabAnalysis",
                  tabPanel("Correlation",
                           p("Mark at least two traits above."),
                           qtlcharts::iplotCorr_output('vcor_output', height = 900)
                           #)
                  )
                  ,
                  tabPanel("Spatial Map",
                           #d3heatmap::d3heatmapOutput("fieldbook_heatmap")
                           uiOutput("fieldbook_heatmap_ui")
                  )
                  ,
                  tabPanel("Density",
                           plotOutput('phDens_output', height = 400)
                  )
                  ,

                  #
                  tabPanel("Heatmap Genotype x Trait",
                           p("Mark at least two traits above."),
                           #d3heatmap::d3heatmapOutput('phHeat_output', height = 1400)
                           uiOutput("phHeat_output_ui")
                  )
                  ,
                  tabPanel("Dendrogram Phenotypes",
                           p("Mark at least two traits above."),
                           plotOutput('phDend_output', height = 1400)
                  )
                  #,
                  #
                  # tabPanel(title = "Report",
                  #
                  #          uiOutput("aovVarsUI"),
                  #
                  #          radioButtons("aovFormat","Report format",
                  #                       c("HTML", "WORD" #, "PDF"
                  #                       ),
                  #                       inline = TRUE),
                  #          radioButtons("expType", "Experiment type",
                  #                       c("RCBD", "ABD", "CRD"
                  #                         #, "A01D"
                  #                         ), inline = TRUE),
                  #          conditionalPanel("input.expType == 'A01D'",
                  #                           selectInput("block", "BLOCK", c("BLOC", "BLOCK")),
                  #                           numericInput("k", "k", 2, 2, 5, step = 1)
                  #          ),
                  #
                  #          actionButton("fbRepoDo", "Create report!"),
                  #          HTML("<center>"),
                  #          uiOutput("fbRep"),
                  #          HTML("</center>")
                  #
                  # )

           )
    )

  )
}

get_crops <- function(amode = "Demo"){
  list.dirs(fbglobal::get_base_dir(amode), recursive = FALSE) %>% basename()
}


#' fbasingle_ui
#'
#' @param title a text
#' @author Reinhard Simon
#'
#' @return shiny tag list
#' @export
fbasingle_ui <- function(title="") {
  #tagList(


  shinydashboard::tabItem(tabName = title,
    h2("Single Chart"),
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL, collapsible = TRUE,
                 title = "Data",
                 shinydashboard::tabBox("Details", width = 12,
                  tabPanel("About",
                    HTML("Use this module to visually assess your data.</br>
                         Switch to the tab ‘Source’:</br>
                         this may take a bit as it activates the link to a remote database</br>
                         via the BrAPI protocol.</br></br>
                         The online server has a time-limit resulting in a message ‘Disconnected from server’ after some time of inactivity. Just ‘reload or refres the page’.
                         ")
                  ),
                  tabPanel("Source",
                   fluidRow(
                     column(width = 3,
                            shiny::uiOutput("ui_src_type")
                            ),
                     column(width = 3,
                            shiny::uiOutput("ui_src_filter")
                     ),
                    column(width = 6,
                           shiny::uiOutput("ui_src_fieldbook")
                           )
                   ),
                   uiOutput("fbParams")

                 ),
                 tabPanel("Fieldbook",
                          # conditionalPanel(
                          #   condition = "input.fba_src_type != 'Brapi'",
                            DT::dataTableOutput("hotFieldbook")
                          #)

                 )

             )
      )
    )
    )
    , visuals()
  )
}
