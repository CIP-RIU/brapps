visuals <- function(){

  fluidRow(
    column(width = 12,
           shinydashboard::tabBox(width = 12, #selected = "Map",
                  id = "tabAnalysis",

                  tabPanel("About",
                           HTML("Use this module to visually assess your data.</br>
                                Switch to the tab \" Data Source\":</br><br/>
                                in the online version this may take a bit as it directly activates the link to a remote database</br>
                                via the BrAPI protocol.</br></br>
                                The online backend server has a time-limit resulting in a message \"Disconnected from server\" after some time of inactivity. Just \"reload or refresh the page\".
                                ")
                           ),
                  tabPanel("Data Source",
                           fluidRow(
                             column(width = 3,
                                    shiny::uiOutput("ui_src_type"),
                                    shiny::uiOutput("ui_src_filter"),
                                    shiny::uiOutput("ui_src_fieldbook")
                             ),
                             column(width = 3,
                                    #uiOutput("fbParams")
                                    shiny::selectInput("fba_set_gen", "Genotype", choices = NULL) ,
                                    shiny::selectInput("fba_set_blk", "Block", choices = NULL),
                                    shiny::selectInput("fba_set_plt", "Plot", choices= NULL),
                                    shiny::selectInput("fba_set_rep", "Replication", choices = NULL)
                             ),
                             column(width = 5,
                                    shiny::selectInput("fba_set_trt", "Traits", choices = NULL, multiple = TRUE)

                                    )
                           )

                  ),
                  tabPanel("Fieldbook",
                           # conditionalPanel(
                           #   condition = "input.fba_src_type != 'Brapi'",
                           DT::dataTableOutput("hotFieldbook")
                           #)

                  ),

                  tabPanel("Density",
                           plotOutput('phDens_output', height = 400)
                  )
                  ,
                  tabPanel("Spatial Map",
                           #d3heatmap::d3heatmapOutput("fieldbook_heatmap")
                           uiOutput("fieldbook_heatmap_ui")
                  )
                  ,
                  tabPanel("Correlation",
                           p("Mark at least two traits above."),
                           qtlcharts::iplotCorr_output('vcor_output', height = 900)
                           #)
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
                  ),
                  tabPanel("Trait Network",
                           p("Mark at least four traits above."),
                           plotOutput('phNet_output', height = 1400)
                  )
                  ,

                  tabPanel(title = "Report",

                           uiOutput("aovVarsUI"),

                           radioButtons("aovFormat","Report format",
                                        c('PDF', 'HTML', 'Word'),
                                        inline = TRUE),
                           radioButtons("expType", "Experiment type",
                                        c("RCBD", "ABD", "CRD", "A01D"

                                          ), inline = TRUE)
                           ,

                           conditionalPanel(
                             condition = "input.expType == 'A01D'",

                             shiny::numericInput('fba_src_k', 'Select Block Size',   value = 2, min = 2, max = 100)
                           ),

                           downloadButton("fbRepo", "Download Report!")

                  )

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

    visuals()
  )
}
