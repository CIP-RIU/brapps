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

# get_crops <- function(amode = "Demo"){
#   list.dirs(fbglobal::get_base_dir(amode), recursive = FALSE) %>% basename()
# }


#' fbasingle_ui
#'
#' @param title a text
#' @author Reinhard Simon
#'
#' @return shiny tag list
#' @export
fbasingle_ui <- function(title=""){
  #tagList(

  bdb <- brapi::ba_db()

  ndb <- names(bdb)
  ndb <- ndb[!ndb %in% "mockbase"]


  shinydashboard::tabItem(tabName = title,
    h2("Single Chart"),
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL, collapsible = TRUE,
                 title = "Data",
                 shinydashboard::tabBox("Details", width = 12,
                  tabPanel("Source",
                   fluidRow(
                     column(width = 3,
<<<<<<< HEAD
                            radioButtons("fba_src_type", "Select a source type",
                                         list("Default" = "Default"
                                              ,
                                              "Database (using BrAPI)" = "Brapi"
                                              ,"File" = "Local"
=======
                            radioButtons("fba_src_crop", "Select a crop",
                                         #get_crops(),
                                         c("potato", "sweetpotato"),
                                         inline = TRUE)
                            ),
                     column(width = 3,
                            radioButtons("fba_src_type", "Select a source type",
                                         list(#"Default" = "Default"
                                              #,
                                              # "Database (using BrAPI)" = "brapi"
                                              #,
                                              "File" = "Local"
>>>>>>> 0b1b69c51027f8fadccb799ebd4d6bd6b84c53c1
                                         ),
                                         "Default",
                                         inline = TRUE),


                            conditionalPanel(
                              condition = "input.fba_src_type == 'Local'",

                              shinyFilesButton('fb_Input',
                                               label = 'File select',
                                               title = 'Please select a file', multiple=FALSE)
                            )
                            ),
                     column(width = 3,
                            conditionalPanel(
                              condition = "input.fba_src_type != 'Brapi'",
                              radioButtons("fba_src_crop", "Select a crop",
                                           get_crops(),
                                           inline = TRUE)
                            )
                     ),
                    column(width = 6,
                           conditionalPanel(
                             condition = "input.fba_src_type == 'Default'",
                             selectInput("fbaInput", "Fieldbook", choices = NULL)
                           ),
                           conditionalPanel(
                             condition = "input.fba_src_type == 'Local'",
                             verbatimTextOutput('filepaths')
                           ),
                           conditionalPanel(
                             condition = "input.fba_src_type == 'Brapi'",
                             shiny::selectInput("baui_bdb", "BrAPI database", ndb),
                             shiny::checkboxInput("baui_chk_prg", "Use Breeding Programs as filter", value = FALSE),
                             shiny::uiOutput("baui_prgs"),
                             shiny::uiOutput("baui_stds")
                           )

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
