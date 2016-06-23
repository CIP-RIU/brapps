library(brapi)
library(brapps)
library(shinydashboard)
library(d3heatmap)
library(shinyURL)
library(qtlcharts)
library(leaflet)
library(dplyr)
library(withr)
library(DT)

env_dashboard <- function(){

  tabItem(tabName = "env_dashboard",
          fluidRow(
            column(width = 8
                   ,
                   tabBox(width = NULL, id = "tabLocation",
                          tabPanel("Map",
                                   leafletOutput("mapLocs")
                          )
                          ,
                          tabPanel("Report",
                                   htmlOutput("rep_loc")
                                   #HTML("<h1>Under development!</h1>")
                          )
                   )
            )
            ,
            column(width = 4,
                   tabBox(width = NULL, title = "Site"
                          ,
                          tabPanel("Histogram",
                                   plotOutput("histogram")
                          )
                          ,
                          tabPanel("Info",
                                   htmlOutput("siteInfo")
                          )
                          ,
                          tabPanel("Fieldtrials",
                                   htmlOutput("site_fieldtrials")
                          )
                          # TODOD
                          ,
                          tabPanel("Genotypes",
                                   htmlOutput("site_genotypes")
                          )

                   )
            )
          ),


          fluidRow(
            column(width = 8
                   ,
                   box(width = NULL,
                       title = "Location table"
                       ,
                       #p(class = 'text-center', downloadButton('locsDL', 'Download Filtered Data')),
                       DT::dataTableOutput("tableLocs")
                       #locationsUI("location")
                   )
            )
          )
  )

}


brapi_host = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu"
#globalVariables(c("values", "crop", "mode"))

get_plain_host <- function(){
  host = stringr::str_split(Sys.getenv("BRAPI_DB") , ":80")[[1]][1]
  if(host == "") host = brapi_host
  if(stringr::str_detect(host, "@")){
    if(stringr::str_detect(host, "http://")) {
      host = stringr::str_replace(host, "http://", "")
    }
    host = stringr::str_replace(host, "[^.]{3,8}:[^.]{4,8}@", "")
  }
  host
}

host = get_plain_host()

ui <- dashboardPage(skin = "yellow",

                    dashboardHeader(title = "HIDAP",
                                  dropdownMenuOutput("notificationMenu")),
                    dashboardSidebar(
                      sidebarMenu(

                        menuItem("Phenotype", icon = icon("leaf"),
                                 menuSubItem("Analysis",
                                             tabName = "phe_dashboard", icon = icon("calculator"))


                                 #numericInput("fbaInput", "Fieldbook ID", 142, 1, 9999)


                        ),

                        menuItem("Environment", tabName = "env_dashboard", icon = icon("globe")
                      )
                      # ,
                      # menuItem("About", tabName = "inf_dashboard", icon = icon("info"))
                    )),
                    dashboardBody(
                      #tags$head(tags$style(HTML(mycss))),
                      tabItems(
                        env_dashboard()
                        ,
                        tabItem(tabName = "phe_dashboard",
                                fluidRow(
                                  column(width = 12,
                                         box(width = NULL, collapsible = TRUE,
                                             title = "Fieldbook",
                                             uiOutput("fbList"),
                                             DT::dataTableOutput("hotFieldbook")
                                             #locationsUI("location")
                                         )
                                  )

                                )
                                ,
                                fluidRow(
                                  column(width = 12,
                                         tabBox(width = NULL, selected = "Map", id = "tabAnalysis",
                                                tabPanel("Correlation",
                                                         uiOutput("fbCorrVarsUI"),
                                                         #tags$img(src = "www/35.gif"),
                                                         div(id = "plot-container",


                                                             qtlcharts::iplotCorr_output('vcor_output', height = 400)
                                                         )
                                                ),
                                                tabPanel("Map",
                                                         d3heatmap::d3heatmapOutput("fieldbook_heatmap")
                                                )
                                                ,
                                                tabPanel(title = "Report",

                                                      uiOutput("aovVarsUI"),

                                                      radioButtons("aovFormat","Report format",
                                                                   c("HTML", "WORD", "PDF"),
                                                                   inline = TRUE),

                                                      actionButton("fbRepDo", "Create report!"),
                                                      HTML("<center>"),
                                                      uiOutput("fbRep"),
                                                      HTML("</center>"),

                                                      HTML("<div style='display:none'>"),
                                                      shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                                                      #shinyURL.ui("URL", tinyURL = F)
                                                      HTML("</div>")




                                                )



                                         )
                                  )

                                )
                        )

                      )        )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")

  try({
  brapi_con("sweetpotato", "http://sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
            80, "rsimon16",
            "sweetpotato")
  })
  shinyURL.server()

  brapps::fieldbook_analysis(input, output, session, values)

  brapps::locations(input, output, session, values)


})

shinyApp(ui, sv)









