library(shiny)
library(brapi)
library(brapps)
library(shinydashboard)
library(d3heatmap)
library(rhandsontable)
#library(shinyURL)
library(shinyFiles)
library(DT)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP"),
                    dashboardSidebar(disable = TRUE),

                    body = dashboardBody(
                       #tabItems(
                      #   tabItem(tabName = "tab_analysis",
                            brapps::fbasingle_ui("SingleChart")
                       #)

                      #)
                    )
)

############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")

  # brapi_con("sweetpotato", "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
  #           80, "rsimon16",
  #           "sweetpotato")
  #

  #shinyURL.server()
  brapps::fieldbook_analysis(input, output, session, values)
})

shinyApp(ui, sv)

