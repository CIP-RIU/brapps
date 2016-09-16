library(shiny)
#library(brapi)
library(brapps)
library(shinydashboard)
library(d3heatmap)
library(rhandsontable)
library(shinyURL)
library(shinyFiles)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP"),
                    dashboardSidebar(disable = TRUE),
                    #dashboardSidebar(
                      # sidebarMenu(
                      #   menuItem("Phenotype",
                      #     menuSubItem("Analysis",
                      #            tabName = "tab_analysis", icon = icon("map-o"))
                      #            #,
                      #            #numericInput("fbaInput", "Fieldbook ID", 142, 1, 9999),
                      #
                      #     ,
                      #     HTML("<div style='display:none'>"),
                      #     shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                      #     #shinyURL.ui("URL", tinyURL = F)
                      #     HTML("</div>")
                      #
                      #
                      #   )
                      # )
                    #)
                    #,
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

