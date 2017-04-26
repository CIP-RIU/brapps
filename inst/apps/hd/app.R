library(brapps)
library(shinydashboard)
library(d3heatmap)
library(qtlcharts)
library(dplyr)
library(withr)
library(DT)


ui <- dashboardPage(skin = "yellow",

      dashboardHeader(title = "HIDAP",
                    dropdownMenuOutput("notificationMenu")),
      dashboardSidebar(
        sidebarMenu(
                   menuItem("Single Trial Analysis",
                            menuSubItem("Single trial graph", tabName = "singleChart", icon = icon("calculator")),
                            menuSubItem("Selection response", tabName = "selResponse", icon = icon("indent"))
        )
      )),
      dashboardBody(
        tabItems(
          brapps::rts_ui("selResponse"),
          brapps::fbasingle_ui("singleChart")
        )

      )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "local")

  brapps::fieldbook_analysis(input, output, session, values)
  brapps::rts_sv(input, output, session, values)
})

shinyApp(ui, sv)









