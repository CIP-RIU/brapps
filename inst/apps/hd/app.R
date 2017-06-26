library(brapps)
library(shinydashboard)
library(d3heatmap)
library(qtlcharts)
library(dplyr)
library(withr)
library(DT)
library(st4gi)
library(pepa)


ui <- fluidPage(
  shinytoastr::useToastr(),


  dashboardPage(skin = "yellow",


      dashboardHeader(title = "HIDAP prototype",
                    dropdownMenuOutput("notificationMenu")),
      dashboardSidebar(
        sidebarMenu(
         menuItem("Single Trial Analysis",
                  menuSubItem("Single trial graph", tabName = "singleChart", icon = icon("calculator")),
                  menuSubItem("Selection response", tabName = "selResponse", icon = icon("indent"))
        ),
        menuItem("Program Overview",
                 menuSubItem("Geographic distribution", tabName = "mapLocation", icon = icon("earth"))
                 )
      )),
      dashboardBody(
        tabItems(
          brapps::rts_ui("selResponse"),
          brapps::fbasingle_ui("singleChart")
          ,
          brapps::locations_ui("mapLocation", "Geographic overview")
        )

      )
  )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "local")

  brapps::fieldbook_analysis(input, output, session, values)
  brapps::rts_sv(input, output, session, values)
  brapps::locations(input, output, session, values)

})

shinyApp(ui, sv)









