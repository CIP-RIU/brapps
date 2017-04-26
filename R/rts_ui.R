

rts1_ui <- function(){
  shiny::fluidPage(

    # Input values
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("N",
                            "Plot capacity:",
                            min = 100,
                            max = 10000,
                            step = 100,
                            value = 1000),
        shiny::numericInput("sg1",
                            "Number of selected genotypes:",
                            min = 0,
                            max = 10000,
                            step = 10,
                            value = 10),
        shiny::numericInput("sigmaG2",
                            "Genotypic variance:",
                            min = 0,
                            max = 1000,
                            step = 0.1,
                            value = 1),
        shiny::numericInput("sigmaE2",
                            "Error variance:",
                            min = 0,
                            max = 1000,
                            step = 0.1,
                            value = 1)
      ),

      # Show the plot

      shiny::mainPanel(
        shiny::plotOutput("rtsplot1")
      )
    )
  )
}

rts2_ui <- function(){
  shiny::fluidPage(

    # Application title
    #shiny::titlePanel("Response to selection with several locations"),

    # Input values
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("N",
                            "Plot capacity:",
                            min = 100,
                            max = 10000,
                            step = 100,
                            value = 1000),
        shiny::numericInput("k",
                            "Number of locations:",
                            min = 1,
                            max = 100,
                            step = 1,
                            value = 3),
        shiny::numericInput("sg1",
                            "Number of selected genotypes:",
                            min = 0,
                            max = 10000,
                            step = 10,
                            value = 10),
        shiny::numericInput("sigmaG2",
                            "Genotypic variance:",
                            min = 0,
                            max = 1000,
                            step = 0.1,
                            value = 1),
        shiny::numericInput("sigmaGL2",
                            "Genotypic x location variance:",
                            min = 0,
                            max = 1000,
                            step = 0.1,
                            value = 1),
        shiny::numericInput("sigmaE2",
                            "Error variance:",
                            min = 0,
                            max = 1000,
                            step = 0.1,
                            value = 1)
      ),

      # Show the plot

      shiny::mainPanel(
        shiny::plotOutput("rtsplot2")
      )
    )
  )
}


rts3_ui <- function(){
  # Application title
  #shiny::titlePanel("Response to selection with several locations and years"),

  # Input values
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput("N",
                          "Plot capacity:",
                          min = 100,
                          max = 10000,
                          step = 100,
                          value = 1000),
      shiny::numericInput("k",
                          "Number of locations:",
                          min = 1,
                          max = 100,
                          step = 1,
                          value = 3),
      shiny::numericInput("q",
                          "Number of years:",
                          min = 1,
                          max = 100,
                          step = 1,
                          value = 3),
      shiny::numericInput("sg1",
                          "Number of selected genotypes:",
                          min = 0,
                          max = 10000,
                          step = 10,
                          value = 10),
      shiny::numericInput("sigmaG2",
                          "Genotypic variance:",
                          min = 0,
                          max = 1000,
                          step = 0.1,
                          value = 1),
      shiny::numericInput("sigmaGL2",
                          "Genotypic x location variance:",
                          min = 0,
                          max = 1000,
                          step = 0.1,
                          value = 1),
      shiny::numericInput("sigmaGY2",
                          "Genotypic x year variance:",
                          min = 0,
                          max = 1000,
                          step = 0.1,
                          value = 1),
      shiny::numericInput("sigmaGLY2",
                          "Genotypic x location x year variance:",
                          min = 0,
                          max = 1000,
                          step = 0.1,
                          value = 1),
      shiny::numericInput("sigmaE2",
                          "Error variance:",
                          min = 0,
                          max = 1000,
                          step = 0.1,
                          value = 1)
    ),

    # Show the plot

    shiny::mainPanel(
      shiny::plotOutput("rtsplot3")
    )
  )
  }


rts4_ui <- function(){
  # Application title
  #shiny::titlePanel("Response to selection with several locations in two steps"),

  # Input values
  shiny::fluidRow(
    shiny::column(4,
                  shiny::wellPanel(
                    shiny::h3("Number of genotypes, locations and replications at each step:"),
                    shiny::numericInput("g", "Number of genotypes at begining:", 1000),
                    shiny::numericInput("k1", "Number of locations at step 1:", 2),
                    shiny::numericInput("r1", "Number of replications at step 1:", 2),
                    shiny::numericInput("sg1", "Selected genotypes at step 1:", 100),
                    shiny::numericInput("k2", "Number of locations at step 2:", 5),
                    shiny::numericInput("r2", "Number of replications at step 2:", 3),
                    shiny::numericInput("sg2", "Selected genotypes at step 2:", 10)
                  )),
    shiny::column(3,
                  shiny::wellPanel(
                    shiny::h3("Estimated variances:"),
                    shiny::numericInput("sigmaG2", "G variance:", 1),
                    shiny::numericInput("sigmaGL2", "GxL variance:", 1),
                    shiny::numericInput("sigmaGY2", "GxY variance:", 1),
                    shiny::numericInput("sigmaGLY2", "GxLxY variance:", 1),
                    shiny::numericInput("sigmaE2", "Error variance:", 1)
                  )),
    shiny::column(3,
                  shiny::wellPanel(
                    shiny::h3("Response to selection:"),
                    # Show the plot
                    shiny::tableOutput("rtstable4")
                  )
    )
  )

}


#' rts1_ui
#'
#' @param id a name
#' @return shiny object
#'
#' @export
rts_ui <- function(id = "rts1"){
  shinydashboard::tabItem(tabName = id,
  h2("Response to selection"),
  shinydashboard::tabBox(id = id,width = NULL,

         shiny::tabPanel("for a single experiment",
                          rts1_ui()
         ),
         shiny::tabPanel("with several locations",
                         rts2_ui()
         ),
         shiny::tabPanel("with several locations and years",
                         rts3_ui()
         )
         # ),
         # shiny::tabPanel("Response to selection with several locations in two steps",
         #                 rts4_ui()
         # )
  )
)

}
