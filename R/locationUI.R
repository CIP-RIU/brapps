
#' locationsUI
#'
#' @param id shiny ID
#' @import shiny
#' @author Reinhard Simon
#' @return list
#' @export
locationsUI <- function(id){
  ns <- NS(id)

  tagList(
    #h2("Trial Location Explorer"),
    DT::dataTableOutput(ns("table"))
  )

}
