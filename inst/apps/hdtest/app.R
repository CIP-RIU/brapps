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

brapi_host = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu"

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
                                 ,
                                 uiOutput("fbList")
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
                        ),
                        tabItem(tabName = "phe_dashboard",
                                fluidRow(
                                  column(width = 12,
                                         box(width = NULL,
                                             title = "Fieldbook",
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
                                                         #tags$img(src = "www/35.gif"),
                                                         div(id = "plot-container",
                                                             #tags$img(src = "www/35.gif"),

                                                             qtlcharts::iplotCorr_output('vcor_output', height = 400)
                                                         )
                                                ),
                                                tabPanel("Map",
                                                         d3heatmap::d3heatmapOutput("fieldbook_heatmap")
                                                ),
                                                tabPanel(title = "Report",
                                                    tabBox(id = "tabAnalaysisReports", width = NULL,
                                                      tabPanel("HTML report",
                                                               htmlOutput("fbRepHtml")
                                                               )
                                                      ,
                                                      tabPanel("Word report",
                                                               htmlOutput("fbRepWord")
                                                      ),
                                                      # tabPanel("PDF report",
                                                      #          htmlOutput("fbRepPdf")
                                                      # ),
                                                      HTML("<div style='display:none'>"),
                                                      shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                                                      #shinyURL.ui("URL", tinyURL = F)
                                                      HTML("</div>")

                                                      )


                                                )



                                         )
                                  )

                                )
                        )

                      )        )
)

#
#
# fieldbook_analysis <- function(input, output, session){
#
#   dataInput <- reactive({
#     fbId = input$fbaInput
#     fbId
#   })
#
#   fbInput <- reactive({
#     fbId = dataInput()
#     #print(fbId)
#     brapi::study_table(fbId)
#   })
#
#   fbList <- reactive({
#     shiny::withProgress(message = 'Gathering info ...', {
#     sts = brapi::studies()
#     sts[sts$studyType != "", ]
#     })
#   })
#
#   output$fbList <- renderUI({
#     sts = fbList()
#     if(is.null(sts)) return()
#     sl = as.list(sts$studyDbId)
#     names(sl) = sts$name
#     selectInput("fbaInput", "Fieldbook", choices = sl)
#   })
#
#   observe({
#   output$hotFieldbook <- DT::renderDataTable({
#     #print(input$fbaInput)
#     req(input$fbaInput)
#     x = NULL
#     withProgress(message = "Loading fieldbook ...",
#                  detail = "This may take a while ...", value = 1, max = 4, {
#     try({
#       x <- brapi::study_table(input$fbaInput)  #fbInput()
#
#     })
#     })
#     x
#    },  server = FALSE,  #extensions = 'FixedColumns',
#
#       selection = list(mode = 'single', target = 'column'),
#       options = list(scrollX = TRUE ))
#
#
#   })
#
#   output$vcor_output = qtlcharts::iplotCorr_render({
#
#     DF <- fbInput()
#     shiny::withProgress(message = 'Imputing missing values', {
#       options(warn = -1)
#
#
#       treat <- "germplasmName" #input$def_genotype
#       trait <- names(DF)[c(7:ncol(DF))]  #input$def_variables
#       DF = DF[, c(treat, trait)]
#
#       DF[, treat] <- as.factor(DF[, treat])
#
#       # exclude the response variable and empty variable for RF imputation
#       datas <- names(DF)[!names(DF) %in% c(treat, "PED1")] # TODO replace "PED1" by a search
#       x <- DF[, datas]
#
#       for(i in 1:ncol(x)){
#         x[, i] <- as.numeric(x[, i])
#       }
#       y <- DF[, treat]
#       if (any(is.na(x))){
#         capture.output(
#           DF <- randomForest::rfImpute(x = x, y = y )
#         )
#         #data <- cbind(y, data)
#
#       }
#       names(DF)[1] <- treat
#
#       DF = agricolae::tapply.stat(DF, DF[, treat])
#       DF = DF[, -c(2)]
#       names(DF)[1] = "Genotype"
#       row.names(DF) = DF$Genotype
#       DF = DF[, -c(1)]
#
#       # iplotCorr(DF,  reorder=TRUE,
#       #           chartOpts=list(cortitle="Correlation matrix",
#       #                          scattitle="Scatterplot"))
#
#       options(warn = 0)
#
#     })
#     iplotCorr(DF)
#   })
#
#
#   # TODO BUG?: somehow this section needs to go last!
#   output$fieldbook_heatmap <- d3heatmap::renderD3heatmap({
#     if(is.null(input$fbaInput)) return(NULL)
#     DF = fbInput()
#     #print(str(DF))
#     if(is.null(DF)) return(NULL)
#     #if (!is.null(DF)) {
#     #ci = input$hotFieldbook_select$select$c
#     ci = input$hotFieldbook_columns_selected
#     #print(ci)
#     trt = names(DF)[ncol(DF)]
#     if (!is.null(ci)) trt = names(DF)[ci]
#     #print(trt)
#     fm <- fbmaterials::fb_to_map(DF, gt = "germplasmName", #input[["def_genotype"]],
#                                  variable = trt,
#                                  rep = "REP", #input[["def_rep"]],
#                                  # blk = input[["def_block"]],
#                                  plt = "PLOT"  #input[["def_plot"]]
#     )
#     amap = fm[["map"]]
#     anot = fm[["notes"]]
#     d3heatmap(x = amap,
#               cellnote = anot,
#               colors = "Blues",
#               Rowv = FALSE, Colv = FALSE,
#               dendrogram = "none")
#   })
#
#
#   #####################
#
#   #observeEvent(input$butDoPhAnalysis, ({
#
#   do_report <- function(fmt = "html_document"){
#     DF <- fbInput()
#     yn = names(DF)[c(7:ncol(DF))]
#     report = paste0("reports/report_anova.Rmd")
#
#     usr = Sys.getenv("USERNAME")
#     if (usr=="") usr = Sys.getenv("USER")
#     author =  paste0(usr, " using HIDAP")
#
#     rps = "REP" # input$def_rep
#     gtp = "germplasmName" #input$def_genotype
#     xmt = list(title = attr(DF, "meta")$studyName, contact = "x y", site = attr(DF, "meta")$locationName, country = "Z", year = 2016 )
#
#     withProgress(message = "Creating reports ...",
#                  detail = "This may take a while ...", value = 1, max = 4, {
#                    try({
#                      incProgress(1, message = fmt)
#                      fn = rmarkdown::render(report,
#                                             output_format = fmt,
#                                             run_pandoc = TRUE,
#                                             output_dir = "www/reports",
#                                             params = list(
#                                               meta = xmt,
#                                               trait = yn,
#                                               treat = gtp,
#                                               rep  = rps,
#                                               data = DF,
#                                               maxp = 0.1,
#                                               author = author,
#                                               host = host))
#                      incProgress(1, message = "Loading")
#
#                    }) # try
#
#   })
#     fn
#   }
#
#
#   output$fbRepHtml <- renderUI({
#     out = "Report created but cannot be read."
#     fn = do_report()
#     try({
#       out <- readLines(fn)
#     })
#     HTML(out)
#
#   })
#
#   output$fbRepPdf <- renderUI({
#     out = "Report created but cannot be read."
#     fn = do_report("pdf_document")
#     print(fn)
#     message(fn)
#     try({
#       #out <- paste0("<iframe src='http://localhost/reports/report_anova.pdf&embedded=true' style='width:718px; height:700px;' frameborder='0'></iframe>")
#       out <- paste0("<a href='reports/report_anova.pdf' target='_new'>PDF</a>")
#
#       #out <- paste0("<a href='", fn, "'>PDF</a>")
#     })
#     HTML(out)
#
#   })
#
#   output$fbRepWord <- renderUI({
#     out = "Report created but cannot be read."
#     fn = do_report("word_document")
#     print(fn)
#     message(fn)
#     try({
#       #out <- paste0("<iframe src='http://localhost/reports/report_anova.pdf&embedded=true' style='width:718px; height:700px;' frameborder='0'></iframe>")
#       out <- paste0("<a href='reports/report_anova.docx' target='_new'>Word</a>")
#
#       #out <- paste0("<a href='", fn, "'>PDF</a>")
#     })
#     HTML(out)
#
#   })
#
#
# }



############################################################

sv <- function(input, output, session) ({
  brapi_con("sweetpotato", "http://sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
            80, "rsimon16",
            "sweetpotato")

  shinyURL.server()

  brapps::fieldbook_analysis(input, output, session)

  brapps::locations(input, output, session)

  # observe({
  #   # Re-execute this reactive expression after 1000 milliseconds
  #   invalidateLater(1000, session)
  #   unlink(get_base_data(atype = "location"), recursive = TRUE)
  # })

})

shinyApp(ui, sv)









