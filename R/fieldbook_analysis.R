

# repo_ana <- function (areport = "rcbd", traits, geno, rep, data, maxp = 0.1, block = 1, k = 1,
#                           title = paste0("Automatic report for a ",toupper(areport), " design"),
#                           subtitle = NULL, author = "International Potato Center",
#                           format = c("html", "word", "pdf"))
# {
#   format   <- paste(match.arg(format), "_document", sep = "")
#   fmt = "html"
#   if(stringr::str_detect(format, "word")){
#     fmt = "docx"
#   }
#   if(stringr::str_detect(format, "pdf")){
#     fmt = "pdf"
#   }
#   dirfiles <- file.path("www", "reports") #system.file("rmd", package = "pepa")
#   outdir   <- file.path("www", "reports")
#   fileRmd  <- file.path(dirfiles, paste0(areport, ".Rmd"))
#   # fileURL  <- file.path(outdir, paste0(areport, ".html"))
#   # fileDOCX <- file.path(outdir, paste0(areport, ".docx"))
#   # filePDF  <- file.path(outdir, paste0(areport, ".pdf"))
#   out = "No report could be created."
#   if(areport != "a01d" ){
#     out = rmarkdown::render(fileRmd, output_format = format,
#                       output_dir = outdir,
#                       params = list(traits = traits,
#                                     geno = geno, rep = rep, data = data, maxp = maxp, title = title,
#                                     #block = block, k = k,
#                                     subtitle = subtitle, author = author))
#
#   } else {
#     out = rmarkdown::render(fileRmd, output_format = format,
#                       output_dir = outdir,
#                       params = list(traits = traits,
#                                     geno = geno, rep = rep, data = data, maxp = maxp, title = title,
#                                     block = block, k = k,
#                                     subtitle = subtitle, author = author))
#
#   }
#   # if (format == "html_document")
#   #   try(browseURL(fileURL))
#   # if (format == "word_document")
#   #   try(system(paste("open", fileDOCX)))
#   # if (format == "pdf_document")
#   #   try(system(paste("open", filePDF)))
#   #message(out)
#   file.path("reports", paste0(areport, ".", fmt))
# }
#





#' fieldbook_analysis
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param values shiny
# @import rhandsontable
#' @import d3heatmap
#' @import qtlcharts
#' @import agricolae
#' @import rmarkdown
#' @author Reinhard Simon
#' @importFrom shinyFiles shinyFileChoose getVolumes parseFilePaths shinyFilesButton
#' @importFrom magrittr '%>%'
#' @importFrom utils read.csv
#' @importFrom graphics box legend lines mtext par plot points
#' @importFrom stats as.dendrogram density dist dnorm hclust integrate pnorm qnorm uniroot
# @return data.frame
#' @export
fieldbook_analysis <- function(input, output, session, values){
  # crop = isolate(values$crop)
  # amode = isolate(values$amode)

  aFilePath = reactive({!is.null(input$fbaInput) | !is.null(input$filepath)})

  vols <- getVolumes(c("(E:)", "Page File (F:)"))
  shinyFiles::shinyFileChoose(input, 'fb_Input', roots = vols , session = session, filetypes = c('', 'xls', 'xlsx'))


  output$ui_src_type <- shiny::renderUI({
    bdb <- brapi::ba_db()

    ndb <- names(bdb)
    ndb <- ndb[!ndb %in% c("mockbase", "ricebase")]
    ndb <- ndb[stringr::str_detect(ndb, "base")]
    #out <- NULL
    #print(paste("X", is_server()))
    if (!is_server()) {
      #print("chck")
      out <- shiny::tagList(
        radioButtons("fba_src_type", "Select a source type",
                    list(
                      "Database (using BrAPI)" = "Brapi",
                      "Default" = "Default"
                        ,"File" = "Local"
                   ),
                   "Brapi",
                   inline = TRUE)
        ,
        conditionalPanel(
          condition = "input.fba_src_type == 'Local'"
          # ,
          # shinyFiles::shinyFilesButton('fb_Input',
          #                  label = 'File select',
          #                  title = 'Please select a file', multiple = FALSE)
        ),

        conditionalPanel(
          condition = "input.fba_src_type == 'Brapi'",
          shiny::selectInput("baui_bdb", "BrAPI database", ndb)

        )
      )

    } else {
      out <- shiny::selectInput("baui_bdb", "BrAPI database", ndb)
    }
    #out
    #print(class(out))
    #print(str(out))
    return(out)
  })


  output$ui_src_filter <- shiny::renderUI({
    if (!is_server()) {
    out <- tagList(
      conditionalPanel(
        condition = "input.fba_src_type != 'Brapi'",
        radioButtons("fba_src_crop", "Select a crop",
                     get_crops(),
                     inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.fba_src_type == 'Brapi'",
        shiny::checkboxInput("baui_chk_prg", "Use Breeding Programs as filter", value = FALSE),
        shiny::uiOutput("baui_prgs")
      )
    )
    } else {
      out <- tagList(
        shiny::checkboxInput("baui_chk_prg", "Use Breeding Programs as filter", value = FALSE),
        shiny::uiOutput("baui_prgs")
      )
    }
    return(out)
  })

  output$ui_src_fieldbook <- shiny::renderUI({
    shinytoastr::toastr_info("This may take a while to auto-fill.",
                             position = "top-center", progressBar = TRUE, timeOut = 10000)
    if (!is_server()) {
      out <- shiny::tagList(
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
          shiny::uiOutput("baui_stds")
        )

      )
    } else {
      out <- shiny::tagList(
        shiny::uiOutput("baui_stds")
      )
    }
    return(out)
  })


    dataInput <- reactive({
      req(aFilePath)
      req(input$fbaInput)
      fbId = input$fbaInput
      # if(input$fba_src_type == "Local"){
      #   fbIdf = parseFilePaths(vols, input$fb_Input)
      #   #print(fbIdf)
      #   if(nrow(fbIdf) != 0){
      #     fbn = fbIdf$name[1] %>% as.character
      #     fbp = fbIdf$datapath[1] %>% as.character
      #     fbId = fbp[[1]][1]
      #     updateSelectInput(session, "fbaInput", fbId, fbId)
      #   } else  fbId = NULL
      # }
      fbId
    })



    con <- shiny::reactive({
      req(input$baui_bdb)
      brapi::ba_db()[[input$baui_bdb]]
    })

    data_prg <- shiny::reactive({
      shiny::withProgress(message = "Connecting", detail = "Loading programs",{
        brapi::ba_programs(con(), pageSize = 1000)
      })
    })

    data_std <- shiny::reactive({
      #req(input$progrs)
      shiny::withProgress(message = "Connecting", detail = "Loading studies",  {
        std <- brapi::ba_studies_search(con(), pageSize = 10000)
        if (input$baui_chk_prg) {
          std <- std[std$programDbId == input$progrs, ]
        }
        return(std)
      })
    })

    data_fdb <- shiny::reactive({
      req(input$studs)
      shiny::withProgress(message = "Connecting", detail = "Loading fieldbook",  {
        std <- brapi::ba_studies_table(con(), input$studs, rclass = "data.frame")
        return(std)
      })
    })



    output$baui_prgs <- shiny::renderUI({
      shiny::req(input$baui_chk_prg)
      if (input$baui_chk_prg) {
        prg <- as.list(data_prg()$programDbId)
        names(prg) <- data_prg()$name
        shiny::selectInput("progrs", "Breeding programs", choices = prg,
                           selected = prg[1])
      }
    })

    output$baui_stds <- shiny::renderUI({
      std <- as.list(data_std()$studyDbId)
      names(std) <- data_std()$studyName
      #print(std)
      std <- std[!is.na(std)]

      shiny::selectInput("studs", "Breeding studies (fieldbooks)", choices = std,
                         selected = std[1])
    })


    fbInput <- reactive({
    req(input$studs)
    out <- NULL
    #updateSelectInput(session, "fba_set_trt", choices = NULL)

    if (!is_server()) {
      if(input$fba_src_type == "Brapi") {
        out <- data_fdb()
        validate(
          need(is.data.frame(out),
               "Need a table.")
        )
        colnames(out) <- toupper(colnames(out))
      } else {
        out <- get_study(id = dataInput(), amode = input$fba_src_type, crop = input$fba_src_crop)
      }
    } else {
      out <- data_fdb()
      validate(
        need(is.data.frame(out),
             "Need a table.")
      )
      colnames(out) <- toupper(colnames(out))

    }




    out
    })


    # get_traits_with_data <- reactive({
    #   DF = fbInput()
    #   ok = sapply(DF, function(x) sum(is.na(x))) / nrow(DF) < .1
    #   ok = names(DF)[ok]
    #   ok = ok[stringr::str_detect(ok, " ")]
    #   ok
    # })



    fbList <- reactive({
      req(input$fba_src_type)
      sts = NULL
      shiny::withProgress(message = 'Gathering info ...', {
      # if(brapi::can_internet()){
      #   sts = brapi::studies()
      #   sts[sts$studyType != "", ]
      # }
      if(input$fba_src_type == "Default"){
        sts <- get_all_studies(amode = "Default") # to improve using demo as backfall
       # print(sts)
      }

      sts
      })
    })

    # output$fbList <- renderUI({
    #
    observeEvent(input$fba_src_type, {
      #req(input$fba_src_type)
      bd <- NULL
      updateSelectInput(session, "fbaInput", choices = NA, selected = NA)
      output$filepaths <- renderPrint({
        parseFilePaths(vols, input$fb_Input)$datapath[1] %>% as.character()
        })

      #  get_sl_from_brapi <- function(){
      #   sts = fbList()
      #   if(is.null(sts)) return()
      #   sl = as.list(sts$studyDbId)
      #   names(sl) = sts$name
      #   sl
      # }


      # if( input$fba_src_type == "Default"){
      #   withProgress( message = "Getting trials", {
      #     bd = fbglobal::fname_fieldbooks(crop = input$fba_src_crop)
      #     sl = list.files(bd)
      #   })
      # }
      # bd = dataInput()
      # if( input$fba_src_type == "Local"){
        bd = dataInput()
        sl = bd
      # }

      # out = NULL
      # set_fb = NULL
      if(!is.null(bd)) {
        set_fb = updateSelectInput(session, "fbaInput", "Fieldbook", choices = sl)
      }

      #set_fb
    })

    get_cn <- reactive({
      colnames(fbInput()) %>% toupper()
    })

    extract_params <- reactive({
      cn = get_cn()
      gti= which(stringr::str_detect(cn, "CODE|INSTN|GENOTYPE|GEN|GERMPLASMNAME|CIPNUMBER"))[1]
      bki= which(stringr::str_detect(cn, "BLOCK|BLK|BLOC" ))[1]
      rpi= which(stringr::str_detect(cn, "REP|REPL|REPLICATION" ))[1]
      pti= which(stringr::str_detect(cn, "PLOT|PLT" ))[1]
      ci = 1:length(cn)
      fcs = c(gti, bki, rpi, pti)
      tti = ci[!ci %in% fcs]
      fci = max(fcs)
      #tti = max((length(cn) - 3), min(tti)):length(cn)
      # Filter out names in factors!
      # heuristic! Factors are not mixed with the variables, so the maximum index indicates
      # the last factor variable!

      # trait names options
      tni <- (fci + 1):length(cn)
      tno <- cn[tni]

      tn = cn[tti]
      tn <- tn[!fcs %in% cn]
      list(tn = tno, tti = tti, gti = gti,bki = bki, rpi = rpi, pti = pti, ci = ci)
    })


    # gather_params <- reactive({
    #   #req(input$fbaInput)
    #   withProgress(message = "Getting trial info ...", {
    #     #cn = colnames(fbInput()) %>% toupper()
    #     # ep = extract_params(cn)
    #     cn <- get_cn()
    #     ep = extract_params()
    #
    #     shiny::updateSelectInput(session = session, inputId = "fba_set_gen", choices = cn, selected = cn[ep$gti])
    #     shiny::updateSelectInput(session = session, inputId = "fba_set_blk", choices = c(NA, cn), selected = cn[ep$bki])
    #     shiny::updateSelectInput(session = session, inputId = "fba_set_rep", choices = cn, selected = cn[ep$rpi])
    #     shiny::updateSelectInput(session = session, inputId = "fba_set_plt", choices = cn, selected = cn[ep$pti])
    #     shiny::updateSelectInput(session = session, inputId = "fba_set_trt", choices = ep$tn, selected = ep$tn[1], multiple = TRUE)
    #
    #
    #     # out =
    #     #   fluidRow(width = 12,
    #     #            column(width = 3,
    #     #                   selectInput("fba_set_gen", "Genotype", choices = cn, selected = cn[ep$gti]) ,
    #     #                   selectInput("fba_set_blk", "Block", choices = c(NA, cn), cn[ep$bki]),
    #     #                   selectInput("fba_set_plt", "Plot", choices = cn, selected = cn[ep$pti]),
    #     #                   selectInput("fba_set_rep", "Replication", choices = cn, selected = cn[ep$rpi])
    #     #            ),
    #     #            column(width = 9,
    #     #                   selectInput("fba_set_trt", "Traits", choices = ep$tn
    #     #                               , selected = ep$tn[1]
    #     #                               , multiple = TRUE)
    #     #            )
    #     #
    #     #   )
    #   })
    # #out
    # })


    observe({
      #req(input$fbaInput)
      #gather_params()
      #withProgress(message = "Getting trial info ...", {
        #cn = colnames(fbInput()) %>% toupper()
        # ep = extract_params(cn)
        input$studs
        cn <- get_cn()
        ep = extract_params()

        shiny::updateSelectInput(session = session, inputId = "fba_set_gen", choices = cn, selected = cn[ep$gti])
        shiny::updateSelectInput(session = session, inputId = "fba_set_blk", choices = c(NA, cn), selected = cn[ep$bki])
        shiny::updateSelectInput(session = session, inputId = "fba_set_rep", choices = cn, selected = cn[ep$rpi])
        shiny::updateSelectInput(session = session, inputId = "fba_set_plt", choices = cn, selected = cn[ep$pti])
        shiny::updateSelectInput(session = session, inputId = "fba_set_trt", choices = ep$tn, selected = ep$tn[1])


      #})
      })


    # output$fbParams <- renderUI({
    #   req(input$fba_src_type)
    #   if( !is_server()) {
    #
    #     if( input$fba_src_type == "Local") {
    #       req(input$fbaInput)
    #       if(!stringr::str_detect(input$fbaInput, ".rda")) return(gather_params())
    #     }
    #   }
    #   gather_params()
    # })



  output$hotFieldbook <- DT::renderDataTable({

    validate(
      need(is.data.frame(fbInput()),
           "Need a table as data source!")
    )

    withProgress(message = "Loading table ...", {
      fbInput()
    })
  }
  ,  server = TRUE,  extensions = 'FixedColumns',
  options = list(scrollX = TRUE
                 # ,
                 # fixedColumns = list(leftColumns = 6)
  )
  #,
  # selection = list(target = 'column', mode = "single")
  )

  validate_table <- function(DF) {
    validate(
      need(
        is.data.frame(DF), "The data source needs to be a table."
      )
    )

    validate(
      need(
        nrow(DF) > 9, "The data source needs to be a table with more than 9 rows."
      )
    )
  }



  phCorr <- function(DF, trait, useMode = "dendo", maxGermplasm = 9999, filterTrait = NULL){
    req(input$fba_set_trt)
    req(input$fba_set_gen)
    # if (length(dim(DF)) < 2) return(NULL)
    # if (!is.data.frame(DF)) return(NULL)
    # if (any(is.na(DF[, 1]))) return(NULL)
    validate_table(DF)

    treat <- input$fba_set_gen
    if(length(trait) < 2) return(NULL)
    if(!all(trait %in% names(DF))) return(NULL)

    DF = DF[, c(treat, trait)]

    out <- NULL
    #print(str(DF))
    # print(class(DF))
    # print(head(DF))
    # print(DF[, 1])
    try({

    if(any(is.na(DF[, input$fba_set_gen]))) return(NULL)

    na_count <- sapply(DF, function(y) sum(length(which(is.na(y)))))
    too_many_na <- na_count / nrow(DF) > 0.1
    # #print(too_many_na)
    #
    stm <- sum(too_many_na)
    if (stm > 1) {
      shiny::showNotification(paste(stm,
                      "traits/variables are excluded since they have more than 10% missing data."))
      DF <- DF[, !too_many_na]
    }

    #print(names(DF))
    if(any(is.na(DF[, 1]))) return(NULL)
    for(i in 2:ncol(DF)){
      #try({
      if (all(is.na(DF[, i]))) return(NULL)
      DF[, i] = DF[, i] %>% as.character() %>% as.numeric()

      #})

    }

      options(warn = -1)
      # exclude the response variable and empty variable for RF imputation
      #datas <- names(DF)[!names(DF) %in% c(treat)] # TODO replace "PED1" by a search
      datas <- names(DF)[2:ncol(DF)]
      x <- DF[, datas]
      #print(head(x))
      # for(i in 1:ncol(x)){
      #   x[, i] <- as.numeric(x[, i])
      # }
      y <- DF[, treat] %>% as.factor
      DF <- cbind(y, x)
      names(DF)[1] <- treat


      #print(head(DF))
      # print(y)
      if (any(is.na(x)) & !any(is.na(y))) {
        #frm <- paste0(treat, " ~ ", paste(names(DF[, 2:ncol(DF)]), collapse = "+"))
        frm <- paste0(treat, " ~ .")
        # str(DF)
        # print(frm)
        # print(head(DF))
        # print(summary(DF))

          utils::capture.output(
            DF <- randomForest::rfImpute(stats::as.formula(frm), DF #, iter = 3, ntree = 50
                                         )
          )
      }


      names(DF)[1] <- treat
      DF = agricolae::tapply.stat(DF, DF[, treat])
      DF = DF[, -c(2)]
      names(DF)[1] = treat #"germplasmName"
      row.names(DF) = DF[, treat]
      DF = DF[, -c(1)]
      options(warn = 0)

    #})
    out <- DF
      })
      out
  }
# end phCorr


  output$fieldbook_heatmap_ui <- renderUI({
    d3heatmapOutput("fieldbook_heatmap")
  })



  output$fieldbook_heatmap <- d3heatmap::renderD3heatmap({
    req(input$fba_set_trt)
    out = NULL
    withProgress(message = "Creating spatial map ...", {
    fm_DF = fbInput()

    validate_table(fm_DF)

    REP = "REP"
    try({
      REP = input$fba_set_rep

    })
    reps <- fm_DF[, REP]

    validate(
      need( (length(unique(reps)) > 1),
            "Only experiments with more than 1 replication are currently supported.")
    )


    # print("table reps")
    # print(table(reps))
    validate(
      need((length(unique(table(reps))) == 1),
           "Only experiments with equal number of repetitions are currently supported.")
    )

    # print(max(reps))
    # print( length(unique(fm_DF[, input$fba_set_gen])))
    # print(nrow(fm_DF))

    is_multiple <- function() {
      n_rep <- max(reps)
      n_gen <- length(unique(fm_DF[, input$fba_set_gen]))

      n_row <- nrow(fm_DF)
      n_row %% (n_gen * n_rep) == 0
    }

    validate(
      need(is_multiple(),
           "Number of rows must be a multiple of the number of genotypes.")
    )

    validate(
      need((max(table(reps)[2]) < 201),
           "Only experiments with up to 200 distinct genotypes are currently supported.")
    )

    trt = input$fba_set_trt[1]
    if(is.null(trt)) return(NULL)

    if(is.null(fm_DF)) return(NULL)
    cn = colnames(fm_DF)
    if(!(trt %in% cn)) return(NULL)

    fm <- fbmaterials::fb_to_map(fm_DF,
                                 gt = input$fba_set_gen,
                                 rep = REP,
                                 plt = input$fba_set_plt,
                                 variable = trt
    )
    amap = fm[["map"]]
    anot = fm[["notes"]]
    out = d3heatmap(x = amap,
              cellnote = anot,
              theme = "dark", colors = "Blues",
              Rowv = FALSE, Colv = FALSE,
              dendrogram = "none")
    }) # progrss
    out
  })

  has_more_traits <- reactive({
    req(input$fba_set_trt)
    trt = input$fba_set_trt
    req(length(trt) > 1)
    trt
  })

  get_ph_corr <- reactive({
    req(input$fba_set_trt)
    req(input$fba_set_gen)
    DF <- fbInput()
    trt = has_more_traits()
    # print("====")
    # print(trt)
    shiny::withProgress(message = 'Imputing missing values', {
      out = phCorr(DF, trt, useMode = "dendo")
    })
    out
  })


  output$vcor_output = qtlcharts::iplotCorr_render({
    req(input$fba_set_trt)
    req(input$fba_set_gen)
    validate(
      need(!any(input$fba_set_trt %in% input$fba_set_rep) ,
        "Traits must not be one of the factors."
      )
    )
    shiny::withProgress(message = 'Creating graph ...', {
      iplotCorr(get_ph_corr())
    })
  })

  output$phHeat_output_ui <- renderUI({
    req(input$fba_set_trt)
    d3heatmap::d3heatmapOutput("phHeat_output", height = 1400)
  })


output$phHeat_output = d3heatmap::renderD3heatmap({
  req(input$fba_set_trt)
  DF = get_ph_corr()
  par(mar=c(3,1,1,10))
  d3heatmap::d3heatmap(DF, theme = "dark", colors = "Blues")
})

  output$phDend_output = renderPlot({
    req(input$fba_set_trt)
    DF = get_ph_corr()
    dend <- DF %>% dist %>% hclust %>% as.dendrogram()

    par(mar=c(3,1,1,10))
    plot(dend, horiz = TRUE)
  })


output$phNet_output = shiny::renderPlot({
    req(input$fba_set_trt)
    validate(
      need(length(input$fba_set_trt) > 3, "Need at least four traits.")
    )
    DF = get_ph_corr()

    par(mar=c(3,1,1,10))
    DF <- corrr::correlate(DF)
    corrr::network_plot(DF, legend = TRUE, min_cor = 0.2, colors = c("skyblue1", "white", "indianred2"))
})

output$phDens_output = renderPlot({
  #req(input$fba_set_trt)
  #req(input$phDens)
  req(input$fba_set_trt)
  req(input$fba_set_rep)

  #par(mar=c(3,1,1,10))
  DF <- fbInput()
  validate_table(DF)

  REP =  input$fba_set_rep
  validate(
    need((!all(is.na(DF[, c(REP)]))),
         "Need a column indicating the replication."
         )
  )



  if(!(REP %in% names(DF))) return(NULL)
  if(any(is.null(DF[, REP]))) return(NULL)

  titl = input$fba_set_trt[1]

  cn = colnames(DF)
  if(!(titl %in% cn)) return(NULL)
  #titl = input$phDens

  # if (all(is.na(DF[, c(REP)]))) {
  #   DF <- DF[, c(titl)]
  #   DF[, 1] <- as.numeric(DF[, 1])
  #   n = max(DF[, REP])
  #   cls = c("black", "blue", "red", "orange", "darkgreen", "grey60")
  #
  #   dens <- density(DF[DF[, 1], 1], na.rm = TRUE)
  #
  # } else {
    DF <- DF[, c(REP, titl)]
    DF[, 2] <- as.numeric(DF[, 2])
    n = max(DF[, REP])
    cls = c("black", "blue", "red", "orange", "darkgreen", "grey60")

    dens <- density(DF[DF[, REP], 2], na.rm = TRUE)

  #}



  dds = list(n+1)
  dds[[1]] = dens$y / max(dens$y)

  if(n > 1 & n < 6){#Assumption no more than 5 repetitions
    for(i in 1:n){
      #print(DF[DF$REP == i, 2])
      dds[[i + 1]] <- density(DF[DF[, REP] == i, 2] , na.rm = TRUE)$y
      dds[[i + 1]] <- dds[[i + 1]] / max(dds[[i + 1]])
      dds[[1]] = dds[[1]] + dds[[i + 1]]
    }
  }
  dds[[1]] <- dds[[1]] / max(dds[[1]])


  plot(dds[[1]], main = titl, ylim = c(0, 1), type = "l")
  #plot( main = titl, ylim = c(0, 1), type = "l")
  if(n > 1 & n < 6){#Assumption no more than 5 repetitions
    for(i in 1:n){
      #print(DF[DF$REP == i, 2])
      #dens <- density(DF[DF[, REP] == i, 2] , na.rm = TRUE)
      #lines(dens, col = cls[i+1])
      lines(dds[[i+1]], col = cls[i+1])
    }
    legend("topright", legend = c("overall", 1:n), title = "Repetition", lty = 1, col = cls[1:(n+1)])
  }
  #abline(v = mean(DF[, 2], na.rm = TRUE), lwd = 2, col = "grey30")
  #rug(DF[, 2], quiet = TRUE)

})





#####################

# observeEvent(fbInput(),
#              gather_params()
#              )
#
#
# observeEvent(input$fbRepoDo, {
#   output$fbRep <- shiny::renderUI({
#     req(input$fba_set_trt)
#     #print("step 1")
#     #print("Hi")
#     DF <- fbInput()
#     trait = input$fba_set_trt
#
#     treat <- input$fba_set_gen #"germplasmName" #input$def_genotype
#     #trait = input$fbCorrVars
#     if(length(trait) < 1) return(NULL)
#
#     shiny::withProgress(message = 'Imputing missing values', {
#       options(warn = -1)
#       REP = input$fba_set_rep
#       GEN = input$fba_set_gen
#
#       DF = DF[, c(treat, REP,  trait)]
#
#       DF[, treat] <- as.factor(DF[, treat])
#
#       # exclude the response variable and empty variable for RF imputation
#       datas <- names(DF)[!names(DF) %in% c(treat, "PED1")] # TODO replace "PED1" by a search
#       x <- DF[, datas]
#       for(i in 1:ncol(x)){
#         x[, i] <- as.numeric(x[, i])
#       }
#       y <- DF[, treat]
#       if (any(is.na(x))){
#         utils::capture.output(
#           DF <- randomForest::rfImpute(x = x, y = y )
#         )
#       }
#       names(DF)[1] <- treat
#     })
#     out = "no report"
#     if(input$expType == "RCBD"){
#       #pepa::repo.rcbd(trait, geno = "germplasmName", rep = "REP", data = DF, format = tolower(input$aovFormat))
#       out = repo_ana("rcbd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
#     }
#     if(input$expType == "CRD"){
#       #pepa::repo.crd(trait, geno = "germplasmName",  data = DF, format = tolower(input$aovFormat))
#       out = repo_ana("crd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
#     }
#     if(input$expType == "ABD"){
#       #pepa::repo.abd(trait, geno = "germplasmName", rep = "REP", data = DF, format = tolower(input$aovFormat))
#       out = repo_ana("abd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
#     }
#     if(input$expType == "A01D"){
#       # pepa::repo.a01d(trait, geno = "germplasmName", rep = "REP", block = input$block, k = input$k,
#       #                  data = DF, format = tolower(input$aovFormat))
#       out = repo_ana("a01d", trait, geno = GEN, rep = REP, block = input$fba_block, k = input$k,
#                data = DF, format = tolower(input$aovFormat))
#
#     }
#   HTML("<a href='", out, "' target='_new'>Report</a>")
#
#   })


#})

#report_type <- "report.Rmd"
report_path <- system.file(file.path("rmd"), package = "pepa")

report_fn <- reactive({
  paste0('report_', unique(fbInput()$STUDYNAME), "_", input$expType, "_", Sys.time(), "." , switch(
    input$aovFormat, PDF = 'pdf', HTML = 'html', Word = 'docx'))
  })

output$fbRepo <- downloadHandler(
  filename = function() {
    # fn <- paste0('report_', unique(fbInput()$STUDYNAME), "_", input$expType, "_", Sys.time(), "." , switch(
    #   input$aovFormat, PDF = 'pdf', HTML = 'html', Word = 'docx'
    # ))
    report_fn()
  },

  content = function(file) {
  withProgress(message = "Report creation in progress", detail = "This may take a while ...", value = 0, {

  tryCatch({
    report_name <- switch(
      input$expType, RCBD = "rcbd.Rmd", ABD = "abd.Rmd", CRD = "crd", A01D = "a01d.Rmd"
    )

    src <- list.files(report_path, full.names = TRUE)

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    #unlink(report_name)
    file.copy(src, basename(src), overwrite = TRUE)

    k = ifelse(!is.null(input$fba_src_k), input$fba_src_k, 0)

    df <- fbInput()[, c(input$fba_set_gen, input$fba_set_rep, input$fba_set_trt)]
    names(df) <- toupper(names(df))

    dots <- lapply(c(input$fba_set_gen, input$fba_set_rep), as.symbol)
    df <- df %>% dplyr::group_by_(.dots = dots) %>% dplyr::summarise_each(funs(mean)) %>% as.data.frame()

    ttl <- paste0("Automated report for study: ", unique(fbInput()$STUDYNAME), " - ",
                switch(input$expType,
                       RCBD = "Randomized Complete Block Design (RCBD)",
                       ABD = "Augmented Block Design (ABD)",
                       CRD = "Completely Randomized Design (CRD)",
                       A01D = "Alpha Design (A01D)")
    )
    #print(ttl)

    fba_params = switch(input$expType,
      RCBD = list(
                  traits = input$fba_set_trt,
                  geno = input$fba_set_gen,
                  rep = input$fba_set_rep,
                  data = df,
                  maxp = 0.10,
                  title = ttl,
                  subtitle = NULL,
                  author = "International Potato Center (CIP)"),
      ABD = list(
        traits = input$fba_set_trt,
        geno = input$fba_set_gen,
        rep = input$fba_set_rep,
        data = df,

        title = ttl,
        subtitle = NULL,
        author = "International Potato Center (CIP)"),
      CRD = list(
        traits = input$fba_set_trt,
        geno = input$fba_set_gen,
        data = df,
        maxp = 0.10,
        title = ttl,
        subtitle = NULL,
        author = "International Potato Center (CIP)"),
      A01D = list(
        traits = input$fba_set_trt,
        geno = input$fba_set_gen,
        rep = input$fba_set_rep,
        block = input$fba_set_blk,
        k = k,
        data = df,
        maxp = 0.10,
        title = ttl,
        subtitle = NULL,
        author = "International Potato Center (CIP)")
    )

    # print(fba_params)
    #
    # print(report_name)

    out <- render(report_name,
                  switch(
                    input$aovFormat,
                    PDF = pdf_document(), HTML = html_document(), Word = word_document()
                  ),
                  params = fba_params,
                  envir = new.env(parent = globalenv())
    )


    # print(out)
    # print(file)

    res <- file.rename(out, file)
    #print(res)
    res
  })
  }) # progress
  }
)


output$rep_frmt <- shiny::renderUI({
req(input$fba_set_trt)
validate(
  need(length(input$fba_set_trt) > 0,  "Need at least one seleceted trait for analysis")
)


out <- tagList(

  HTML("Revise your selection on the Data source tab."),

  radioButtons("aovFormat","Report format",
               c('PDF', 'HTML', 'Word'),
               inline = TRUE),
  radioButtons("expType", "Experiment type",
               c("RCBD", "ABD", "CRD", "A01D"

               ), inline = TRUE),


  conditionalPanel(
    condition = "input.expType == 'A01D'",

    shiny::numericInput('fba_src_k', 'Select Block Size',   value = 2, min = 2, max = 100)
  ),

  downloadButton("fbRepo", "Download Report!")
)
  return(out)

})


}









