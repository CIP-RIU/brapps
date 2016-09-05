
repo_ana <- function (areport = "rcbd", traits, geno, rep, data, maxp = 0.1, block = 1, k = 1,
                          title = paste0("Automatic report for a ",toupper(areport), " design"),
                          subtitle = NULL, author = "International Potato Center",
                          format = c("html", "word", "pdf"))
{
  format   <- paste(match.arg(format), "_document", sep = "")
  fmt = "html"
  if(stringr::str_detect(format, "word")){
    fmt = "docx"
  }
  if(stringr::str_detect(format, "pdf")){
    fmt = "pdf"
  }
  dirfiles <- file.path("www", "reports") #system.file("rmd", package = "pepa")
  outdir   <- file.path("www", "reports")
  fileRmd  <- file.path(dirfiles, paste0(areport, ".Rmd"))
  # fileURL  <- file.path(outdir, paste0(areport, ".html"))
  # fileDOCX <- file.path(outdir, paste0(areport, ".docx"))
  # filePDF  <- file.path(outdir, paste0(areport, ".pdf"))
  out = "No report could be created."
  if(areport != "a01d" ){
    out = rmarkdown::render(fileRmd, output_format = format,
                      output_dir = outdir,
                      params = list(traits = traits,
                                    geno = geno, rep = rep, data = data, maxp = maxp, title = title,
                                    #block = block, k = k,
                                    subtitle = subtitle, author = author))

  } else {
    out = rmarkdown::render(fileRmd, output_format = format,
                      output_dir = outdir,
                      params = list(traits = traits,
                                    geno = geno, rep = rep, data = data, maxp = maxp, title = title,
                                    block = block, k = k,
                                    subtitle = subtitle, author = author))

  }
  # if (format == "html_document")
  #   try(browseURL(fileURL))
  # if (format == "word_document")
  #   try(system(paste("open", fileDOCX)))
  # if (format == "pdf_document")
  #   try(system(paste("open", filePDF)))
  #message(out)
  file.path("reports", paste0(areport, ".", fmt))
}



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
#' @author Reinhard Simon
#' @importFrom shinyFiles shinyFileChoose getVolumes parseFilePaths shinyFilesButton
#' @importFrom magrittr '%>%'
#' @importFrom utils read.csv
# @return data.frame
#' @export
fieldbook_analysis <- function(input, output, session, values){
  crop = isolate(values$crop)
  amode = isolate(values$amode)

  aFilePath = reactive({!is.null(input$fbaInput) | !is.null(input$filepath)})

  vols <- getVolumes(c("(E:)", "Page File (F:)"))
  shinyFileChoose(input, 'fb_Input', roots = vols , session = session, filetypes=c('', 'xls', 'xlsx'))

  brapi_host = brapi$db

    # observeEvent(input$fba_src_type,{
    #   print("Change of type!")
    #   print(input$fba_src_type)
    #   print(input$fbaInput)
    #   updateSelectInput(session, "fbaInput", choices = NULL, selected = NULL)
    #   print(input$fbaInput)
    # }
    # )

    dataInput <- reactive({
      req(aFilePath)
      fbId = input$fbaInput
      if(input$fba_src_type == "Local"){
        fbIdf = parseFilePaths(vols, input$fb_Input)
        #print(fbIdf)
        if(nrow(fbIdf) != 0){
          fbn = fbIdf$name[1] %>% as.character
          fbp = fbIdf$datapath[1] %>% as.character
          fbId = fbp[[1]][1]
          updateSelectInput(session, "fbaInput", fbId, fbId)
        } else  fbId = NULL
      }
      fbId
    })

    fbInput <- reactive({
      #req(input$fbaInput)
      req(aFilePath())
      #get_study(id = input$fbaInput, amode = input$fba_src_type, crop = input$fba_src_crop)
      get_study(id = dataInput(), amode = input$fba_src_type, crop = input$fba_src_crop)
    })

    fbList <- reactive({
      req(input$fba_src_type)
      sts = NULL
      shiny::withProgress(message = 'Gathering info ...', {
      if(brapi::can_internet()){
        sts = brapi::studies()
        sts[sts$studyType != "", ]
      }
      if(input$fba_src_tye == "Demo"){
        sts <- get_all_studies() # to improve using demo as backfall

      }

      sts
      })
    })

    # output$fbList <- renderUI({
    #
    observeEvent(input$fba_src_type, {
      #req(input$fba_src_type)
      updateSelectInput(session, "fbaInput", choices = NA, selected = NA)
      output$filepaths <- renderPrint({
        parseFilePaths(vols, input$fb_Input)$datapath[1] %>% as.character()
        })

       get_sl_from_brapi <- function(){
        sts = fbList()
        if(is.null(sts)) return()
        sl = as.list(sts$studyDbId)
        names(sl) = sts$name
        sl
      }
      sl = NULL
      if( input$fba_src_type == "brapi"){
        sl = get_sl_from_brapi()
      }


      if( input$fba_src_type == "Default"){
        withProgress( message = "Getting trials", {
          bd = fbglobal::fname_fieldbooks(crop = input$fba_src_crop)
          sl = list.files(bd)
        })
      }
      if( input$fba_src_type == "Local"){
        bd = dataInput()
        sl = bd
      }

      # out = NULL
      # set_fb = NULL
      if(!is.null(bd)) {
        set_fb = updateSelectInput(session, "fbaInput", "Fieldbook", choices = sl)
      }
      #set_fb
    })




    gather_params <- function(){
      withProgress(message = "Getting trial info ...", {
        cn = colnames(fbInput()) %>% toupper()
        gti= which(stringr::str_detect(cn, "CODE|INSTN|GENOTYPE|GENO|GERMPLASMNAME|CIPNUMBER"))[1]
        bki= which(stringr::str_detect(cn, "BLOCK|BLK|BLOC" ))[1]
        rpi= which(stringr::str_detect(cn, "REP|REPL|REPLICATION" ))[1]
        pti= which(stringr::str_detect(cn, "PLOT|PLT" ))[1]
        ci = 1:length(cn)
        tti= ci[!ci %in% c(gti, bki, rpi, pti)]
        out = tagList(
          fluidRow(width = 12,
                   column(width = 3, selectInput("fba_set_gen", "Genotype", choices = cn, selected = cn[gti]) ),
                   column(width = 3, selectInput("fba_set_blk", "Block", choices = c(NA, cn), cn[bki])),
                   column(width = 3, selectInput("fba_set_plt", "Plot", choices = cn, selected = cn[pti]) ),
                   column(width = 3, selectInput("fba_set_rep", "Replication", choices = cn, selected = cn[rpi]) )
          )
          ,
          fluidRow(width = 12,
                   column(width = 12,selectInput("fba_set_trt", "Traits", choices = cn, selected = cn[tti], multiple = TRUE))
          )
        )
      })
    out

    }




    output$fbParams <- renderUI({

      req(aFilePath())
      req(input$fbaInput)
      #print(input$fbaInput)
      out = NULL
      if( input$fba_src_type == "Default") return(gather_params())
      if( input$fba_src_type == "Local") {
        if(!stringr::str_detect(input$fbaInput, ".rda")) return(gather_params())
      }
    })



  output$hotFieldbook <- DT::renderDataTable({
    #req(input$fbaInput)
    req(aFilePath())
    withProgress(message = "Getting trial data ...", {
      DF = fbInput()
      # cnm = c(input$fba_set_plt, input$fba_set_blk, input$fba_set_gen, input$fba_set_trt )
      #  if (("NA" %in% cnm)) {
      #   cnm = c(input$fba_set_plt, input$fba_set_gen, input$fba_set_trt )
      #  }
      # # print(cnm)
      # # print(colnames(DF))
      # out = DF
      # try({
      #   out = DF[, cnm]
      # })
      #
      # out

      DF
    })

  },  server = FALSE,  extensions = 'FixedColumns',
  options = list(scrollX = TRUE
                 # ,
                 # fixedColumns = list(leftColumns = 6)
  ),
  selection = list(target = 'column', mode = "single")
  )

  phCorr <- function(DF, trait, useMode = "dendo", maxGermplasm = 9999, filterTrait = NULL){

    #treat <- "germplasmName" #input$def_genotype
    treat <- input$fba_set_gen
    #trait <- input$fba_set_trt
    #trait <- names(DF)[c(7:ncol(DF))]  #input$def_variables
    #trait = input$fbCorrVars
    #print(length(trait))
    if(length(trait) < 2) return(NULL)
    if(!all(trait %in% names(DF))) return(NULL)
    DF = DF[, c(treat, trait)]
    for(i in 2:ncol(DF)){
      DF[, i] = DF[, i] %>% as.character() %>% as.numeric()
    }

      options(warn = -1)
      # exclude the response variable and empty variable for RF imputation
      datas <- names(DF)[!names(DF) %in% c(treat, "PED1")] # TODO replace "PED1" by a search
      x <- DF[, datas]
      for(i in 1:ncol(x)){
        x[, i] <- as.numeric(x[, i])
      }
      y <- DF[, treat] %>% as.factor
      if (any(is.na(x))){
        utils::capture.output(
          DF <- randomForest::rfImpute(x = x, y = y, iter = 3, ntree = 50 )
        )
       }
      names(DF)[1] <- treat
      DF = agricolae::tapply.stat(DF, DF[, treat])
      DF = DF[, -c(2)]
      names(DF)[1] = "germplasmName"
      row.names(DF) = DF$germplasmName
      DF = DF[, -c(1)]
      options(warn = 0)

    #})
    DF
  }
# end phCorr


  output$fieldbook_heatmap <- d3heatmap::renderD3heatmap({
    req(aFilePath())
    #req(input$phFieldMapVarsUI)
    req(input$phFieldMapVars)

    #withProgress({
    trt = input$phFieldMapVars
    #print(trt)
    if(is.null(trt)) return(NULL)
    fm_DF = fbInput()
    if(is.null(fm_DF)) return(NULL)
    # print(colnames(fm_DF))
    # print(str(fm_DF))
    # print(input$fba_set_rep)

    REP = "REP"
    try({
      REP = input$fba_set_rep
      # print(REP)
    })
    # print(REP)

    ##if (!is.null(ci)) trt = names(DF)[ci]

    fm <- fbmaterials::fb_to_map(fm_DF,
                                 gt = input$fba_set_gen, #"germplasmName", #input[["def_genotype"]],
                                 #gt = "TRT1",

                                 rep = REP, #"REP", #input[["def_rep"]],
                                 # blk = input[["def_block"]],
                                 plt = input$fba_set_plt, #"PLOT"  #input[["def_plot"]]
                                 variable = trt
    )
    amap = fm[["map"]]
    anot = fm[["notes"]]
    out = d3heatmap(x = amap,
              cellnote = anot,
              theme = "dark", colors = "Blues",
              Rowv = FALSE, Colv = FALSE,
              dendrogram = "none")
    #}, "Creating field map ...")
    out
  })


output$vcor_output = qtlcharts::iplotCorr_render({
  req(input$fbCorrVars)
  req(length(input$fbCorrVars) > 1)
  DF <- fbInput()
  shiny::withProgress(message = 'Imputing missing values', {
    DF <- phCorr(DF, input$fbCorrVars)
  })
  #str(DF)
  iplotCorr(DF)
})

output$phHeat_output = d3heatmap::renderD3heatmap({
  req(input$phHeatCorrVars)
  req(length(input$phHeatCorrVars) > 1)
  DF <- fbInput()
  shiny::withProgress(message = 'Imputing missing values', {
    DF <- phCorr(DF, input$phHeatCorrVars, useMode = "dendo")
  })
  #print(head(DF))
  par(mar=c(3,1,1,10))
  d3heatmap::d3heatmap(DF, theme = "dark", colors = "Blues")
})

output$phDend_output = renderPlot({
  req(input$phDendCorrVars)
  req(length(input$phDendCorrVars) > 1)
  DF <- fbInput()
  shiny::withProgress(message = 'Imputing missing values', {
    DF <- phCorr(DF, input$phDendCorrVars, useMode = "dendo")
  })
  dend <- DF %>% dist %>% hclust %>% as.dendrogram()

  par(mar=c(3,1,1,10))
  plot(dend, horiz = TRUE)
})

output$phDens_output = renderPlot({
  req(input$phDens)

  #par(mar=c(3,1,1,10))
  DF <- fbInput()
  if(!("REP" %in% names(DF))) return(NULL)
  if(any(is.null(DF$REP))) return(NULL)
  titl = input$phDens

  DF <- DF[, c("REP", titl)]
  DF[, 2] <- as.numeric(DF[, 2])
  n = max(DF$REP)
  cls = c("black", "blue", "red", "orange", "darkgreen", "grey60")

  dens <- density(DF[DF$REP, 2], na.rm = TRUE)
  plot(dens, main = titl, ylim = c(0, 0.8))
  if(n > 1 & n < 6){#Assumption no more than 5 repetitions
    for(i in 1:n){
      #print(DF[DF$REP == i, 2])
      dens <- density(DF[DF$REP == i, 2] , na.rm = TRUE)
      lines(dens, col = cls[i+1])
    }
    legend("topright", legend = c("overall", 1:n), title = "Repetition", lty = 1, col = cls[1:(n+1)])
  }
  #abline(v = mean(DF[, 2], na.rm = TRUE), lwd = 2, col = "grey30")
  rug(DF[, 2], quiet = TRUE)

})





#####################

#observeEvent(input$butDoPhAnalysis, ({

get_traits_with_data <- reactive({
  DF = fbInput()
  ok = sapply(DF, function(x) sum(is.na(x))) / nrow(DF) < .1
  ok = names(DF)[ok]
  ok = ok[stringr::str_detect(ok, " ")]
  ok
})

get_traits_choice <- reactive({
  req(aFilePath)
  #req(input$fbaInput)
  #req(input$fba_set_trt)
  # trts = get_traits_with_data()
  # ci = input$hotFieldbook_columns_selected
  # DF = fbInput()
  # trt_sel = trts[length(trts)]
  # if(!is.null(ci)) {
  #   trt_sel = names(DF)[ci]
  # } else
  #   if(!(trt_sel %in% trts)){
  #     trt_sel = trts[length(trts)]
  #   }
  # # add one more trait so a corr matrix can be shown
  # # choose one at random
  # if(length(trts)>=2){
  #   trt_sel <- c(trt_sel, trts[!trts %in% trt_sel][1] )
  # }
  #
  # list(trts = trts, trt_sel = trt_sel)
  trts = input$fba_set_trt
  trtsel = trts[length(trts)]
  list(trts = trts, trt_sel = trtsel )
})

#### Corr helper
output$fbCorrVarsUI <- renderUI({
   tc = get_traits_choice()
   selectizeInput("fbCorrVars", "Select two or more traits:", tc$trts, selected = tc$trt_sel,
                  multiple = TRUE, width = "100%")
})

output$phHeatCorrVarsUI <- renderUI({
  tc = get_traits_choice()
  selectizeInput("phHeatCorrVars", "Select two or more traits:", tc$trts, selected = tc$trt_sel,
                 multiple = TRUE, width = "100%")
})

output$phDendCorrVarsUI <- renderUI({
  tc = get_traits_choice()
  selectizeInput("phDendCorrVars", "Select two or more traits:", tc$trts, selected = tc$trt_sel,
                 multiple = TRUE, width = "100%")
})



output$aovVarsUI <- renderUI({
  tc = get_traits_choice()
  selectizeInput("aovVars", "Select trait(s):", tc$trts, selected = tc$trt_sel,
                 multiple = TRUE, width = "100%")
})

output$phDensUI <- renderUI({
  tc = get_traits_choice()
  selectizeInput("phDens", "Select trait:", tc$trts, selected = tc$trt_sel,
                 multiple = FALSE, width = "100%")

})

output$phFieldMapVarsUI <- renderUI({
  tc = get_traits_choice()
  selectizeInput("phFieldMapVars", "Select trait:", tc$trts, selected = tc$trt_sel,
                 multiple = FALSE, width = "100%")

})



observeEvent(input$fbRepoDo, {
  output$fbRep <- shiny::renderUI({
    #print("step 1")
    DF <- fbInput()
    trait = input$aovVars

    treat <- input$fba_set_gen #"germplasmName" #input$def_genotype
    #trait = input$fbCorrVars
    if(length(trait) < 1) return(NULL)

    shiny::withProgress(message = 'Imputing missing values', {
      options(warn = -1)
      REP = input$fba_set_rep
      GEN = input$fba_set_gen

      DF = DF[, c(treat, REP,  trait)]

      DF[, treat] <- as.factor(DF[, treat])

      # exclude the response variable and empty variable for RF imputation
      datas <- names(DF)[!names(DF) %in% c(treat, "PED1")] # TODO replace "PED1" by a search
      x <- DF[, datas]
      for(i in 1:ncol(x)){
        x[, i] <- as.numeric(x[, i])
      }
      y <- DF[, treat]
      if (any(is.na(x))){
        utils::capture.output(
          DF <- randomForest::rfImpute(x = x, y = y )
        )
      }
      names(DF)[1] <- treat
    })
    out = "no report"
    if(input$expType == "RCBD"){
      #pepa::repo.rcbd(trait, geno = "germplasmName", rep = "REP", data = DF, format = tolower(input$aovFormat))
      out = repo_ana("rcbd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
    }
    if(input$expType == "CRD"){
      #pepa::repo.crd(trait, geno = "germplasmName",  data = DF, format = tolower(input$aovFormat))
      out = repo_ana("crd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
    }
    if(input$expType == "ABD"){
      #pepa::repo.abd(trait, geno = "germplasmName", rep = "REP", data = DF, format = tolower(input$aovFormat))
      out = repo_ana("abd", trait, geno = GEN, rep = REP, data = DF, format = tolower(input$aovFormat))
    }
    if(input$expType == "A01D"){
      # pepa::repo.a01d(trait, geno = "germplasmName", rep = "REP", block = input$block, k = input$k,
      #                  data = DF, format = tolower(input$aovFormat))
      out = repo_ana("a01d", trait, geno = GEN, rep = REP, block = input$fba_block, k = input$k,
               data = DF, format = tolower(input$aovFormat))

    }
  HTML("<a href='", out, "' target='_new'>Report</a>")

  })


})

}









