
#' locations
#'
#' @param input shiny
#' @param output shiyn
#' @param session shiny
#' @import shiny
#' @importFrom magrittr '%>%'
#' @author Reinhard Simon
# @return data.frame
#' @export
locations <- function(input, output, session){

  # greenLeafIcon <- leaflet::makeIcon(
  #   iconUrl = "http://leafletjs.com/docs/images/leaf-green.png",
  #   iconWidth = 38, iconHeight = 95,
  #   iconAnchorX = 22, iconAnchorY = 94,
  #   shadowUrl = "http://leafletjs.com/docs/images/leaf-shadow.png",
  #   shadowWidth = 50, shadowHeight = 64,
  #   shadowAnchorX = 4, shadowAnchorY = 62
  # )

  #if(!exists("brapi")) return()

  crop = "sweetpotato"

  get_base_dir <- function(){
    getwd()
  }

  get_base_data <- function(source = "brapi", acrop = crop, atype = "fieldbook"){
    bd = file.path(get_base_dir(), "xdata")
    fp = file.path(bd, source, acrop, atype)
    if(!dir.exists(fp)) dir.create(fp, recursive = TRUE)
    fp
  }

  get_plain_host <- function(){
   brapi::get_brapi()
  }

  return_null_with_msg <- function(msg){
    cat(msg)
    return(NULL)
  }


  dat <- reactive({
    fp = file.path(get_base_data(atype = "location"), "locations.rda")
    dat = NULL
    try({
      if(file.exists(fp)) {
        dat = readRDS(file = fp)
      }
    })

    if(is.null(dat)) {

    try({
      if(is.null(brapi)) return_null_with_msg("Not connection to a BrAPI db set. Please connect.")
      dat <- brapi::locations_list()
      saveRDS(dat, file = fp)
    })
    if(is.null(dat)){
      return_null_with_msg("Could not retrieve data from database. Check your login details and internet connection.")
    }
    }
    dat[!is.na(dat$latitude), ]
  })



  dat_sel <- reactive({
    #req(input$tableLocs)
    if(is.null(dat())) return_null_with_msg("Could not retrieve data from database. Check your login details and internet connection.")
    sel = input$tableLocs_rows_all
    if(is.null(sel)){
      pts = dat()
    } else {
      pts = dat()[sel, ]
    }
    pts
  })

  output$tableLocs <- DT::renderDataTable( dat()
                                      , server = FALSE,
                                       options = list(scrollX = TRUE))

  output$mapLocs <- leaflet::renderLeaflet({
    #print("1")
    pts <- dat_sel()
    #print("2")
    if(is.null(pts)) pts <- dat()
    #print("3")

    if(is.null(pts)) return_null_with_msg("Could not retrieve data from database. Check your login details and internet connection.")
    #print("4")

    leaflet::leaflet(pts, height = "100%") %>%
      leaflet::addTiles() %>%
      leaflet::addAwesomeMarkers(clusterOptions = leaflet::markerClusterOptions(clickable = T)) %>%
      leaflet::fitBounds(
        ~min(pts$longitude), ~min(pts$latitude),
        ~max(pts$longitude), ~max(pts$latitude)
      )


  })



  # download the filtered data
  output$locsDL = downloadHandler('BRAPI-locs-filtered.csv', content = function(file) {
    utils::write.csv(dat_sel(), file)
  })


  mrks <- reactive({
    x = input$mapLocs_marker_click
    subset(dat_sel(), dat_sel()$latitude == as.numeric(x$lat) &
             dat_sel()$longitude == as.numeric(x$lng))
  })

  output$histogram <- renderPlot({
    graphics::hist(dat()$altitude, main = "Frequency of altitude of breeding locations.",
         xlab = "altitude [m]", sub = "Selected location frequencies are in red.")
    graphics::hist(dat_sel()$altitude, add = T, col = "red")
    if(length(mrks()) > 0){
      # print("abline")
      # print(mrks()$altitude)
      graphics::abline(v = mrks()$altitude, col="blue", lwd = 5)
    }
  })

  ##################################

  rec2info <- function(rec){
    #rec %>% as.data.frame
    nms = names(rec)
    dat = t(rec)
    dat = cbind(nms, dat)
    #rint(str(dat))
    # print(nrow(dat))
    row.names(dat) = 1:nrow(dat)
    colnames(dat) = c("Attribute", "Value")
    dat = dat[c(1, 5, 9, 4, 3, 2, 6, 7, 8, 10, 12, 13, 14, 11), ]
    x = htmlTable::htmlTable(dat)
    paste0("<center>", x, "</center>") %>% HTML
  }

  observe({
    rec = mrks()
    if (nrow(rec)==1) {
      output$siteInfo <- renderUI({
        #str(rec) %>% paste %>% print
        rec2info(rec)
      })
    } else {
      output$siteInfo = renderUI({
        "No location selected."
      })
    }

  })


  ############### report #########

  output$rep_loc <- renderUI({

    withProgress(message = 'Updating report', value = 0, max = 10, {

    locs <- dat_sel()
    n = nrow(locs)
    if(n<1) return("no locations in view!")
    #report = paste0("report_location.Rmd")
    #report = file.path("inst", "rmd", "report_location.Rmd")
    rep_name = "report_location.Rmd"
    report = file.path(system.file("rmd", package = "brapps"), rep_name)
    rep_dir <- "www/reports/"
    # if(!file.exists(rep_dir)){
    #   rep_dir = tempdir()
    # }
    report_dir = system.file("apps/hdtest/reports", package = "brapps")
    wd = getwd()

    setProgress(5)

    fn <- withr::with_dir(report_dir, {
      rmarkdown::render(report,
                            #output_format = "all",
                            output_dir = file.path(wd, "www"), #rep_dir,
                            params = list(
                              locs = locs))
    })
    setProgress(8)

    #html <- readLines(file.path(rep_dir, "report_location.html"))
    report_html = stringr::str_replace(rep_name, ".Rmd", ".html")
    #output$rep_loc <- renderUI("")
    report = file.path(wd, "www", report_html)

    }) # progress

    html <- includeHTML(report)
    HTML(html)
  })


  get_geo_locs <- function(){
    #locs = dat() #  brapi::locations_list()
    #filter out those without georefs
    #locs = locs[!is.na(locs$latitude), ]
    #if(nrow(locs) == 0) return(NULL)
    #locs
    #dat()
  }

  get_geo_mark <- function(){
    click<-input$mapLocs_marker_click
    if(is.null(click))
      return(NULL)
    # leaflet::clearMarkers(input$mapLocs)
    #
    # leaflet::addMarkers(input$mapLocs,lat =   click$lat, lng= click$lng, icon = greenLeafIcon)

    locs = dat() #  get_geo_locs()

    locs[locs$latitude == click$lat & locs$longitude == click$lng, ]
  }

  get_all_studies <- function(){
    fp = file.path(get_base_data(atype = "fieldbook"), "fieldbooks.rda")
    stds = NULL
    try({
      if(file.exists(fp)) {
        stds = readRDS(file = fp)
      }
    })

    if(is.null(stds)){
      stds = brapi::studies()
      saveRDS(stds, fp)
    }
    stds
  }

  get_trials_for_location <- function(){
    locs = get_geo_mark()
    #print(locs)
    if(is.null(locs)) return(NULL)

    stds = get_all_studies()
    # 1st try to find via id if not use unique name
    stds <- stds[!is.na(stds$locationDbId),]
    sid = stds[stds$locationDbId == locs$locationDbId, "studyDbId"]
    # if (length(sid) == 0) {
    #   sid = stds[stringr::str_detect(toupper(stds$name), locs$Uniquename), "studyDbId"]
    # }
    sid
  }


  output$site_fieldtrials <- renderUI({
    withProgress(message = 'Getting trial list ...', value = 0, max = 10, {
    sid = get_trials_for_location()
    #print(sid)
    if(is.null(sid)){
      out = "No location selected."
    } else {
    setProgress(5)

    if(length(sid) > 0){
      host = brapi$db  #get_plain_host()
      path = "/breeders/trial/"
      if(!stringr::str_detect(host, "http")){
        host = paste0("http://", host)
      }
      stds = get_all_studies()

      out = paste0("<br><a href='",host, path, sid, "' target='_blank'>", stds[stds$studyDbId %in% sid, "name"], "</a>") %>%
        paste(collapse = ", ")
      }

      setProgress(8)

      }
    })
    HTML(out)

  })


  output$site_genotypes <- renderUI({
    out = "No data retrieved."
    if(!is.null(get_geo_mark())){
    withProgress(message = 'Getting trial list ...', value = 0, max = 10, {
      sid = get_trials_for_location()
      #print(sid)
      year = NULL
      stds = get_all_studies()
      if(length(sid) >0){
        # get a study from the most recent year
        if(length(sid) > 1){
          xs = stds[stds$studyDbId %in% sid, 1]

          try({
            xs = stds
            xs$years = as.integer(xs$years)
            xs = xs[xs$studyDbId %in% sid,]
            xs = xs[max(xs$years) == xs$years, ]
          })
          stds = xs
          sid = xs$studyDbId

        } else {

            stds = stds[stds$studyDbId == sid, ]

        }
      }
      #print(stds)
      year = stds$years

      fb = NULL
        fbid = paste0(stds$studyDbId, ".rda")

        if(!is.null(year) & !is.na(year)){
          fp1 = file.path(get_base_data(atype = "fieldbook"), year)
        } else {
          fp1 = file.path(get_base_data(atype = "fieldbook"))
        }
        if(!dir.exists(fp1)) dir.create(fp1)
        fp = file.path(fp1, fbid)


        try({
          if(file.exists(fp)) {
            fb = readRDS(file = fp)
          }
        })

        if(is.null(fb)){
          fb = brapi::study_table(sid[1])
          saveRDS(fb, fp)
        }


      if(is.null(fb)) return(NULL)
      topgp = brapi::get_top_germplasm(fb)

      gid = topgp$germplasmDbId
      gnm = topgp$germplasmName
      hid = topgp$`Harvest index computing percent`

      host = brapi$db

      path = "/stock/"

      #TODO change for genotypes
      out = paste0("<a href='http://",host, path, gid,"/view' target='_blank'>", gnm, " (",hid,  ")</a>")
      txt = paste0("Top genotypes for trait (", "Harvest index" ,") from most recent (", year
                   ,") fieldbook: ", stds$name,
                   " for location: ",get_geo_mark()$name,":</br>") # TODO make trait choosable
      out = paste( out, collapse = ", ")
      out = paste(txt, out)

    setProgress(8)

    })
    }
    HTML(out)
  })


}
