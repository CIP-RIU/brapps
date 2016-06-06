
#' locations
#'
#' @param input shiny
#' @param output shiyn
#' @param session shiny
#' @param values shiny
#' @import shiny
#' @importFrom magrittr '%>%'
#' @importFrom brapi can_internet
#' @author Reinhard Simon
# @return data.frame
#' @export
locations <- function(input, output, session, values){

  crop = isolate(values$crop)
  mode = isolate(values$mode)
  msg_no_loc = "No location selected."

  url = system.file("images", package = "brapps")
  greenLeafIcon <- leaflet::makeIcon(

    iconUrl = file.path(url, "leaf-green.png"),
    iconWidth = 38, iconHeight = 95,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = file.path(url, "leaf-shadow.png"),
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )



  # get_base_data <- function(mode = "brapi", acrop = crop, atype = "fieldbooks"){
  #   bd = fbglobal::get_base_dir(mode = mode)
  #   fp = file.path(bd, acrop, atype)
  #   #print("get base data")
  #   #rint(fp)
  #   if(!dir.exists(fp)) dir.create(fp, recursive = TRUE)
  #   fp
  # }

  return_null_with_msg <- function(msg){
    cat(msg)
    return(NULL)
  }

  fp = file.path(get_base_data(atype = "location", acrop = crop), "locations.rda")

  locationData <- reactiveFileReader(10000, session, fp, readRDS)

  dat <- reactive({
    dat = NULL
    if(file.exists(fp)){
      dat = locationData()
    }

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
    out = dat[!is.na(dat$latitude), ]
    #print(head(dat))
    dat
  })



  dat_sel <- reactive({
    #req(input$tableLocs)
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
    pts <- dat_sel()
    if(is.null(pts)) pts <- dat()
    if(is.null(pts)) return_null_with_msg("Could not retrieve data from database. Check your login details and internet connection.")
    pts = pts[!is.na(pts$longitude), ]

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

  output$siteInfo <- renderUI({
    out = msg_no_loc
    rec = mrks()
    if (nrow(rec)==1) {
      out = rec2info(rec)
    }

    HTML(out)
  })


  ############### report #########

  output$rep_loc <- renderUI({

    withProgress(message = 'Updating report', value = 0, max = 10, {

    locs <- dat_sel()
    n = nrow(locs)
    if(n<1) return("no locations in view!")
    rep_name = "report_location.Rmd"
    #tgt = file.path(getwd(), "reports", rep_name)
    report <- file.path(getwd(), "reports", rep_name)
    dn = dirname(report)
    if(!dir.exists(dn)) {
      dir.create(report)
    }
    if(!file.exists(report)){

      org = system.file("/apps/hdtest/reports/report_location.Rmd", package = "brapps")

      file.copy(org, report)
    }



    setProgress(5)
    fn = "no report created."
    try({
    fn <- rmarkdown::render(report,
                            output_dir = file.path("www", "reports"), #rep_dir,
                            params = list(
                              locs = locs))
    })
    setProgress(8)
    }) # progress

    html <- includeHTML(fn)
    HTML(html)
  })


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

  # get_all_studies <- function(){
  #   fp = file.path(get_base_data(atype = "fieldbook"), "fieldbooks.rda")
  #   stds = NULL
  #   try({
  #     if(file.exists(fp)) {
  #       stds = readRDS(file = fp)
  #     }
  #   })
  #   if(is.null(stds)){
  #     stds = brapi::studies()
  #     saveRDS(stds, fp)
  #   }
  #   stds
  # }

  get_study_path <- function(year, id){
    # if(can_internet()){
    #   mode = "brapi"
    # } else {
    #   mode = "Demo"
    # }
    mode = "brapi"
    if(is.null(year)){
      fp = file.path(get_base_data(atype = "fieldbook", mode = mode, acrop = crop), paste0(id,".rda"))
    }
    if(!is.null(year)){
      fp = file.path(get_base_data(atype = "fieldbook", mode = mode, acrop = crop), year, paste0(id, ".rda"))
    }
    dn = dirname(fp)
    #print(dn)
    if(!dir.exists(dn)) dir.create(dn, recursive = TRUE)
    fp
  }
#
#   get_study <- function(year, id){
#
#     fp = get_study_path(year, id)
#     stdy = NULL
#     try({
#       if(file.exists(fp)) {
#         stdy = readRDS(file = fp)
#       }
#     })
#     if(is.null(stdy)){
#       if(can_internet() & !is.null(brapi)){
#         stdy = brapi::study_table(id)
#         saveRDS(stdy, fp)
#       }
#      }
#     stdy
#   }

  get_trials_for_location <- function(){
    locs = get_geo_mark()
    if(is.null(locs)) return(NULL)

    stds = get_all_studies()
    stds <- stds[!is.na(stds$locationDbId), ]
    sid = stds[stds$locationDbId %in% locs$locationDbId, "studyDbId"]

    # Download most recent trial for this location!
    if(can_internet()){
      ms = max(sid)
      xs = stds[stds$studyDbId == ms, ]
      ss = get_study(stds$years[ms], ms)
    }
    sid
  }


  output$site_fieldtrials <- renderUI({
    html = msg_no_loc
    withProgress(message = 'Getting trial list ...', value = 0, max = 10, {
    sid = get_trials_for_location()
    #print(sid)
    if(is.null(sid)){
      out = msg_no_loc
    } else {
    setProgress(5)

    if(length(sid) > 0){

      stds = get_all_studies()
      stds = stds[stds$studyDbId %in% sid, ]

      txt = paste0("No internet connected!<br/>")
      out = stds$name %>% paste(collapse = ", ")

      if(can_internet() & !is.null(brapi)){
        txt = ""

        path = "/breeders/trial/"
        db = brapi$db
        host = db
        if(!stringr::str_detect(db, "@")){
          if(!stringr::str_detect(db, "http")) {
            host = paste0("http://", db)
          }
        }
        if(rstudioapi::isAvailable()){
          if(!stringr::str_detect(db, "http")) {
            host = paste0("http://", db)
          }
        }
        #print(sid)
        out = paste0("<br><a href='",host, path, sid, "' target='_blank'>", stds$name, "</a>") %>%
          paste(collapse = ", ")

      }
      html = paste0(txt, out)
    }

      setProgress(8)

      }
    })
    HTML(html)

  })


  output$site_genotypes <- renderUI({
    out = msg_no_loc

    #if(!can_internet()) return("No internet connected!")
    #if(!is.null(get_geo_mark())){
    withProgress(message = 'Getting trial list ...', value = 0, max = 10, {
      sid = get_trials_for_location()
      #print(paste("geno/site",sid))
      if(is.null(sid)){
        out = msg_no_loc
      } else {
      #print(sid)
      year = NULL
      stds = get_all_studies()

      #ms = NULL
      if(length(sid) > 1){
        sid = max(sid)

      }
      stds = stds[stds$studyDbId == sid, ]
      year = stds$years

      #print(year)
      fb = NULL
      fb = get_study(year, sid)
      #print(sid)


      res = NULL

      if(is.null(fb)) res = "The most recently added trial for this site seems to have no data!"

      if(is.null(res)){
        topgp = brapi::get_top_germplasm(fb)
        #print(topgp)
        if(is.null(topgp))
          res = "Cannot find this trait in the most recently added trial."

      }


      if(is.null(res)){
      gid = topgp$germplasmDbId
      gnm = topgp$germplasmName
      hid = topgp$`Harvest index computing percent`
      txt = ""
      if(can_internet() ){
        db = brapi$db
        host = db
        if(!stringr::str_detect(db, "@")){
          if(!stringr::str_detect(db, "http")) {
            host = paste0("http://", db)
          }
        }
        if(rstudioapi::isAvailable()){
          if(!stringr::str_detect(db, "http")) {
            host = paste0("http://", db)
          }
        }

        path = "/stock/"
        out = paste0("<a href='",host, path, gid,"/view' target='_blank'>", gnm, " (",hid,  ")</a>")

        out = paste(out, collapse = ", ")
        txt = ""
        #print("here")
        #print(out)

      }
      if(!can_internet()){
        out = paste0 (gnm, " (",hid,  ")")
        out = paste(out, collapse = ", ")
        txt = paste("No internet connected!</br></br>")
      }

      locs = dat()
      loc_name = locs[locs$locationDbId %in% sid, "name"]
      txt = paste0(txt, "Top genotypes for trait (", "Harvest index" ,") from most recent (", year
                   ,") fieldbook: ", stds$name,
                   " for location: ",loc_name,":</br>") # TODO make trait choosable
      res = paste(txt, out)
      }

      out = res
    setProgress(8)
      }

    })
    #}
    HTML(out)
  })


  observeEvent(input$mapLocs_marker_click, {
    ## Get the click info like had been doing
    click <- input$mapLocs_marker_click
    clat <- click$lat
    clng <- click$lng
     leaflet::leafletProxy('mapLocs') %>% # use the proxy to save computation
      leaflet::addMarkers(lng = clng, lat = clat, layerId = "marked", icon = greenLeafIcon)

  })
  #dat()
}
