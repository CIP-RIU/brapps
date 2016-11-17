
brapi_sync <- function(dbName, pb = NULL){
  # progress bar!

  # wobble time
  pause_sec <- function(){
    s = sample(0:2 + seq(.4, .6, .1 ), 1)
    message(paste("Pausing: ", s , "sec(s)" ))
    Sys.sleep(s)
  }

  # configure
  pb <- progress::progress_bar$new(
    format = " downloading [:bar] :percent in :elapsed",
    total = 100, clear = FALSE, width= 60)

  pb$tick()
  Sys.setenv(BRAPI_DBNAME = dbName)
  db = get_brapi_db()
  dbDet = db[db$dbName == dbName, ]
  with(dbDet,
       brapi::brapi_con(crop = dbCrop, db = dbUrl , port = as.integer(dbPort),
                        user = dbUser, pwd = dbPassword)
  )

  pb$tick()

  # # download all locations
  dn = file.path(fbglobal::get_base_dir(amode = "brapi", TRUE), dbDet$dbCrop)
  # locations <- brapi::locations_list()

  # if(!dir.exists(dn)) dir.create(dn)
  # sts <- file.path(dn, "table_sites.rds" )
  # saveRDS(locations, file = sts)
  #
  # pb$tick()
  # pause_sec()
  #
  # # download summary table of studies
  s = brapi::studies()
  sts <- file.path(dn, "table_studies.rds" )
  saveRDS(s, file = sts)

  pb$tick()
  pause_sec()
  #
  # # cycle through all studies
  n = nrow(s)
  #n = 33
  dn = file.path(dn, "fieldbooks")
  if(!dir.exists(dn)) dir.create(dn)

  for(i in 8:n){
    message(paste0("Processing ", i, "/", n))
    r = s[i, ]
    sid = r$studyDbId
    fn <- file.path(dn, paste0(sid, ".rds" ))

    pb$tick()
    pause_sec()

    fc <- tryCatch({
      brapi::study_table(sid)

    }
    , error = function(e){
      unlink(fn)
      NULL
    }
    )


    #if(is.null(fc) | all(is.na(fc$PLOT))){
    #print(str(fc))
    #print(fc)
    if(!is.null(fc)){
      if(!("PLOT" %in% fc$PLOT)){
        fc <- cbind(PLOT = 1:nrow(fc), fc)
      }
      saveRDS(fc, file = fn)
    }

  }

  # save synchronization status

}

brapi_sync("sweetpotatobase")

#brapi_sync("cassavabase")
