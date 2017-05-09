# crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

add_TmeanPTotal <- function(locs) {
  coords <- as.data.frame(locs[, c("longitude", "latitude")])
  pa <- raster::raster(file.path(system.file("raster/PAn.gri", package = "brapps")))
  ta <- raster::raster(file.path(system.file("raster/TAnMn.gri", package = "brapps")))

  rs <- raster::stack(ta, pa)

  tp <- raster::extract(rs, coords)
  tp <- as.data.frame(tp)
  names(tp) <- c("annualMeanTemperature", "annualTotalRainfall")
  tp$annualMeanTemperature <- tp$annualMeanTemperature / 10

  locs <- cbind(locs, tp)
  return(locs)
}

chart_envelope <- function(locs) {
  plot(locs[, c("annualMeanTemperature", "annualTotalRainfall")], pch = 21, col = "red")
}
