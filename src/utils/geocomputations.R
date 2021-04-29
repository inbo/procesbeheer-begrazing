#' Calculate a canopy height model from a digital surface model and a digital
#' terrain model
calc_chm <- function(dsm, dtm, overwrite = FALSE) {


  # prelim check
  destination <- file.path(get_map_procesbeheer(), "chm")

  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  tifname <- paste0(gsub("DEM", "CHM", dsm@ptr$names), ".tif")

  if (file.exists(file.path(destination, tifname)) && !overwrite) {
    chm <- terra::rast(file.path(destination, tifname))
    return(chm)
  }

  # crop dtm to drone image extent
  floor_extent_dsm <- floor(ext(dsm))
  ext(dsm) <- floor_extent_dsm
  dtm_crop <- crop(dtm, dsm)

  # disaggregate dtm to 0.025 x 0.025 resolution
  dtm_crop <- disaggregate(dtm_crop, fact = 1/0.025)

  # resample dsm so it has same raster as dtm_crop
  dsm <- terra::resample(dsm, dtm_crop)

  # wkt string overschrijven (zelfde crs, maar verschillende representatie)
  crs(dsm) <- crs(dtm_crop)

  # calculate CHM
  chm <- dsm - dtm_crop

  # Write the rasterfile
  terra::writeRaster(x = chm,
                     filename = file.path(destination, tifname),
                     gdal = c("COMPRESS=LZW", "TFW=YES"),
                     overwrite = overwrite)

  return(chm)
}


#' Calculate normalized difference vegetation index (NDVI) from a false-colour
#' infrared image
#'
#' A digital surface model (drone image) is used as input to determine the
#' extent for which ndvi is needed.
#' The false-colour infrared image is obtained via a web coverage service (WCS).
calc_ndvi <- function(dsm, version = c("1.0.0", "2.0.1"), overwrite = FALSE) {


  # prelim check
  version <- match.arg(version)

  destination <- file.path(get_map_procesbeheer(), "ndvi")

  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  tifname <- paste0(gsub("DEM", "NDVI", dsm@ptr$names), ".tif")

  if (file.exists(file.path(destination, tifname)) && !overwrite) {
    ndvi <- terra::rast(file.path(destination, tifname))
    return(ndvi)
  }

  # get extent from dsm
  floor_extent_dsm <- floor(ext(dsm))

  bbox_vec <- as.vector(floor_extent_dsm)

  # wcs service
  wcs <- "https://inspire.informatievlaanderen.be/overdrachtdiensten/oi-omz/wcs"
  url <- parse_url(wcs)

  #url$query <- list(service = "wcs",
  #                  version = "2.0.1",
  #                  request = "DescribeCoverage",
  #                  CoverageId = "OI.OrthoimageCoverage.OMZ.CIR")
  if (version == "2.0.1") {
    stop(paste0("code for version = ", version, "is not yet working"))
    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGEID = "OI.OrthoimageCoverage.OMZ.CIR",
                      SUBSETTINGCRS = "http://www.opengis.net/def/crs/EPSG/0/31370",
                      SUBSET = paste0("x(",
                                      bbox_vec["xmin"],
                                      ",",
                                      bbox_vec["xmax"],")"),
                      SUBSET = paste0("y(",
                                      bbox_vec["ymin"],
                                      ",",
                                      bbox_vec["ymax"],")")
                      #  ,format = "image/tiff", #optional
                      #  OUTPUTCRS = "EPSG:31370" #equal to SUBSETTINGCRS if no outputCrs defined
    )
    request <- build_url(url)
  }

  if (version == "1.0.0") {
    resolution <- 0.4
    result_width <- (bbox_vec["xmax"] - bbox_vec["xmin"]) / resolution
    result_height <- (bbox_vec["ymax"] - bbox_vec["ymin"]) / resolution

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGE = "OI.OrthoimageCoverage.OMZ.CIR",
                      CRS = "EPSG:31370",
                      BBOX = paste(
                        bbox_vec["xmin"],
                        bbox_vec["ymin"],
                        bbox_vec["xmax"],
                        bbox_vec["ymax"],
                        sep = ","),
                      WIDTH = result_width,
                      HEIGHT = result_height,
                      FORMAT = "geoTIFF",
                      RESPONSE_CRS = "EPSG:31370"
    )
    request <- build_url(url)
  }

  file <- tempfile(fileext = ".tif")
  GET(url = request,
      write_disk(file))

  cir_raster <- terra::rast(file)
  nir <- cir_raster[[1]]
  red <- cir_raster[[2]]
  ndvi <- (nir - red) / (nir + red)

  # Write the rasterfile
  terra::writeRaster(x = ndvi,
                     filename = file.path(destination, tifname),
                     gdal = c("COMPRESS=LZW", "TFW=YES"),
                     overwrite = overwrite)

  return(ndvi)

}

#' Calculate the contour of the area covered by a drone flight mission.
#'
#' WIP
calc_contour <- function(raster) {
  raster2 <- classify(raster, c(-Inf, +Inf))
  raster2 <- raster::raster(raster2)
  raster2 <- raster::rasterToPolygons(raster2)
  raster2 <- sf::st_as_sf(raster2)
  return(raster2)
}


#' Calculate (extract) coverage fractions of raster values inside a polygon
#'
#' Helper functions used in calc_bufferstats
calc_areas <- function(df, raster) {
  list_df <- exactextractr::exact_extract(x = raster, y = df)
  area_for_one <- function(x) {
    # calculate area of one rastercell
    cellarea <- res(raster)[1] * res(raster)[2]
    x %>%
      group_by(value) %>%
      summarise(area_m2 = sum(coverage_fraction * cellarea),
                .groups = "drop_last")
  }
  out <- map(list_df, area_for_one)
  return(out)
}

#' Calculate area covered by raster class values in buffer zones surrounding
#' GRTS points
#'
#'
calc_bufferstats <- function(
  raster,
  cuts,
  grts_raster,
  bufferdist) {

  # buffer points
  grts_buffer <- grts_raster %>%
    crop(raster) %>%
    as.points() %>%
    as.data.frame(geom = TRUE) %>%
    sf::st_as_sf(wkt = "geometry",
                 crs = "epsg:31370") %>%
    sf::st_buffer(dist = bufferdist)

  # reclassify raster
  raster_classes <- classify(raster, cuts, include.lowest = TRUE)

  # convert to RasterLayer
  raster_classes <- raster::raster(raster_classes)

  # extract values and coverage_fraction of raster
  # per chunk of chunksize locations to avoid out of memory in case of large buffers
  result <- grts_buffer %>%
    select(GRTSmaster_habitats) %>%
    nest(data = everything()) %>%
    mutate(areas = map(.x = data,
                       .f = calc_areas,
                       raster = raster_classes))

  result <- result %>%
    unnest(c(data, areas)) %>%
    unnest(areas) %>%
    select(-geometry) %>%
    ungroup() %>%
    rename(class = value) %>%
    mutate(proportion = area_m2 / (pi * bufferdist^2),
           bufferdist = bufferdist)
  return(result)
}


