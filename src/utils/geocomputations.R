#' Get a layer from a web coverage service within a bounding box
#'
get_wcs_layer <- function(wcs = c("dtm", "omz"),
                          bbox, #xmin, xmax, ymin, ymax
                          layername,
                          resolution,
                          crs = "EPSG:31370",
                          version = c("1.0.0", "2.0.1")) {
  # prelim check
  version <- match.arg(version)
  wcs <- match.arg(wcs)

  # set url
  wcs <- ifelse(
    wcs == "omz",
    "https://inspire.informatievlaanderen.be/overdrachtdiensten/oi-omz/wcs",
    "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dtm/wcs"
  )

  assertthat::assert_that(is.character(layername))
  assertthat::assert_that(is.character(crs))
  assertthat::assert_that(
    is.vector(bbox, mode = "numeric"),
    length(bbox) == 4)

  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  assertthat::assert_that(is.numeric(resolution))

  # build url request
  url <- parse_url(wcs)

  if (version == "2.0.1") {
    stop(paste0("code for version = ", version, "is not yet working"))
    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGEID = layername,
                      CRS = crs,
                      SUBSET = paste0("x,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["xmin"],
                                      ",",
                                      bbox["xmax"],")"),
                      SUBSET = paste0("y,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["ymin"],
                                      ",",
                                      bbox["ymax"],")"),
                      #SCALEFACTOR = 50,
                      FORMAT = "image/tiff",
                      RESPONSE_CRS = crs
    )
    request <- build_url(url)
    # download een mht bestand met tif erin
    # geen idee hoe deze tif uit mht te halen
    file <- tempfile(fileext = ".mht")
    GET(url = request,
        write_disk(file))
  }

  if (version == "1.0.0") {
    result_width <- (bbox["xmax"] - bbox["xmin"]) / resolution
    result_height <- (bbox["ymax"] - bbox["ymin"]) / resolution

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGE = layername,
                      CRS = crs,
                      BBOX = paste(
                        bbox["xmin"],
                        bbox["ymin"],
                        bbox["xmax"],
                        bbox["ymax"],
                        sep = ","),
                      WIDTH = result_width,
                      HEIGHT = result_height,
                      FORMAT = "geoTIFF",
                      RESPONSE_CRS = crs
    )
    request <- build_url(url)
  }

  file <- tempfile(fileext = ".tif")
  GET(url = request,
      write_disk(file))

  raster <- terra::rast(file)
  return(raster)
}



#' Calculate a canopy height model from a digital surface model and a digital
#' terrain model
#'
#' The digital terrain model is obtained from a web coverage service
calc_chm <- function(dsm, overwrite = FALSE) {


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

  # get drone image extent
  floor_extent_dsm <- floor(ext(dsm))
  ext(dsm) <- floor_extent_dsm

  bbox_vec <- as.vector(floor_extent_dsm)

  # get dtm image
  dtm_crop <- get_wcs_layer(
    wcs = "dtm",
    bbox = bbox_vec,
    layername = "EL.GridCoverage.DTM",
    resolution = 1)

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
calc_ndvi <- function(dsm, overwrite = FALSE) {


  # prelim check

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

  # get cir raster
  cir_raster <- get_wcs_layer(wcs = "omz",
                              bbox = bbox_vec,
                              layername = "OI.OrthoimageCoverage.OMZ.CIR",
                              resolution = 0.4)
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



#' Calculate (extract) coverage fractions of raster values inside a polygon
#'
#' Helper functions used in calc_bufferstats
calc_areas <- function(df, raster, fun = NULL) {
  list_df <- exactextractr::exact_extract(x = raster, y = df, fun = fun)

  if (is.null(fun)) {
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
  } else {
    return(list_df)
  }
}

#' Calculate area covered by raster class values in buffer zones surrounding
#' GRTS points
#'
#' First case. A vector of cuts is given (and fun = NULL), in which case the
#' raster layer is classified into ordinal classes using the breakpoints
#' provided in cuts. The result will be a dataframe containing for each GRTS
#' bufferzone the area (m2) and proportion of classes present within that zone.
#' Second case. A function (e.g. mean, see ?exactextractr::exact_extract()) is
#' given and the corresponding summary value is calculated for each bufferzone.
#'
#' @param raster A spatRaster.
#' @param cuts A vector of breakpoints to classify the values in the raster.
#' Should include lower- and uppermost theoretical values.
#' @param grts_raster A spatRaster representing the GRTSmaster_habitats raster.
#' @param bufferdist A distance in meter to be used as bufferradius around each
#' GRTS point.
#' @param mask A polygon layer to be used as mask for the selection of GRTS
#' points for which calculations are needed. Points that are not inside a
#' polygon will be masked.
calc_bufferstats <- function(
  raster,
  cuts,
  grts_raster,
  bufferdist,
  mask = NULL,
  fun = NULL) {

  # prepare mask
  if (!is.null(mask)) {
    assertthat::assert_that(inherits(mask, "sf"))
    mask <- mask %>%
      st_buffer(dist = 2 * bufferdist) %>%
      vect()
    # mask rasters
    raster <- terra::mask(raster, mask = mask)
    grts_raster <- grts_raster %>%
      terra::crop(raster) %>%
      terra::mask(mask = mask)
  }

  # buffer points
  grts_buffer <- grts_raster %>%
    crop(raster) %>%
    as.points() %>%
    as.data.frame(geom = TRUE) %>%
    sf::st_as_sf(wkt = "geometry",
                 crs = "epsg:31370") %>%
    sf::st_buffer(dist = bufferdist)

  # reclassify raster
  if (!missing(cuts)) {
    raster <- classify(raster, cuts, include.lowest = TRUE)
  }

  # convert to RasterLayer
  raster <- raster::raster(raster)

  # extract values and coverage_fraction of raster
  # per chunk of chunksize locations to avoid out of memory in case of large buffers
  result <- grts_buffer %>%
    select(GRTSmaster_habitats) %>%
    nest(data = everything()) %>%
    mutate(areas = map(.x = data,
                       .f = calc_areas,
                       raster = raster,
                       fun = fun))

  if (!missing(cuts)) {
    classes_df <- data.frame(lower = cuts, upper = lead(cuts)) %>%
      filter(!is.na(upper)) %>%
      mutate(class = 1:n(),
             label = paste0("[",lower,",", upper,"]")) %>%
      select(-lower, -upper)

    result <- result %>%
      unnest(c(data, areas)) %>%
      unnest(areas) %>%
      select(-geometry) %>%
      ungroup() %>%
      rename(class = value) %>%
      left_join(classes_df, by = "class") %>%
      mutate(proportion = area_m2 / (pi * bufferdist^2),
             bufferdist = bufferdist)
    return(result)
  } else {
    return(result %>%
             rename(result = areas))
  }
}
