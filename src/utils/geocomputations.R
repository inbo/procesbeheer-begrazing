#' Convert a single point location to a grid cell polygon
#'
#' @param xy an object of class POINT
#' @param cell_width_m cell width in meter, default 500
#' @param point_position default center of grid cell
#' @param crs default EPSG code 31370
#'
#' @return
#' @export
#'
#' @examples
point_to_gridcell <- function(
    xy,
    cell_width_m = 500,
    point_position = c("center", "lowerleft", "upperleft", "lowerright", "upperright"),
    crs = 31370) {
  point_position <- match.arg(point_position)

  if (point_position != "center") stop(point_position, " not yet implemented")

  stopifnot(sf::st_is(xy, "POINT"))
  xy_df <- sf::st_drop_geometry(xy)
  xy <- sf::st_geometry(xy)

  # buffer with 1 point per quandrant
  xy_buffer <- sf::st_buffer(x = xy,
                             dist = cell_width_m / 2,
                             nQuadSegs = 1)

  # rotate 45 degrees around centroid
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  pl <- (xy_buffer - xy) * rot(pi/4) + xy
  pl <- sf::st_sf(data.frame(xy_df, pl), crs = crs)
  return(pl)
}


#' Calculation of land-use metrics within a grid cell
#'
#' @param grid_cell A polygon within which boundaries zonal statistics will be
#' calculated
#' @param layer A rasterlayer containing land use classes or a polygon layer (sf object)
#' @param grid_group_by_col A character vector of columns to group by for zones
#' @param layer_group_by_col A character vector of columns to group by for
#' layer
#'
#' @return
#' @export
#'
#' @examples
landusemetrics_grid_cell <- function(
    grid_cell,
    layer,
    grid_group_by_col = "POINT_ID",
    layer_group_by_col = "",
    progress = FALSE
) {
  if (inherits(layer, "SpatRaster") | inherits(layer, "RasterLayer")) {
    assertthat::assert_that(sf::st_crs(grid_cell)$wkt == terra::crs(layer))

    landcoverfraction <- function(df) {
      df %>%
        mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
        group_by(!!!syms(grid_group_by_col), value) %>%
        summarize(freq = sum(frac_total), .groups = "drop_last")
    }

    res <- exactextractr::exact_extract(
      x = layer,
      y = grid_cell,
      fun = landcoverfraction,
      summarize_df = TRUE,
      include_cols = grid_group_by_col,
      progress = progress)

    return(res)

  }

  if (inherits(layer, "sf")) {
    assertthat::assert_that(sf::st_crs(grid_cell)$wkt == sf::st_crs(layer)$wkt)

    int <- st_intersection(layer, grid_cell)

    cell_areas <- grid_cell %>%
      select(!!!syms(grid_group_by_col)) %>%
      mutate(cell_area = sf::st_area(geometry)) %>%
      sf::st_drop_geometry()

    temparrow <- tempfile(fileext = ".parquet")

    int$area <- sf::st_area(int$geometry)
    int <- int %>%
      sf::st_drop_geometry() %>%
      inner_join(cell_areas, by = grid_group_by_col) %>%
      arrow::write_dataset(path = temparrow)

    int <- arrow::open_dataset(temparrow) %>%
      arrow::to_duckdb() %>%
      group_by(!!!syms(grid_group_by_col),
               !!!syms(layer_group_by_col),
               cell_area) %>%
      summarise(area_m2 = sum(area)) %>%
      mutate(area_prop = area_m2 / cell_area) %>%
      collect()

    return(int)
  }
}



#' Calculate a canopy height model from a digital surface model and a digital
#' terrain model
#'
#' The digital terrain model is obtained from a web coverage service
calc_chm <- function(dsm,
                     chm_resolution = 1,
                     dtm_resolution = 1,
                     overwrite = FALSE) {
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  assertthat::assert_that(is.wholenumber(dtm_resolution / chm_resolution))
  # prelim check
  destination <- file.path(get_map_procesbeheer(),
                           "PB06_Drone_En_Beelden",
                           "PB_afgeleide_data",
                           "chm")

  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  tifname <- paste0(gsub("DEM", "CHM", dsm@ptr$names),
                    "_res",
                    chm_resolution,
                    ".tif")

  if (file.exists(file.path(destination, tifname)) && !overwrite) {
    chm <- terra::rast(file.path(destination, tifname))
    return(chm)
  }

  # get drone image extent
  ceiling_extent_dsm <- ceiling(ext(dsm))
  ext(dsm) <- ceiling_extent_dsm

  bbox_vec <- as.vector(ceiling_extent_dsm)

  # get dtm image
  dtm_crop <- get_coverage_wcs(
    wcs = "dtm",
    bbox = bbox_vec,
    layername = "EL.GridCoverage.DTM",
    resolution = dtm_resolution)

  # target raster & resolution
  # first disaggregate dtm
  disagg_fact <- round(dtm_resolution / chm_resolution)
  if (disagg_fact > 1) {
    dtm_crop <- terra::disagg(dtm_crop, fact = disagg_fact, method = "bilinear")
  }


  # next resample dsm, this will automaticalle resample to same resolution
  dsm <- terra::resample(dsm, dtm_crop, method = "bilinear")

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

  destination <- file.path(get_map_procesbeheer(),
                           "PB06_Drone_En_Beelden",
                           "PB_afgeleide_data",
                           "ndvi")

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
  cir_raster <- get_coverage_wcs(wcs = "omz",
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
  list_df <- exactextractr::exact_extract(x = raster, y = df, fun = fun,
                                          progress = interactive())

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
    as.data.frame(geom = "WKT") %>%
    sf::st_as_sf(wkt = "geometry",
                 crs = "epsg:31370") %>%
    sf::st_buffer(dist = bufferdist)

  # reclassify raster
  if (!missing(cuts)) {
    raster <- classify(raster, cuts, include.lowest = TRUE)
  }

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
