get_map_procesbeheer <- function() {
  if (Sys.getenv("MAP_PROCESBEHEER") == "") {
    usethis::edit_r_environ("project")
    stop(paste0("No MAP_PROCESBEHEER environmental variable found, ",
                "please edit the .Renviron file and add a line ",
                "MAP_PROCESBEHEER = '<path to the team drive procesbeheer folder>'"))
  }
  return(Sys.getenv("MAP_PROCESBEHEER"))
}


get_map_inbogis <- function() {
  if (Sys.getenv("MAP_INBOGIS") == "") {
    usethis::edit_r_environ("project")
    stop(paste0("No MAP_INBOGIS environmental variable found, ",
                "please edit the .Renviron file and add a line ",
                "MAP_INBOGIS = '<path to the team drive procesbeheer folder>'"))
  }
  return(Sys.getenv("MAP_INBOGIS"))
}


load_rasterfiles <- function(path) {
  filename <- basename(path)
  filename <- fs::path_ext_remove(filename)
  filename <- make.names(filename)
  assign(filename, terra::rast(path), inherits = TRUE)
}




