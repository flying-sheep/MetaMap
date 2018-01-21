#' @title create App dir
#'
#' Creates a directory called shiny in \code{location}.
#'     You can then run the app by just typing \code{runApp("location")}
#'
#' @param location
#' @export
createAppDir <- function(location){
  if(!file.exists(pkg_file("shiny/www")))
    stop(paste("The directory",pkg_file("shiny/www"),"is missing! Please reinstall the package!"))
  location <- file.path(location, "shiny")
  dir.create(location)
  www_location <- file.path(location, "www")
  dir.create(www_location)
  if (all(file.copy(file.path(pkg_file("shiny"), c("server.r", "ui.r")), location),
          file.copy(file.path(
            pkg_file("shiny/www"), list.files(pkg_file("shiny/www"))
          ), www_location)))
    message(paste0("Created ", location, "!"))
  else{
    message("An error has occured!")
  }
}
