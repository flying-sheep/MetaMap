#' Launch App
#' @importFrom shiny runApp
#' @export
launch <- function(...) {
	runApp(pkg_file("shiny"), ...)
}
