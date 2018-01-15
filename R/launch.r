#' Launch App
#' @importFrom shiny runApp
#' @export
launch <- function() {
	runApp(system.file("shiny", package = "metatranscriptome"),
	       display.mode = "normal",
	       launch.browser = TRUE)
}
