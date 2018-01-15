#' Launch App
#' @importFrom shiny runApp
#' @export
#'
launch <- function(...) {
	runApp(list(ui = ui, server = server), ...)
}
