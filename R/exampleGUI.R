#' A Shiny-based GUI for the rs_IDs function
#'
#' Launches a (possibly buggy) Shiny app that acts as a graphical user interface for the \code{\link{rs_IDs}} function. It's a bit hacky, so its performance is not guaranteed.
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @seealso \code{\link{rs_IDs}}, \href{../doc/loosebirdtag.html}{vignette}.
#' @examples
#' \dontrun{
#' exampleGUI()  #yeah, just run it.
#' }
#' @export
#' @importFrom shiny runApp
#'
exampleGUI <- function() {
  appDir <- system.file("gui-example", package = "rabi")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `rabi`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
