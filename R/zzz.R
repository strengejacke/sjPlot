.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .8) {
    packageStartupMessage("Visit http://strengejacke.de/sjPlot for illustrative examples of sjPlot-functions.")
  }
}