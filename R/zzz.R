.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .8) {
    packageStartupMessage("Visit http://strengejacke.de/sjPlot for package-vignettes.")
  } else if (stats::runif(1) > .9) {
    packageStartupMessage("#refugeeswelcome")
  }
}