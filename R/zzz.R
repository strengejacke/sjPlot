.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .8) {
    packageStartupMessage("Learn more about sjPlot with 'browseVignettes(\"sjPlot\")'.")
  } else if (stats::runif(1) > .9) {
    packageStartupMessage("#refugeeswelcome")
  }
}
