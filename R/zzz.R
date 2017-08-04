.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .8) {
    packageStartupMessage("Learn more about sjPlot with 'browseVignettes(\"sjPlot\")'.")
  } else if (stats::runif(1) > .8) {
    packageStartupMessage("Install package \"strengejacke\" from GitHub (`devtools::install_github(\"strengejacke/strengejacke\")`) to load all sj-packages at once!")
  } else if (stats::runif(1) > .8) {
    packageStartupMessage("#refugeeswelcome")
  }
}
