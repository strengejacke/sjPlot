.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .66) packageStartupMessage("Utilitiy- and tool-functions have been moved to the 'sjmisc'-package! See \"news(Version == \"1.8\", package = \"sjPlot\")\" for more information.")
}