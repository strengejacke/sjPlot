.onLoad <- function(libname, pkgname) {
  if (!require(sjmisc)) {
    warning("Package 'sjmisc' required to use sjPlot! Please install it...", call. = F)
  }
}
