library(testthat)
library(sjPlot)

if (length(strsplit(packageDescription("sjPlot")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllsjPlotTests" = "yes")
}

test_check("sjPlot")
