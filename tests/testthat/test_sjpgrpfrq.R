context("sjp.grpfrq")

test_that("Grpd Frq", {
  skip_on_cran()
  sjp.grpfrq(efc$e42dep, efc$e16sex)
})

test_that("Grpd Violin", {
  skip_on_cran()
  sjp.grpfrq(efc$c12hour, 
             efc$e16sex,
             interactionVar = efc$c172code,
             type = "v",
             innerBoxPlotWidth = .05,
             innerBoxPlotDotSize = 2)
})

test_that("Grpd Lines", {
  skip_on_cran()
  sjp.grpfrq(efc$e17age, 
             efc$e16sex,
             type = "lines",
             smoothLines = T,
             showValueLabels = F)
})
