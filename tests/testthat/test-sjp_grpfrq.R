stopifnot(require("testthat"),
          require("sjPlot"))

context("sjPlot, sjp.grpfrq")

# glm, logistic regression ----
data(efc)

test_that("sjp.grpfrq", {
  p <- sjp.grpfrq(efc$e17age, efc$e16sex)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "dot")
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "line")
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot")
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "violin")

  p <- sjp.grpfrq(efc$e17age, efc$e16sex, bar.pos = "stack")

  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", intr.var = efc$c172code)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "violin", intr.var = efc$c172code)

  p <- sjp.grpfrq(efc$e17age, efc$e16sex, show.values = FALSE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, show.values = FALSE, show.n = TRUE)

  p <- sjp.grpfrq(efc$e17age, efc$e16sex, show.values = TRUE, show.n = TRUE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "dot", show.values = TRUE, show.n = TRUE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, show.values = TRUE, show.n = TRUE, show.prc = TRUE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "dot", show.values = TRUE, show.n = TRUE, show.prc = TRUE)

  p <- sjp.grpfrq(efc$e17age, efc$e16sex, show.grpcnt = TRUE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", show.grpcnt = TRUE)
  p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", intr.var = efc$c172code, show.grpcnt = TRUE)

  p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "bar", show.grpcnt = TRUE)
})
