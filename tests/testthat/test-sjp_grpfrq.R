if (suppressWarnings(
  require("testthat") &&
  require("sjPlot")
)) {

  context("sjPlot, sjp.grpfrq")

  # glm, logistic regression ----
  data(efc)
  efc$gewicht <- rnorm(nrow(efc), 1, .2)

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
    expect_message(p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", show.grpcnt = TRUE))
    expect_message(p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", intr.var = efc$c172code, show.grpcnt = TRUE))

    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "bar", show.grpcnt = TRUE)

    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "dot")
    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "dot", show.ci = T)
    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "line", show.ci = T)
    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "boxplot", show.ci = T)
    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "violin", show.ci = T)

    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, weight.by = efc$gewicht)
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "dot", weight.by = efc$gewicht)
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "line", weight.by = efc$gewicht)
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "boxplot", weight.by = efc$gewicht)
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "violin", weight.by = efc$gewicht)

    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, legend.title = "Geschlecht", legend.labels = c("M", "W"), axis.titles = "Dependency", axis.labels = c("gar nicht", "leicht", "mittel", "schwer"))
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "dot", legend.title = "Geschlecht", legend.labels = c("M", "W"), axis.titles = "Dependency", axis.labels = c("gar nicht", "leicht", "mittel", "schwer"))
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "line", legend.title = "Geschlecht", legend.labels = c("M", "W"), axis.titles = "Dependency", axis.labels = c("gar nicht", "leicht", "mittel", "schwer"))
    p <- sjp.grpfrq(efc$e42dep, efc$e16sex, type = "boxplot", legend.title = "Geschlecht", legend.labels = c("M", "W"), axis.titles = "Dependency", axis.labels = c("A", "B"))
    p <- sjp.grpfrq(efc$e17age, efc$e16sex, type = "violin", legend.title = "Geschlecht", legend.labels = c("M", "W"), axis.titles = "Dependency", axis.labels = c("A", "B"))
  })
}
