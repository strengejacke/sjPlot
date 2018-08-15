stopifnot(require("testthat"),
          require("sjPlot"),
          require("sjmisc"),
          require("lme4"))

context("sjPlot, tab_model type std")

data(sleepstudy)
data(iris)
data(efc)

efc <- to_factor(efc, e42dep, c172code, c161sex)

m1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, REML = F)
m2 <- lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris)
m3 <- lm(neg_c_7 ~ e42dep + barthtot + c161sex, data = efc)

test_that("plot_model", {
  p <- plot_model(m1)
  p <- plot_model(m2)
  p <- plot_model(m3)

  p <- plot_model(m1, type = "slope")
  p <- plot_model(m2, type = "slope")
  p <- plot_model(m3, type = "slope")

  p <- plot_model(m1, type = "resid")
  p <- plot_model(m2, type = "resid")
  p <- plot_model(m3, type = "resid")
})


test_that("plot_model, std", {
  p <- plot_model(m1, type = "std")
  p <- plot_model(m1, type = "std2")
  p <- plot_model(m2, type = "std")
  p <- plot_model(m2, type = "std2")
  p <- plot_model(m3, type = "std")
  p <- plot_model(m3, type = "std2")
})

