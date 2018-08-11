stopifnot(require("testthat"),
          require("sjPlot"),
          require("lme4"))

context("sjPlot, tab_model type std")

# glm, logistic regression ----
data(sleepstudy)
data(iris)

m1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, REML = F)
m2 <- lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris)

test_that("tab_model, std", {
  p <- tab_model(m1, show.std = "std")
  p <- tab_model(m1, show.std = "std2")
  p <- tab_model(m2, show.std = "std")
  p <- tab_model(m2, show.std = "std2")
})
