stopifnot(require("testthat"),
          require("sjPlot"),
          require("lme4"))

context("sjPlot, plot_model type std")

# glm, logistic regression ----
data(sleepstudy)
data(iris)

m1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, REML = F)
m2 <- lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris)

test_that("plot_model, std", {
  p <- plot_model(m1, type = "std")
  p <- plot_model(m1, type = "std2")
  p <- plot_model(m2, type = "std")
  p <- plot_model(m2, type = "std2")
})
