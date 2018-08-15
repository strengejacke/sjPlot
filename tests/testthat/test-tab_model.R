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

test_that("tab_model", {
  p <- tab_model(m1, m2, m3)
})

test_that("tab_model, check shows", {
  p <- tab_model(m1, m2, m3, show.intercept = FALSE, show.fstat = TRUE, show.se = TRUE)
  p <- tab_model(m1, m2, m3, show.intercept = FALSE, show.fstat = TRUE, show.se = TRUE, show.ci = F, show.df = TRUE, p.val = "kr")
})

test_that("tab_model, check terms", {
  p <- tab_model(m1, m2, m3, show.intercept = FALSE, show.fstat = TRUE, show.se = TRUE, terms = c("Days", "Sepal.Width", "c161sex2", "barthtot"))
  p <- tab_model(m1, m2, m3, show.intercept = FALSE, show.fstat = TRUE, show.se = TRUE, show.ci = F, show.df = TRUE, p.val = "kr", rm.terms = c("Days", "Sepal.Width", "c161sex2", "barthtot"))
  p <- tab_model(m1, m2, m3, show.intercept = FALSE, show.fstat = TRUE, show.se = TRUE, show.ci = F, show.df = TRUE, rm.terms = c("Sepal.Width", "c161sex2", "barthtot"))
})

test_that("tab_model, std", {
  p <- tab_model(m1, show.std = "std")
  p <- tab_model(m1, show.std = "std2")
  p <- tab_model(m2, show.std = "std")
  p <- tab_model(m2, show.std = "std2")
  p <- tab_model(m1, m2, show.std = "std")
  p <- tab_model(m1, m2, m3, show.std = "std")
  p <- tab_model(m1, m2, m3, show.std = "std2")
})
