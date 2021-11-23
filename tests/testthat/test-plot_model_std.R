.runThisTest <- Sys.getenv("RunAllsjPlotTests") == "yes"

if (suppressWarnings(
  require("testthat") &&
  require("sjPlot") &&
  require("sjmisc") &&
  require("sjlabelled") &&
  require("haven") &&
  require("lme4")
)) {
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


  if (.runThisTest) {

    if (suppressWarnings(
      require("testthat") &&
      require("rstanarm") &&
      require("sjPlot") &&
      require("lme4")
    )) {

      # fit linear model
      data(sleepstudy)
      sleepstudy$age <- round(runif(nrow(sleepstudy), min = 20, max = 60))
      sleepstudy$Rdicho <- dicho(sleepstudy$Reaction)

      m1 <- stan_glmer(
        Reaction ~ Days + age + (1 | Subject),
        data = sleepstudy, QR = TRUE,
        # this next line is only to keep the example small in size!
        chains = 2, cores = 1, seed = 12345, iter = 500
      )

      m2 <- stan_glmer(
        Rdicho ~ Days + age + (1 | Subject),
        data = sleepstudy, QR = TRUE,
        family = binomial,
        chains = 2, iter = 500
      )

      test_that("plot_model, rstan", {
        p <- plot_model(m1)
        p <- plot_model(m2)
        p <- plot_model(m1, bpe = "mean")
        p <- plot_model(m2, bpe = "mean")
        p <- plot_model(m1, bpe = "mean", bpe.style = "dot")
        p <- plot_model(m2, bpe = "mean", bpe.style = "dot")
        p <- plot_model(m1, bpe = "mean", bpe.style = "line", bpe.color = "green")
        p <- plot_model(m2, bpe = "mean", bpe.style = "line", bpe.color = "green")
        p <- plot_model(m1, bpe = "mean", bpe.style = "line", bpe.color = "green", prob.inner = .4, prob.outer = .8)
        p <- plot_model(m2, bpe = "mean", bpe.style = "line", bpe.color = "green", prob.inner = .4, prob.outer = .8)
        p <- plot_model(m1, bpe = "mean", bpe.style = "line", bpe.color = "green", prob.inner = .4, prob.outer = .8, size.inner = .5)
        p <- plot_model(m2, bpe = "mean", bpe.style = "line", bpe.color = "green", prob.inner = .4, prob.outer = .8, size.inner = .5)
      })
    }
  }
}
