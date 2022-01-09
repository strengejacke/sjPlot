.runThisTest <- Sys.getenv("RunAllsjPlotTests") == "yes"

if (suppressWarnings(
  require("testthat") &&
  require("sjPlot") &&
  require("sjlabelled") &&
  require("haven") &&
  require("sjmisc") &&
  require("lme4") &&
  require("glmmTMB") &&
  interactive()
)) {

  data(sleepstudy)
  data(Salamanders)
  data(iris)
  data(efc)

  efc <- to_factor(efc, e42dep, c172code, c161sex)

  m1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, REML = F)
  m2 <- lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris)
  m3 <- lm(neg_c_7 ~ e42dep + barthtot + c161sex, data = efc)

  m4 <- glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~ spp + mined,
    family = truncated_nbinom2,
    Salamanders
  )

  test_that("tab_model", {
    p <- tab_model(m1, m2, m3)
  })

  test_that("tab_model", {
    tab_model(m1, m2, m3, m4)
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

      test_that("tab_model, rstan", {
        p <- tab_model(m1)
        p <- tab_model(m2)
        p <- tab_model(m1, m2)
        p <- tab_model(m1, m2, show.ci50 = FALSE)
        p <- tab_model(m1, m2, col.order = c("ci.outer", "ci.inner", "est"))
        p <- tab_model(m1, m2, bpe = "mean")
      })
    }
  }
}
