test_that("sjp.lmer", { 
  skip_on_cran()

  library(lme4)
  library(sjmisc)
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  
  # simple plot
  sjp.lmer(fit)
  
  # plot fixed effects
  sjp.lmer(fit, type = "fe")
  
  # sort by predictor Days
  sjp.lmer(fit, sort.coef = "Days")
  
  # plot each predictor as own plot
  # sort each plot
  sjp.lmer(fit,
           facet.grid = FALSE,
           sort.coef = "sort.all")
  
  # plot and sort fixed effects
  sjp.lmer(fit,
           type = "fe",
           sort.coef = TRUE)
  
  
  library(lme4)
  library(sjmisc)
  data(efc)
  # prepare group variable
  efc$grp = as.factor(efc$e15relat)
  levels(x = efc$grp) <- get_val_labels(efc$e15relat)
  # data frame for fitted model
  mydf <- data.frame(neg_c_7 = as.numeric(efc$neg_c_7),
                     sex = as.factor(efc$c161sex),
                     c12hour = as.numeric(efc$c12hour),
                     barthel = as.numeric(efc$barthtot),
                     grp = efc$grp)
  # fit glmer
  fit <- lme4::lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp),
              data = mydf)
  
  # plot random effects
  sjp.lmer(fit)
  
  # plot fixed effects
  sjp.lmer(fit, type = "fe")
  
  sjp.lmer(fit,
           type = "fe.std",
           sort.coef = TRUE)
  
  # plot fixed effects slopes for
  # each random intercept, but only for
  # coefficient "c12hour"
  sjp.lmer(fit,
           type = "fe.ri",
           vars = "c12hour")
  
  sjp.lmer(fit,
           type = "fe.ri",
           emph.grp = c(1, 2, 4),
           vars = "c12hour")
  
  # plot fixed effects correlations
  sjp.lmer(fit, type = "fe.cor")
  
  # qq-plot of random effects
  sjp.lmer(fit, type = "re.qq")

  
  lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt", 
                         header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)
  
  fit <- lme4::lmer(extro ~ open + agree + social + (1 | school), data = lmm.data)
  sjp.lmer(fit)
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
  sjp.lmer(fit, type = "fe")
  sjp.lmer(fit, type = "fe.ri")
  sjp.lmer(fit, type = "fe.pred")
  
  
  fit <- lme4::lmer(extro ~ open + agree + social + (1 | school) + (1 | class), 
                    data = lmm.data)

  sjp.lmer(fit)
  sjp.lmer(fit, ri.nr = 2)
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
  sjp.lmer(fit, type = "fe.ri")
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)
  
  fit <- lme4::lmer(extro ~ open + agree + social + (1 | school/class), data = lmm.data)

  sjp.lmer(fit)
  sjp.lmer(fit, ri.nr = 2)
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
  sjp.lmer(fit, type = "fe")
  sjp.lmer(fit, type = "fe.ri")
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)

  sjp.lmer(fit, type = "fe.std")
  sjp.lmer(fit, type = "re.qq")
  
  fit <- lme4::lmer(extro ~ open + agree + social + (1 + open | school/class), 
                    data = lmm.data)
  
  sjp.lmer(fit)
  sjp.lmer(fit, ri.nr = 2)
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
  sjp.lmer(fit, type = "fe")
  sjp.lmer(fit, type = "fe.ri")
  sjp.lmer(fit, type = "fe.ri", emph.grp = c(3, 5))
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2, emph.grp = c(3, 5))
  
  sjp.lmer(fit, type = "re.qq")
})