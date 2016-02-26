context("sjp.lmer")

fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# simple plot
test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

# plot fixed effects
test_that("lmer, fe", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe")
})

test_that("lmer, sort re", {
  skip_on_cran()
  # sort by predictor Days
  sjp.lmer(fit, sort.coef = "Days")
})

# plot each predictor as own plot
# sort each plot
test_that("lmer, sort all", {
  skip_on_cran()
  sjp.lmer(fit,
         facet.grid = FALSE,
         sort.coef = "sort.all")
})

# plot and sort fixed effects
test_that("lmer, sort fe", {
  skip_on_cran()
  sjp.lmer(fit,
         type = "fe",
         sort.coef = TRUE)
})


data(efc)
# prepare group variable
efc$grp = as.factor(efc$e15relat)
levels(x = efc$grp) <- get_labels(efc$e15relat)
# data frame for fitted model
mydf <- data.frame(neg_c_7 = as.numeric(efc$neg_c_7),
                   sex = as.factor(efc$c161sex),
                   c12hour = as.numeric(efc$c12hour),
                   barthel = as.numeric(efc$barthtot),
                   grp = efc$grp)
# fit glmer
fit <- lme4::lmer(neg_c_7 ~ sex + c12hour + barthel + (1 | grp),
            data = mydf)

# plot random effects
test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

# plot fixed effects
test_that("lmer, fe", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe")
})

test_that("lmer, std. fe", {
  skip_on_cran()
  sjp.lmer(fit,
         type = "fe.std",
         sort.coef = TRUE)
})

# plot fixed effects slopes for
# each random intercept, but only for
# coefficient "c12hour"
test_that("lmer, fe.ri, vars", {
  skip_on_cran()
  sjp.lmer(fit,
         type = "fe.ri",
         vars = "c12hour")
})

test_that("lmer, fe.ri, emph grp, vars", {
  skip_on_cran()
  expect_message(sjp.lmer(fit,
         type = "fe.ri",
         emph.grp = c(1, 2, 4),
         vars = "c12hour"))
})

test_that("lmer, fe.ri, emph grp, vars", {
  skip_on_cran()
  sjp.lmer(fit,
          type = "fe.ri",
          emph.grp = c(1, 2, 4),
          vars = "c12hour",
          facet.grid = F)
})

# plot fixed effects correlations
test_that("lmer, corr", {
  skip_on_cran()
  expect_warning(sjp.lmer(fit, type = "fe.cor"))
})

# qq-plot of random effects
test_that("lmer, fe.ri, emph grp, vars", {
  skip_on_cran()
  sjp.lmer(fit, type = "re.qq")
})


lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt", 
                       header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

fit <- lme4::lmer(extro ~ open + agree + social + (1 | school), data = lmm.data)

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

test_that("lmer, ri.nr", {
  skip_on_cran()
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
})

test_that("lmer, ri.nr", {
  skip_on_cran()
  # WARNING!
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe")
})

test_that("lmer, fe.ri", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri")
})

test_that("lmer, fe.pred", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.pred")
})


fit <- lme4::lmer(extro ~ open + agree + social + (1 | school) + (1 | class), 
                  data = lmm.data)

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, ri.nr = 2)
})

test_that("lmer", {
  skip_on_cran()
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
})

test_that("lmer, ri.nr", {
  skip_on_cran()
  # WARNING!
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri")
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)
})

fit <- lme4::lmer(extro ~ open + agree + social + (1 | school/class), data = lmm.data)

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, ri.nr = 2)
})

test_that("lmer", {
  skip_on_cran()
  expect_message(sjp.lmer(fit, ri.nr = c(1,5)))
})

test_that("lmer", {
  skip_on_cran()
  # WARNING
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
  sjp.lmer(fit, type = "fe")
})


test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri")
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.std")
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "re.qq")
})

fit <- lme4::lmer(extro ~ open + agree + social + (1 + open | school/class), 
                  data = lmm.data)

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, ri.nr = 2)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, ri.nr = c(1,5))
})

test_that("lmer", {
  skip_on_cran()
  expect_warning(sjp.lmer(fit, ri.nr = c(3,5)))
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe")
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri")
})

test_that("lmer", {
  skip_on_cran()
  expect_message(sjp.lmer(fit, type = "fe.ri", emph.grp = c(3, 5)))
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri", emph.grp = c(3, 5), facet.grid = F)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2)
})

test_that("lmer", {
  skip_on_cran()
  expect_message(sjp.lmer(fit, type = "fe.ri", ri.nr = 2, emph.grp = c(3, 5)))
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "fe.ri", ri.nr = 2, emph.grp = c(3, 5), facet.grid = F)
})

test_that("lmer", {
  skip_on_cran()
  sjp.lmer(fit, type = "re.qq")
})
