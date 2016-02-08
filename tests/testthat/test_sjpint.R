context("sjp.int")

fit <- lm(weight ~ Diet * Time, data = ChickWeight)
# show summary to see significant interactions
summary(fit)

test_that("effect-plot", {
  skip_on_cran()
  # plot regression line of interaction terms
  sjp.int(fit, type = "eff")
})

test_that("effect-plot", {
  skip_on_cran()
  # plot regression line of interaction terms, including value labels
  sjp.int(fit, type = "eff", showValueLabels = TRUE)
})


data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(usage = efc$tot_sc_e,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   burden = efc$neg_c_7,
                   dependency = efc$e42dep)
# convert gender predictor to factor
mydf$sex <- relevel(factor(mydf$sex), ref = "2")
# fit "dummy" model
fit <- lm(usage ~ .*., data = mydf)

test_that("cond. effect-plot", {
  skip_on_cran()
  # plot interactions
  sjp.int(fit, type = "cond")
})

test_that("cond. effect-plot, mean-sd", {
  skip_on_cran()
  # plot interactions, using mean and sd as moderator
  # values to calculate interaction effect
  sjp.int(fit, type = "cond", moderatorValues = "meansd")
})

test_that("cond. effect-plot, zeromax", {
  skip_on_cran()
  # use zero and maximum value of moderation effect
  sjp.int(fit, type = "cond", moderatorValues = "zeromax")
})


mydf <- data.frame(usage = efc$tot_sc_e,
                   sex = to_factor(efc$c161sex),
                   education = to_factor(efc$c172code),
                   burden = efc$neg_c_7,
                   dependency = efc$e42dep)
# convert gender predictor to factor
mydf$sex <- relevel(factor(mydf$sex), ref = "2")
# fit "dummy" model
fit <- lm(usage ~ sex * education, data = mydf)

test_that("effect-plot, mean-sd", {
  skip_on_cran()
  # use zero and maximum value of moderation effect
  sjp.int(fit, type = "eff", moderatorValues = "meansd")
})

test_that("effect-plot, quart", {
  skip_on_cran()
  # use zero and maximum value of moderation effect
  sjp.int(fit, type = "eff", moderatorValues = "quart")
})

test_that("cond. effect-plot, quart", {
  skip_on_cran()
  # use zero and maximum value of moderation effect
  sjp.int(fit, type = "cond", moderatorValues = "quart", plevel = 1)
})


# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
# create data frame for fitted model
mydf <- data.frame(y = as.factor(y),
                   sex = as.factor(efc$c161sex),
                   barthel = as.numeric(efc$barthtot))
# fit model
fit <- glm(y ~ sex * barthel,
           data = mydf,
           family = binomial(link = "logit"))

test_that("cond. effect-plot", {
  skip_on_cran()
  # plot interaction, increase p-level sensivity
  sjp.int(fit,
          type = "cond",
          legendLabels = get_labels(efc$c161sex),
          plevel = 0.1)
})

test_that("effect-plot, CI", {
  skip_on_cran()
  sjp.int(fit, type = "eff", showCI = T)
})

test_that("effect-plot, CI, facet", {
  skip_on_cran()
  sjp.int(fit, type = "eff", showCI = T, facet.grid = T)  
})

data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(burden = efc$neg_c_7,
                   sex = efc$c161sex,
                   education = efc$c172code)
# convert gender predictor to factor
mydf$sex <- factor(mydf$sex)
mydf$education <- factor(mydf$education)
# name factor levels and dependent variable
levels(mydf$sex) <- c("female", "male")
levels(mydf$education) <- c("low", "mid", "high")
mydf$burden <- set_label(mydf$burden, "care burden")
# fit "dummy" model
fit <- lm(burden ~ .*., data = mydf)

# plot marginal means of interactions, no interaction found
test_that("emm", {
  skip_on_cran()
  expect_warning(sjp.int(fit, type = "emm"))
})

test_that("emm", {
  skip_on_cran()
  # plot marginal means of interactions, including those with p-value up to 1
  sjp.int(fit, type = "emm", plevel = 1)
})

test_that("emm", {
  skip_on_cran()
  # swap predictors
  sjp.int(fit,
          type = "emm",
          plevel = 1,
          swapPredictors = TRUE)  
})

test_that("emm", {
  skip_on_cran()
  sjp.int(fit,
          type = "emm",
          plevel = 1,
          facet.grid = T,
          showCI = TRUE,
          swapPredictors = TRUE)  
})

mydf$barthel <- efc$barthtot
# re-fit model with continuous variable
fit <- lm(burden ~ .*., data = mydf)

test_that("eff", {
  skip_on_cran()
  # plot effects
  sjp.int(fit, type = "eff", showCI = TRUE)
})

test_that("emm, plot.index", {
  skip_on_cran()
  # plot effects, faceted
  sjp.int(fit,
          type = "eff",
          int.plot.index = 3,
          showCI = TRUE,
          facet.grid = TRUE)
})


fit <- lm(neg_c_7 ~ c12hour + barthtot + c12hour:barthtot, data = efc)

test_that("effect", {
  skip_on_cran()
  sjp.int(fit, type = "eff")
})

test_that("effect, zeromax", {
  skip_on_cran()
  sjp.int(fit, type = "eff", moderatorValues = "zeromax")
})

test_that("effect, meansd, CI", {
  skip_on_cran()
  sjp.int(fit, type = "eff", moderatorValues = "meansd", showCI = T)
})

test_that("effect, meansd, CI, facet", {
  skip_on_cran()
  sjp.int(fit, type = "eff", moderatorValues = "meansd", showCI = T, facet.grid = T)
})

test_that("effect, quart", {
  skip_on_cran()
  sjp.int(fit, type = "eff", moderatorValues = "quart", showCI = T)
})

test_that("cond. effect, quart, CI", {
  skip_on_cran()
  sjp.int(fit, type = "cond", moderatorValues = "quart", showCI = T)
})

test_that("cond. effect. plot index", {
  skip_on_cran()
  sjp.int(fit, type = "cond", int.plot.index = 3, showCI = TRUE, facet.grid = TRUE)
})


# test mixed models
library(lme4)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(usage = efc$tot_sc_e,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   burden = efc$neg_c_7,
                   dependency = efc$e42dep,
                   randomeff = rep(1:10,length.out = nrow(efc)))
# convert gender predictor to factor
mydf$sexf <- relevel(factor(mydf$sex), ref = "2")
mydf$sexb <- mydf$sex == 2
# fit "dummy" model
fit <- lme4::lmer(usage ~ sex*burden + (1 | randomeff), data = mydf)

test_that("cond. effect, lmer", {
  skip_on_cran()
  sjp.int(fit, type = "cond", plevel = 1)
})

# test_that("effect, lmer", {
#   skip_on_cran()
#   sjp.int(fit, type = "eff")
# })

fit <- lme4::lmer(usage ~ sexf*burden + (1 | randomeff), data = mydf)

test_that("cond. effect, lmer, sex factor", {
  skip_on_cran()
  sjp.int(fit, type = "cond", plevel = 1)
})

# test_that("effect, lmer, sex factor", {
#   skip_on_cran()
#   sjp.int(fit, type = "eff")
# })

# this one breaks
fit <- lme4::lmer(usage ~ sexb * burden + (1 | randomeff), data = mydf)

test_that("cond. effect, lmer, sex boolean", {
  skip_on_cran()
  sjp.int(fit, type = "cond", plevel = 1)
})

test_that("effect, lmer, sex boolean", {
  skip_on_cran()
  # message("Error incoming!")
  expect_error(sjp.int(fit, type = "eff"))
})

fit <- lm(usage ~ sexb * burden * education, data = mydf)

test_that("cond. effect, sex boolean", {
  skip_on_cran()
  sjp.int(fit, type = "cond", plevel = 1)
})

test_that("effect, sex boolean", {
  skip_on_cran()
  # message("Error incoming!")
  expect_error(sjp.int(fit, type = "eff"))
})

# create data frame with variables that should be included
# in the model
mydf <- data.frame(burden = efc$neg_c_7,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   groups = efc$e15relat)
# convert gender predictor to factor
mydf$sex <- factor(mydf$sex)
mydf$education <- factor(mydf$education)
mydf$groups <- factor(mydf$groups)
# name factor levels and dependent variable
levels(mydf$sex) <- c("female", "male")
levels(mydf$education) <- c("low", "mid", "high")
mydf$burden <- set_label(mydf$burden, "care burden")
# fit "dummy" model
fit <- lme4::lmer(burden ~ education*sex + (1 | groups), data = mydf)

test_that("emm, lmer", {
  skip_on_cran()
  expect_warning(sjp.int(fit, type = "emm"))
})

test_that("emm, lmer", {
  skip_on_cran()
  sjp.int(fit, type = "emm", plevel = 1)
})

library(lmerTest)
fit <- lmerTest::lmer(burden ~ sex + education + sex:education + (1 | groups), data = mydf)

test_that("emm, lmer", {
  skip_on_cran()
  expect_warning(sjp.int(fit, type = "emm", plevel = 1))
})

test_that("emm, lmer", {
  skip_on_cran()
  expect_warning(sjp.int(fit, type = "emm", plevel = 1, showCI = T))
})

test_that("emm, lmer", {
  skip_on_cran()
  expect_warning(sjp.int(fit, type = "emm", plevel = 1, showCI = T, facet.grid = T))
})
