context("lme4")

library(lme4)
# create binary response
sleepstudy$Reaction.dicho <- dicho(sleepstudy$Reaction, dich.by = "md")
# fit model
fit <- glmer(Reaction.dicho ~ Days + (Days | Subject),
             sleepstudy,
             family = binomial("logit"))

test_that("glmer", {
  # simple plot
  sjp.glmer(fit)
})

test_that("glmer, sort.coef", {
  # sort by predictor Days
  sjp.glmer(fit, sort.coef = "Days")
})

test_that("glmer, facet", {
  # plot each predictor as own plot and sort them
  sjp.glmer(fit,
            facet.grid = FALSE,
            sort.coef = "sort.all")
})

# create binary response
efc$hi_qol <- dicho(efc$quol_5)
# prepare group variable
efc$grp = as.factor(efc$e15relat)
levels(x = efc$grp) <- get_labels(efc$e15relat)
# data frame for fitted model
mydf <- na.omit(data.frame(hi_qol = as.factor(efc$hi_qol),
                           sex = as.factor(efc$c161sex),
                           c12hour = as.numeric(efc$c12hour),
                           neg_c_7 = as.numeric(efc$neg_c_7),
                           grp = efc$grp))

# fit glmer
fit <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1 | grp),
             data = mydf,
             family = binomial("logit"))

test_that("glmer, efc", {
  # plot random effects
  sjp.glmer(fit)
})

test_that("glmer, efc, lables", {
  # plot random effects
  sjp.glmer(fit, pred.labels = c("spouse", "child", "sibling",
                                 "child in-law", "ancle/aunt",
                                 "nephiew/niece", "cousin"))
})

test_that("glmer, efc, lables", {
  # plot fixed effects
  sjp.glmer(fit, type = "fe", pred.labels = c("gender", "care hours",
                                              "negative impact"))
})

# plot and sort fixed effects
sjp.glmer(fit,
          type = "fe",
          sort.coef = TRUE)

# plot fixed effects correlations
sjp.glmer(fit, type = "fe.cor")

# qq-plot of random effects
sjp.glmer(fit, type = "re.qq")

# plot probability curves (predicted probabilities)
# for each covariate, grouped by random intercepts
sjp.glmer(fit,
          type = "ri.pc",
          show.se = TRUE)

# plot probability curves (predicted probabilities)
# for each covariate, grouped by random intercepts
# in integrated plots
sjp.glmer(fit,
          type = "ri.pc",
          facet.grid = FALSE)

sjp.glmer(fit,
          type = "ri.pc",
          emph.grp = c(1, 4),
          facet.grid = FALSE)

sjp.glmer(fit,
          type = "ri.pc",
          emph.grp = c("child", "cousin"),
          facet.grid = FALSE)

test_that("warn about groups", {
  # expect warning
  expect_warning(sjp.glmer(fit,
    type = "ri.pc",
    emph.grp = c(1, 4),
    facet.grid = T))
})

sjp.glmer(fit,
          type = "ri.pc",
          emph.grp = c(14, 16),
          facet.grid = F)

sjp.glmer(fit,
          type = "ri.pc",
          emph.grp = c("sparse"),
          facet.grid = F)

sjp.glmer(fit,
          type = "ri.pc",
          emph.grp = c("sparse", "child"),
          facet.grid = F)

# plot probability curve (predicted probabilities)
# of fixed effect, only for coefficient "neg_c_7"
sjp.glmer(fit,
          type = "fe.pc",
          vars = "neg_c_7")
