context("sjt.glm")

CSS = list(css.table = '+border-collapse:separate;',
           css.tdata = 'border: 1px solid red;',
           css.tgrpdata = 'border: 1px solid blue;',
           css.grouprow = 'border: 1px solid blue;',
           css.separatorcol = 'border: 1px solid green;')

# prepare dummy variables for binary logistic regression
y1 <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
y2 <- ifelse(swiss$Infant.Mortality < median(swiss$Infant.Mortality), 0, 1)
y3 <- ifelse(swiss$Agriculture < median(swiss$Agriculture), 0, 1)

# Now fit the models. Note that both models share the same predictors
# and only differ in their dependent variable (y1, y2 and y3)
fitOR1 <- glm(y1 ~ swiss$Education + swiss$Examination + swiss$Catholic,
              family = binomial(link = "logit"))
fitOR2 <- glm(y2 ~ swiss$Education + swiss$Examination + swiss$Catholic,
              family = binomial(link = "logit"))
fitOR3 <- glm(y3 ~ swiss$Education + swiss$Examination + swiss$Catholic,
              family = binomial(link = "logit"))

test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fitOR1,
          fitOR2,
          CSS = CSS,
          useViewer = F,
          labelDependentVariables = c("Fertility",
                                      "Infant Mortality"),
          labelPredictors = c("Education",
                              "Examination",
                              "Catholic"))
})

test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fitOR1, fitOR2, fitOR3,
          useViewer = F,
          CSS = CSS,
          labelDependentVariables = c("Fertility",
                                      "Infant Mortality",
                                      "Agriculture"),
          labelPredictors = c("Education", "Examination", "Catholic"),
          separateConfColumn = FALSE)
})

test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fitOR1, fitOR2, fitOR3,
          CSS = CSS,
          useViewer = F,
          labelDependentVariables = c("Fertility",
                                      "Infant Mortality",
                                      "Agriculture"),
          labelPredictors = c("Education", "Examination", "Catholic"))
})


# --------------------------------------------
# User defined style sheet
# --------------------------------------------
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fitOR1, fitOR2, fitOR3,
          labelDependentVariables = c("Fertility",
                                      "Infant Mortality",
                                      "Agriculture"),
          labelPredictors = c("Education", "Examination", "Catholic"),
          showHeaderStrings = TRUE,
          useViewer = F,
          CSS = list(css.table = "border: 2px solid;",
                     css.tdata = "border: 1px solid;",
                     css.depvarhead = "color:#003399;"))
})


# load efc sample data
data(efc)
# dichtomozize service usage by "service usage yes/no"
efc$services <- sjmisc::dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# fit 3 models with different link-functions
fit1 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc,
            family = binomial(link = "logit"))
fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc,
            family = binomial(link = "probit"))
fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc,
            family = poisson(link = "log"))

# compare models
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit1, fit2, fit3,
        CSS = CSS,
        useViewer = F,
        showAIC = TRUE,
        showFamily = TRUE,
        showPseudoR = FALSE)
})


# --------------------------------------------
# Change style of p-values and CI-appearance
# --------------------------------------------
# open HTML-table in RStudio Viewer Pane or web browser,
# table indicating p-values as stars
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit1, fit2, fit3,
        CSS = CSS,
        useViewer = F,
        pvaluesAsNumbers = FALSE,
        showAIC = TRUE,
        showFamily = TRUE,
        showPseudoR = FALSE)
})

# open HTML-table in RStudio Viewer Pane or web browser,
# indicating p-values as stars and integrate CI in OR column
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit1, fit2, fit3,
        CSS = CSS,
        useViewer = F,
        pvaluesAsNumbers = FALSE,
        separateConfColumn = FALSE,
        showAIC = TRUE,
        showFamily = TRUE,
        showPseudoR = FALSE)
})

# ----------------------------------
# automatic grouping of predictors
# ----------------------------------
# load efc sample data
data(efc)
# dichtomozize service usage by "service usage yes/no"
efc$services <- sjmisc::dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
# make dependency categorical
efc$e42dep <- to_factor(efc$e42dep)
# fit model with "grouped" predictor
fit <- glm(services ~ neg_c_7 + c161sex + e42dep, data = efc)

# automatic grouping of categorical predictors
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit,
        CSS = CSS,
        useViewer = F)
})

# ----------------------------------
# compare models with different predictors
# ----------------------------------
fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep + c12hour, data = efc)
fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep + c12hour + c172code, data = efc)

# print models with different predictors
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit, fit2, fit3,
        CSS = CSS,
        useViewer = F)
})

efc$c172code <- to_factor(efc$c172code)
fit2 <- glm(services ~ neg_c_7 + c161sex + c12hour, data = efc)
fit3 <- glm(services ~ neg_c_7 + c161sex + c172code, data = efc)

# print models with different predictors
test_that("sjt.glm", {
  skip_on_cran()
  sjt.glm(fit, fit2, fit3, group.pred = FALSE,
        CSS = CSS,
        useViewer = F)
})
