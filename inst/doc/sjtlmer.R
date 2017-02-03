## ---- results='hide', message=FALSE, warning=FALSE-----------------------
# load required packages
library(sjPlot)
library(sjmisc)
library(lme4)

## ---- results='hide'-----------------------------------------------------
# load sample data
data(efc)
# prepare grouping variables
efc$grp = as.factor(efc$e15relat)
levels(x = efc$grp) <- get_labels(efc$e15relat)
efc$care.level <- rec(efc$n4pstu, recodes = "0=0;1=1;2=2;3:4=4", as.num = FALSE)
levels(x = efc$care.level) <- c("none", "I", "II", "III")

# data frame for fitted model
mydf <- data.frame(neg_c_7 = efc$neg_c_7,
                   sex = to_factor(efc$c161sex),
                   c12hour = efc$c12hour,
                   barthel = efc$barthtot,
                   education = to_factor(efc$c172code),
                   grp = efc$grp,
                   carelevel = efc$care.level)

# fit sample models
fit1 <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1 | grp), data = mydf)
fit2 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel + (1 | grp), data = mydf)
fit3 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel +
              (1 | grp) + (1 | carelevel), data = mydf)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2,
#           depvar.labels = c("Negative Impact", "Negative Impact"))

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, show.header = TRUE, string.est = "Estimate",
#           string.ci = "Conf. Int.", string.p = "p-value",
#           string.dv = "Response", string.pred = "Coefficients",
#           string.interc = "Konstante",
#           depvar.labels = c("Negative Impact", "Negative Impact"))

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2,
#           separate.ci.col = FALSE, # ci in same cell as estimates
#           show.std = TRUE,         # also show standardized beta values
#           p.numeric = FALSE,       # "*" instead of numeric values
#           show.re.var = FALSE,     # no random effect variances
#           show.aic = TRUE,         # AIC
#           show.dev = FALSE,        # no deviance
#           show.r2 = FALSE)          # no Pseudo-R2

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, pred.labels = c("Carer's Sex",
#           "Hours of Care", "Elder's Dependency",
#           "Mid Educational Level", "High Educational Level"))

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit3, fit2, fit1, group.pred = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, fit3)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, fit3,
#           CSS = list(css.separatorcol = 'padding-right:1.5em; padding-left:1.5em;'),
#           show.re.var = FALSE,
#           show.icc = FALSE,
#           show.r2 = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, fit3,
#           show.re.var = FALSE,
#           show.icc = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, fit3,
#           remove.estimates = 2,
#           show.re.var = FALSE,
#           show.icc = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  sjt.lmer(fit1, fit2, fit3,
#           remove.estimates = c("c12hour", "sex2"),
#           show.re.var = FALSE,
#           show.icc = FALSE)

