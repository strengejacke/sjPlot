## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", dev = "png", fig.width = 7, fig.height = 5, message = FALSE, warning = FALSE)

## ---- message=FALSE, warning=FALSE---------------------------------------
# load packages
library(sjPlot)
library(sjmisc)
# load sample data set.
data(efc)
set_theme(theme = "forest", 
          geom.label.size = 3, 
          axis.textsize = .9, 
          axis.title.size = .9)

## ----results='hide', message=FALSE---------------------------------------
# fit model
library(lme4)
fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# prepare group variable
efc$grp = as.factor(efc$e15relat)
levels(x = efc$grp) <- get_labels(efc$e15relat)
# data frame for fitted model
mydf <- data.frame(neg_c_7 = efc$neg_c_7,
                   sex = to_factor(efc$c161sex),
                   c12hour = efc$c12hour,
                   barthel = efc$barthtot,
                   grp = efc$grp)
# fit 2nd model
fit2 <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1 | grp), data = mydf)

## ------------------------------------------------------------------------
sjp.lmer(fit, y.offset = .4)

## ------------------------------------------------------------------------
# sort by predictor Days
sjp.lmer(fit, sort.est = "Days", y.offset = .4)

## ------------------------------------------------------------------------
# sort all predictors
sjp.lmer(fit,
         facet.grid = FALSE,
         sort.est = "sort.all",
         y.offset = .4)

## ------------------------------------------------------------------------
# random intercepts
ranef(fit2)
# fixed effects
fixef(fit2)
# plot fixed effects depending on group levels
sjp.lmer(fit2, vars = "c12hour", type = "ri.slope")

## ------------------------------------------------------------------------
# plot fixed effects depending on group levels
# emphasize group levels 1, 2 and 5
sjp.lmer(fit2, 
         type = "ri.slope", 
         vars = "c12hour", 
         emph.grp = c(1, 2, 5), 
         facet.grid = FALSE)

## ------------------------------------------------------------------------
# plot random-slope-intercept
sjp.lmer(fit, type = "rs.ri", vars = "c12hour", sample.n = 15)

## ------------------------------------------------------------------------
# plot random-slope-intercept, plot subjects 1, 5 and 7.
sjp.lmer(fit, type = "rs.ri", 
         sample.n = c(1, 5, 7),
         show.legend = TRUE)

## ------------------------------------------------------------------------
# plot qq-plot of random effects
sjp.lmer(fit2, type = "re.qq")

## ------------------------------------------------------------------------
# plot qq-plot of random effects
sjp.lmer(fit, type = "re.qq")

