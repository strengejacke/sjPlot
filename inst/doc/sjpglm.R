## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", dev = "png", fig.width = 7, fig.height = 5, message = FALSE, warning = FALSE)

## ---- results='hide'-----------------------------------------------------
library(sjPlot)
library(sjmisc)
data(efc)
# set basic theme options
set_theme("forest",
          axis.title.size = .85, 
          axis.textsize = .85, 
          legend.size = .8, 
          geom.label.size = 3.5)

## ----results='hide'------------------------------------------------------
# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
# create data frame for fitting model
df <- data.frame(y = to_factor(y),
                 sex = to_factor(efc$c161sex),
                 dep = to_factor(efc$e42dep),
                 barthel = efc$barthtot,
                 education = to_factor(efc$c172code))
# set variable label for response
set_label(df$y) <- "High Negative Impact"
# fit model
fit <- glm(y ~., 
           data = df, 
           family = binomial(link = "logit"))

## ------------------------------------------------------------------------
sjp.glm(fit)

## ------------------------------------------------------------------------
# set variable label for service usage
set_label(efc$tot_sc_e) <- "Total number of services used by carer"

# fit poisson model
fit2 <- glm(tot_sc_e ~ neg_c_7 + e42dep + c161sex,
            data = efc, family = poisson(link = "log"))
# fit incident rate ratios, we need three decimal points to see 
# a difference to the negative binomial model...
sjp.glm(fit2, digits = 3)

# fit negative binomial model as well
library(MASS)
fit3 <- glm.nb(tot_sc_e ~ neg_c_7 + e42dep + c161sex, data = efc)
# fit incident rate ratios
sjp.glm(fit3, digits = 3)

## ------------------------------------------------------------------------
sjp.glm(fit, trns.ticks = FALSE)

## ------------------------------------------------------------------------
sjp.glm(fit, sort.est = FALSE)

## ------------------------------------------------------------------------
sjp.glm(fit, type = "slope")

## ------------------------------------------------------------------------
sjp.glm(fit2, type = "slope")

## ------------------------------------------------------------------------
sjp.glm(fit, type = "slope", facet.grid = FALSE, 
        show.ci = TRUE, vars = "barthel")

## ------------------------------------------------------------------------
# the binary outcome
sjp.glm(fit, type = "eff")

## ------------------------------------------------------------------------
# plot marginal effects, but only for dependency this time
sjp.glm(fit, type = "eff", facet.grid = FALSE, vars = "education")

## ----echo=FALSE, results='hide'------------------------------------------
# set basic theme options
set_theme("forestw",
          axis.title.size = .65, 
          axis.textsize = .7, 
          legend.size = .6, 
          geom.label.size = 3,
          title.size = .7)

## ------------------------------------------------------------------------
# get list of all plots
p <- sjp.glm(fit, type = "eff", facet.grid = FALSE, 
             show.ci = TRUE, prnt.plot = FALSE)$plot.list
# plot all marginal effects, as grid, proper x-axes
# also, set margins for this example
plot_grid(p, margin = c(0.3, 0.3, 0.3, 0.3))

## ------------------------------------------------------------------------
# the binary outcome
sjp.glm(fit, type = "pred", vars = "barthel")

# the count outcome
sjp.glm(fit3, type = "pred", vars = c("neg_c_7", "e42dep"),
        show.ci = TRUE)

# the count outcome, non faceted
sjp.glm(fit2, type = "pred", vars = c("neg_c_7", "e42dep"),
        facet.grid = FALSE)

# the count outcome, grouped gender and education, w/o data points
# and adjusted y-limits, to completely show CI bands
sjp.glm(fit2, type = "pred", vars = c("neg_c_7", "c161sex","e42dep"),
        facet.grid = FALSE, show.ci = TRUE, show.scatter = FALSE,
        axis.lim = c(0, 4))

