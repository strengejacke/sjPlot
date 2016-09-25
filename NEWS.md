# sjPlot 2.0.3

## New functions
* `sjp.resid`, an experimental function to plot and analyze residuals from linear models.
* `plot_grid` to plot a list of ggplot-objects as arranged grid in a single plot.

## Changes to functions

* For `sjp.glmer` and `sjp.lmer`, argument `show.ci` now also applies for plotting random effects (`type = "re"`, the default), so confidence intervals may not be calculated. This may be useful in some cases where computation of standard errors for random effects caused an error.
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.
* `get_option("p_zero")` was removed, and `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `p.zero` argument.

## Bug fixes

* A bug introduced in update 2.0.2 caused an error in `sjp.lm` for `type = "std"`.
* Effect plots (`type = "eff"`) for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` did not plot all predictors, when predictor name was not exactly specified in formula, but transformed inside formula (e.g. `log(pred + 1)`).
