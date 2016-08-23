# sjPlot 2.0.3

## New functions
* `sjp.resid`, an experimental function to plot and analyze residuals from linear models.

## Changes to functions

* For `sjp.glmer` and `sjp.lmer`, argument `show.ci` now also applies for plotting random effects (`type = "re"`, the default), so confidence intervals may not be calculated. This may be useful in some cases where computation of standard errors for random effects caused an error.
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.
* `get_option("p_zero")` was removed, and `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `p.zero` argument.

## Bug fixes

* A bug introduced in update 2.0.2 caused an error in `sjp.lm` for `type = "std"`.


# sjPlot 2.0.2

## General

* Replace deprecated `dplyr::add_rownames()` with `tibble::rownames_to_column()`.
* Improved title labelling for `type = "pred"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.
* Improved title and facet title labelling for `type = "eff"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.

## Changes to functions

* Added argument `theme.font` to `sjp.setTheme` to apply a base font family for themes.
* `sjp.lmer` gets a new plot type `eff.ri` to plot marginal effects, varying by random intercepts.

## Bug fixes

* In some cases, `sjp.int` cropped parts of the plot, when `jitter.ci` was `TRUE`.
* In `sjp.corr`, argument `sort.corr = FALSE` caused an error.
* In `sjt.glm` and `sjt.glmer`, setting argument `sep.column` to `FALSE` still added separator columns at the right end of the table.
* `sjp.xtab` caused an error when a value from `x` was completely missing in `grp` (or vice versa) ([#144](https://github.com/sjPlot/devel/issues/144)).
