# sjPlot 2.0.1-1

## General

* Replace deprecated `dplyr::add_rownames()` with `tibble::rownames_to_column()`.
* Improved title labelling for `type = "pred"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.
* Improved title and facet title labelling for `type = "eff"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.

## Changes to functions

* Added argument `theme.font` to `sjp.setTheme` to apply a base font family for themes.

## Bug fixes

* In some cases, `sjp.int` cropped parts of the plot, when `jitter.ci` was `TRUE`.
* In `sjp.corr`, argument `sort.corr = FALSE` caused an error.
* In `sjt.glm` and `sjt.glmer`, settingargument `sep.column` to `FALSE` still added separator columns at the right end of the table.
