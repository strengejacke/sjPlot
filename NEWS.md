# sjPlot 2.3.1

## General

* All `sjt`-functions can now be directly integrated into knitr-code-chunks, because sjPlot exports a knitr-print-method (see `vignette("sjtbasic", "sjPlot")`).
* `sjtab()` now also works within knitr-documents (see `vignette("sjtbasic", "sjPlot")`).
* Updated Namespace for functions that moved from package **sjstats** to **sjmisc**.

## Changes to functions

* Changed defaults for `save_plot()`.
* `save_plot()` now also supports _svg_-format.
* For effect-plots (`type = "eff"`), the `axis.title`-argument can now be used to change the title of y-axes.
* For `sjp.lm()`, `sjp.glm()`, `sjp.lmer()` and `sjp.glmer()`, if color palette has more values than needed, it is silently shortend to the required length.
* When plotting mixed models, argument `geom.colors` now also applies to plot-type `type = "ri.slope"`.
* Default correlation-method for `sjt.corr()` and `sjp.corr()` is now `pearson`.
* Argument `emph.p` for printing tables of regression models now defaults to `FALSE`.

## Bug fixes

* Fixed bug in `sjt.frq()` for variables with many missing values and labelled values that did not occur on that variable.
* Argument `value.labels` had no effect for `sjt.frq()`.
* Automatic label detection in `sjt.grpmean()` sometimes not worked for factors without variable labels.
* `sjp.glm()` used _Odds Ratios_ as default title for y-axis when plotting marginal effects. Fixed, now y-axis is correctly labelled.
* `sjt.glm()` used "Odds Ratios" as default column heading for the estimates, even for poisson or other models. Now the string for column headers is selected based on the first model input of the function.
* Solved issue with warning in prediction-plots (`type = "pred"`) for categorical variables on the x-axis.

# sjPlot 2.3.0

## General

* Vignettes were added to this package.

## Changes to functions

* You can use `geom.colors = "bw"` for linetype-plots, to create black & white figures that use different linetypes instead of different colors.
* `sjp.kfold_cv()` now also supports poisson and negative binomial regression models.
* `sjp.pca()` and `sjt.pca()` get a `rotation`-argument, to use either varimax- or oblimin-transformation of factor loadings.
* Argument `show.value` now also applies to bar plots in `sjp.pca()`. 
* `sjt.glm()`, for generalized linar (mixed) models, now shows adjusted standard errors, using the Taylor series-based delta method. 
* More precise rounding of percentage values in `sjt.xtab()`, `sjp.xtab()` and `sjp.grpfrq()`.
* Cramer's V in `sjt.xtab()` is now dentoted as _V_.
* `sjt.xtab()` gets a `...`-argument, to pass down further arguments to the test statistics functions `chisq.test()` and `fisher.test()`.
* `sjt.xtab()` gets a `statistics`-argument, to select one of different measures of associations for the table summary.

## Bug fixes

* Plotting or table output of regression models did not work with null-models (i.e. with intercept only).

# sjPlot 2.2.1

## Changes to functions

* `sjp.lm()` for `type = "ma"` now uses subtitles in multi-line plot-titles.

## Bug fixes

* Residuals in `sjp.kfold_cv()` had wrong leading sign (i.e. positive residuals were negative and vice versa).

# sjPlot 2.2.0

## New Functions

* `sjp.kfold_cv()` to plot model fit from k-fold cross-validation.

## Changes to functions

* Argument `scatter.plot` was renamed to `show.scatter`.
* Argument `var.labels` in `sjt.frq()` was renamed to `title`.
* `sjplot()` and `sjtab()` also accept grouped data frames, to create plots or tables for all subgroups.
* For `sjp.glm()` and `sjp.glmer()`, `type = "pred"`, `type = "slope"`, `type = "pred.fe"` and `type = "fe.slope"` can now also plot data points when `show.scatter = TRUE`. Use `point.alpha` to adjust alpha-level of data points.
* For `sjp.lm()`, `sjp.lmer()`, `sjp.glm()` and `sjp.glmer()`, `type = "pred"` and `type = "pred.fe"` now plot error bars for `show.ci = TRUE` and a discrete variable on the x-axis.
* For `sjp.glm()` and `sjp.glmer()`, `type = "pred"` and `type = "pred.fe"` now accept three variables for the `vars`-argument, to facet grouped predictions by a third variable.
* For `sjp.lm()`, `sjp.lmer()`, `sjp.glm()` and `sjp.glmer()`, the `...`-ellipses argument now is also passed down to all errorbars- and smooth-geoms in prediction- and effect-plots, so you can now use the `width`-argument to show the small stripes at the lower/upper end of the error bars, the `alpha`-argument to define alpha-level or the `level`-argument to define the level of confidence bands.
* `sjp.lm()`, `sjp.lmer()`, `sjp.glm()` and `sjp.glmer()` get a `point.color`-argument, do define color of point-geoms when `show.scatter = TRUE`. If not defined, point-geoms will have same group-color as lines.
* Effect-plots (`type = "eff"`) now plot data points for discrete variables on the x-axis.
* `sjt.lm()` and `sjt.glm()` get a `robust`-argument to compute robust standard errors and confidence intervals.
* `sjp.resid()` now also returns a plot with the residual pattern, `$pattern`.
* Plot and axis titles from effect-plots can now be changed with `title` or `axis.title` argument. Use a character vector of length > 1 to define (axis) titles for each plot or facet; use `""` to remove the titles.
* Pick better defaults for `geom.size`-argument for histogram and density plots in `sjp.frq()`.
* Improved automatic label detection for regression models for plot or table output.

## Bug fixes

* Restored correct order of categories in `sjp.xtab()` and `sjp.grpfrq()` for stacked bars (`position_stack()` reversed order since last ggplot2-update), so labels are now correclty positioned again.
* Restored correct order of categories in `sjp.likert()`, so groups are now in correct order again.
* Fixed bug in `sjt.grpmean()` for variables with unused value labels (values that were labelled, but did not appear on the vector).
* Fixed wrong documentation for `show.summary`-argument in `sjt.xtab()`.
* `sjt.frq()` and `sjp.frq()` showed messed up labels when a labelled vector had both `NA` values _and_ `NaN` or infinite values.
* `sjtab()` did not create tables for `fun = "xtab"` with additional arguments.


# sjPlot 2.1.2

## General

* Effect-plots from `sjp.int()`, `sjp.glm()` and `sjp.glmer()` now support the `transformation`-argument from the __effects__-package. For example, when calling `sjp.glm(fit, type = "eff", transformation = NULL)`, predictions are on their original scale (y-scale) and the title for the y-scale is changed accordingly.

## Changes to functions

* Restored order of categories in `sjp.stackfrq()`, which were reversed by the last ggplot2-update, where `position_stack()` now sort the stacking order to match grouping order.

## Bug fixes

* Fixed bug in `sjplot()` that caused figures not being plotted in certain situations.
* Fixed bug in `sjp.lmm()`, which caused an error for plotting multiple mixed models when Intercept was hidden.
* Fixed bug in `sjp.lmm()`, which caused an error for plotting multiple mixed models when `type = "std"` or `type = "std2"`.

# sjPlot 2.1.1

## General

* Some fixes needed to be compatible with the latest ggplot2-update.

## New functions

* `sjplot`, a pipe-friendly wrapper for some of this package's plotting-functions.
* `sjtab`, a pipe-friendly wrapper for some of this package's table-functions.


# sjPlot 2.1.0

## New functions

* `sjp.resid`, an experimental function to plot and analyze residuals from linear models.
* `plot_grid` to plot a list of ggplot-objects as arranged grid in a single plot.
* `set_theme` to use a preset of default themes for plots from the sjp-functions.

## Changes to functions

* For `sjp.glmer` and `sjp.lmer`, argument `show.ci` now also applies for plotting random effects (`type = "re"`, the default), so confidence intervals may not be calculated. This may be useful in some cases where computation of standard errors for random effects caused an error.
* Effect plots (`type = "eff"`) for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` should now better handle categorical variables and their labels, including using error bars insted of regions for confidence intervals.
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.
* `get_option("p_zero")` was removed, and `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `p.zero` argument.
* `sjp.setTheme` no longer sets default theme presets for plots; use `set_theme` instead.

## Bug fixes

* A bug introduced in update 2.0.2 caused an error in `sjp.lm` for `type = "std"`.
* Effect plots (`type = "eff"`) for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` did not plot all predictors, when predictor name was not exactly specified in formula, but transformed inside formula (e.g. `log(pred + 1)`).
