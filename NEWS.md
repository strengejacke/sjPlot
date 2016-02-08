# sjPlot 1.9.1

## Changes to functions
* Improved text label positioning for plotting functions.
* Plotting functions now get an argument `y.offset` to specify an offset for text labels from geoms.
* `sjp.lm` and `sjt.lm` now support `gls` models fitted with `nlme::gls`.
* `sjp.int` now fits the y-axis to the required range for predicted probabilities for logistic regressions instead of always using a range from 0 to 1, even for smaller effects ([#86](https://github.com/sjPlot/devel/issues/86)).
* `sjp.glmer` and `sjp.lmer` get a `axisLimits.y` argument to specify y-axis limits specifically for predicted probability or effect plots.
* `view_df` now supports showing missings and missing percentages ([#76](https://github.com/sjPlot/devel/issues/76)).
* Harmonized column names of returned data frames to match [broom's naming convention](https://github.com/dgrtwo/broom#tidy-functions) for `sjp.lm`, `sjp.glm`, `sjp.lmer`, `sjp.glmer`, `sjp.lmm`, `sjp.glmm`, `sjp.aov1` and `sjp.int` ([#94](https://github.com/sjPlot/devel/issues/94)).
* Functions with harmonized data frames as return value now also gain the class attribute `sjPlot`, and all returned data frame values are named `data`.
* `sjp.scatter` gets a `useCount` argument to indicate overplotting by point size.
* `sjp.scatter` now also plots data points when using argument `pointLabels`, so exact position of labelled data points is visible. `geom_text_repel` is used to avoid overlapping of points and labels.
* `sjt.xtab` gets a `title` argument to print a table caption.

## Bug fixes
* Automatic label detection did not choose column names when no variable labels were present for functions that accepted data frames as data argument ([#96](https://github.com/sjPlot/devel/issues/96)), now works again.
* `sjp.int` did not work with fitted models from class `lme`, now works again.
* `sjt.xtab` did not show `NA` values for `showNA = TRUE`, now works again.
* `sjt.xtab` did not use arguments `valueLabels` ([#101](https://github.com/sjPlot/devel/issues/101)), now works again.
* Table summary (chi-squared, phi, p) for `sjt.xtab` were wrong, now works again.
* Due to rounding, total percentage in `sjt.xtab` could differ from 100%.
* Minor fixes.
