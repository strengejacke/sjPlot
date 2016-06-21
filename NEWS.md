# sjPlot 2.0.0-3

## General

* `sjt.lmer` and `sjt.glmer` now warn when `show.aic = TRUE` and models were fitted with REML instead of ML.
* Better support for `plm` objects in `sjt.lm`, `sjp.lm` and `sjp.int`.

## Changes to functions

* Added `group.estimate` argument to `sjp.lmer` and `sjp.glmer` (for fixed effetcs only).
* `sjt.frq`, `sjt.xtab` and `view_df` now show notes (`note`-attribute, see `sjmisc::set_note`) of labelled data as tooltip, when mouse hovers the variable name/label, in the HTML-output.
* `axis.title` argument for `sjp.glmer` and `sjp.lmer` can now be a vector of length one or two, to be more flexible with axes titles for the various plot types.
* `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `sep.column` argument to add (default) or remove a separator column (i.e. margin) between model columns.
* `sjp.scatter` now uses value labels from grouping variable as title for plots if `facet.grid = TRUE`.
* Argument `axis.title` now also applies to `type = "pred"` for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.
* Argument `geom.colors` now applies to more plot types in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`. 
* Added argument `legend.title` to `sjp.lmer` and `sjp.glmer` to set legend title for those plot types who have legends.
* Added argument `jitter.ci` to `sjp.int` to add jittering to confidence bands for error bars, to avoid overlap.
* Added argument `string.total` to `sjt.stackfrq` to label the column with total N (see `show.total`).

## Bug fixes

* `axis.lim` was not recognized for non-binomial model families and linear models slope- and effect-plot-types.
