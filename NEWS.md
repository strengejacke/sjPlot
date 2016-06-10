# sjPlot 2.0.0-3

## General

* `sjt.lmer` and `sjt.glmer` now warn when `show.aic = TRUE` and models were fitted with REML instead of ML.

## Changes to functions

* Added `group.estimate` argument to `sjp.lmer` and `sjp.glmer` (for fixed effetcs only).
* `sjt.frq`, `sjt.xtab` and `view_df` now show notes (`note`-attribute, see `sjmisc::set_note`) of labelled data as tooltip, when mouse hovers the variable name/label, in the HTML-output.
* `axis.title` argument for `sjp.glmer` and `sjp.lmer` can now be a vector of length one or two, to be more flexible with axes titles for the various plot types.
* `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `sep.column` argument to add (default) or remove a separator column (i.e. margin) between model columns.
* Argument `axis.title` now also applies to `type = "pred"` for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.
* Argument `geom.colors` now applies to more plot types in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`. 
* Added argument `legend.title` to `sjp.lmer` and `sjp.glmer` to set legend title for those plot types who have legends.

## Bug fixes

* `axis.lim` was not recognized for non-binomial model families and linear models slope- and effect-plot-types.
