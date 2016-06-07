# sjPlot 2.0.0-2

## General

* `sjt.lmer` and `sjt.glmer` now warn when `show.aic = TRUE` and models were fitted with REML instead of ML.

## Changes to functions

* Added `group.estimate` argument to `sjp.lmer` and `sjp.glmer` (for fixed effetcs only).
* `sjt.frq`, `sjt.xtab` and `view_df` now show notes (`note`-attribute, see `sjmisc::set_note`) of labelled data as tooltip, when mouse hovers the variable name/label, in the HTML-output.
* `axis.title` argument for `sjp.glmer` and `sjp.lmer` can now be a vector of length one or two, to be more flexible with axes titles for the various plot types.
* `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `sep.column` argument to add (default) or remove a separator column (i.e. margin) between model columns.
* Argument `axis.title` now also applies to `type = "pred"` for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.


## Bug fixes

* `axis.lim` was not recognized for non-binomial model families and linear models slope- and effect-plot-types.
