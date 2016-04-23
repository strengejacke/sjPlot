# sjPlot 1.9.4-9

## General

* Improved label detection for `sjp.lm`, `sjp.glm`, `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer`.
* Improved handling of different link functions for generalized linear (mixed) models (including negative binomial) for effect plots in `sjp.glmer` and `sjp.glm`.

## Changes to functions

* Effect plots (`type = "eff"`) for (generalized) linear (mixed) models (`sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`) get a `vars` and `facet.grid` argument.
* Predicted values for response (`type = "y.pc"` or `type = "resp"`) for `sjp.glm`, `sjp.glmer`, `sjp.lm` and `sjp.lmer` get a `vars` argument to specify x-axis and optional grouping variables. Furthermore, models from other model families and link functions (including negative binomial) now also work with this plot type.
* Functions `sjp.lmer`, `sjp.glmer`, `sjt.lmer`, `sjt.glmer`, `sjp.lmm` and `sjp.glmm` get a `p.kr` argument, to decide whether computation of p-values should be based on Kenward-Roger approximation or not (for very large data sets, it's recommended to set this argument to `FALSE` because it is very time consuming).

## Bug fixes

* During code clean-up, argument `group.pred` did not work for `sjt`-functions in past update.
