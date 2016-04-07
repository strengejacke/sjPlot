# sjPlot 1.9.4-4

## General

* Improved label detection for `sjp.lm`, `sjp.glm`, `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer`.
* Improved handling of different link functions for generalized linear (mixed) models for effect plots in `sjp.glmer` and `sjp.glm`.
* Effect plots (`type = "eff"`) for (generalized) linear (mixed) models (`sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`) get a `vars` and `facet.grid` argument.
* Predicted values for response (`type = "y.pc"` or `type = "resp"`) for `sjp.glm`, `sjp.glmer`, `sjp.lm` and `sjp.lmer` get a `vars` argument to specify x-axis and optional grouping variables. Furthermore, models from other model families and link functions now also work with this plot type.

## Bug fixes

* During code clean-up, argument `group.pred` did not work for `sjt`-functions in past update.
