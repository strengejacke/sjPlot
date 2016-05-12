# sjPlot 1.9.4-9

## Major changes

This package update included a major revision of function arguments and their naming, in order to get a consistent argument pattern across all packafe functions. This means that your existing code, which uses **sjPlot**-package-functions, most likely needs adaptions to work again.

* Arguments were harmonized across all package functions. This includes refactoring of many function argument names, to get consistent argument names in functions (e.g. `sort.coef` now no longer exists, and was renamed to `sort.est`, which was already used by some other functions).

* Camel cased argument names were replaced by lowercase dot-separated names (e.g. `showCI` was renamed to `show.ci`) and harmonized bewteen different functions 

* `type` arguments of `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` were harmonized, so that one type does the same in all functions. `"pred"` and `"fe.pred"` were renamed to `"slope"` and `"fe.slope"`, `"fe.ri"` and `"ri.pc"` were renamed to `"ri.slope"`, `"resp"` and `"y.pc"` were renamed to `"pred"` and `"pred.fe"`.

* Arguments in functions were re-ordered and bundled according to their functionality (e.g., the variuous `show...` arguments now should appear on after another in the function and package manual).

## General

* Improved label detection for `sjp.lm`, `sjp.glm`, `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer`.
* Improved handling of different link functions for generalized linear (mixed) models (including negative binomial) for effect plots in `sjp.glmer` and `sjp.glm`.

## Changes to functions

* Effect plots (`type = "eff"`) for (generalized) linear (mixed) models (`sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`) get a `vars` and `facet.grid` argument.
* Predicted values for response (`type = "pred"` or `type = "pred.re"`) for `sjp.glm`, `sjp.glmer`, `sjp.lm` and `sjp.lmer` get a `vars` argument to specify x-axis and optional grouping variables. Furthermore, models from other model families and link functions (including negative binomial) now also work with this plot type.
* Functions `sjp.lmer`, `sjp.glmer`, `sjt.lmer`, `sjt.glmer`, `sjp.lmm` and `sjp.glmm` get a `p.kr` argument, to decide whether computation of p-values should be based on Kenward-Roger approximation or not (for very large data sets, it's recommended to set this argument to `FALSE` because it is very time consuming).

## Bug fixes

* During code clean-up, argument `group.pred` did not work for `sjt`-functions in past update.
* Fixed bug with computation of confidence intervals and relative confidence intervals in `sjp.frq`.