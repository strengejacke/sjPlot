# sjPlot 2.0.1-1

## General

* Replace deprecated `dplyr::add_rownames()` with `tibble::rownames_to_column()`.
* Improved title labelling for `type = "pred"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.
* Improved title and facet title labelling for `type = "eff"` in `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer`.

## Bug fixes

* In `sjp.corr`, argument `sort.corr = FALSE` caused an error.
