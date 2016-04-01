# sjPlot 1.9.4

## General

* Package is now depending on R >= 3.2, because some functions did not work on older R-releases.

## Bug fixes

* `type = "rs.ri"` for `sjp.lmer` and `sjp.glmer` did not work with three-level (or more) mixed models or with mixed models with more than one random part ([#121](https://github.com/sjPlot/devel/issues/121)).
