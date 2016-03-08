# sjPlot 1.9.2-2

## General

* P-values for linear mixed models are now computed using the Kenward-Roger approximation from the _pbkrtest_ package, if available.

## Changes to functions

* Better support for different model families in `sjp.glm` and `sjp.glmer`.
* `sjt.lm`, `sjt.lmer`, `sjt.glm` and `sjt.glmer` get a `showDeviance` argument to display model's deviance in the table summary.
* `sjt.lmer` now shows R2-values (based on `sjmisc::r2` function).

## Bug fixes

* `sjt.xtab` did not apply `highlightTotal` to total column ([#111](https://github.com/sjPlot/devel/issues/111)).