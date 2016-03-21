# sjPlot 1.9.2-4

## General

* P-values for linear mixed models are now computed using conditional F-tests with Kenward-Roger approximation for the df from the _pbkrtest_ package, if available.

## Changes to functions

* Better support for different model families in `sjp.glm` and `sjp.glmer`.
* `sjt.lm`, `sjt.lmer`, `sjt.glm` and `sjt.glmer` get a `showDeviance` argument to display model's deviance in the table summary.
* `sjt.lmer` and `sjt.glmer` now show R2-values (based on `sjmisc::r2` function).
* `sjt.df` gets a `...` argument to pass down other arguments to `psych::describe` ([#118](https://github.com/sjPlot/devel/issues/118)).
* Plot-type of `sjp.int` now defaults to `type = "eff"`.
* Minor improvements to `sjp.int` according to plot labels (legend, axis).

## Bug fixes

* `sjt.xtab` did not apply `highlightTotal` to total column ([#111](https://github.com/sjPlot/devel/issues/111)).
* `sjt.xtab` showed wrong total percentages for row and column percentages.
