# sjPlot 1.9.2-1

## Changes to functions

* Better support for different model families in `sjp.glm` and `sjp.glmer`.
* `sjt.lm`, `sjt.lmer`, `sjt.glm` and `sjt.glmer` get a `showDeviance` argument to display model's deviance in the table summary.

## Bug fixes

* `sjt.xtab` did not apply `highlightTotal` to total column ([#111](https://github.com/sjPlot/devel/issues/111)).