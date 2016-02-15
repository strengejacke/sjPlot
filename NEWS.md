# sjPlot 1.9.1-1

## Changes to functions

* `sjp.lmm` now also supports linear mixed effects models (of class `merMod`).
* `sjp.int` now uses proper x-axis-tick-labels for `type = "eff"`, when predictor on x-axis is a factor with non-numeric factor-levels (or has label attributes).


## Bug fixes

* Fixed bug in `sjp.int`, where automatic y-axis-scaling for binary outcomes cut off parts of confidence region in some cases.
