# sjPlot 2.0.0-2

## Changes to functions

* `sjt.frq`, `sjt.xtab` and `view_df` now show notes (`note`-attribute, see `sjmisc::set_note`) of labelled data as tooltip, when mouse hovers the variable name/label, in the HTML-output.


## Bug fixes

* `axis.lim` was not recognized for non-binomial model families and linear models slope- and effect-plot-types.
