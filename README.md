sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of several plotting and table output functions for visualizing data. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models (forest plots), mixed effects models, PCA, correlations, cluster analyses, scatter plots etc.

Furthermore, this package contains some tools that are useful when carrying out data analysis or interpreting data (especially intended for people coming from SPSS and/or who are new to R). These tool functions support importing (SPSS) data, variable recoding and weighting, statistical tests, determination of cluster groups, interpretation of interaction terms in regression models etc.


### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/devel")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjPlot")
```

### References and documentation

- [Documentation](http://www.strengejacke.de/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.

### Changelog of current development build 1.6.9-5

#### New functions
* `sji.toFac` to convert (numeric or atomic) variables to factors, but keeps value and variable labels. Useful alternative to `as.factor`, when data has been imported from SPSS (e.g. with `sji.SPSS`).

#### Changes to functions
* Added parameter `group.pred` in `sjt.lm` and `sjt.glm` to automatically group table rows with factor levels of same factor.
* Improved automatic label extraction for `sjp.lm`, `sjt.lm`, `sjp.glm` and `sjt.glm`.
* Improved pre-set theme `538` in `sjp.setTheme`.
* Added further pre-set themes to `sjp.setTheme`.
* Minor improvements in `sjp.lm` with `type="ma"`.

#### General
* Added various aliases to `sjs`, `sju` and `sji`-functions.
* `autoSetValueLabels` and `autoSetVariableLabels` are now global options. E.g., use `options(autoSetValueLabels = FALSE)` to turn off automatic value label detection in plotting and table functions, or `options(autoSetValueLabels = TRUE)` to turn on automatic label detection.

#### Bug fixes
* Fixed bug in `sjt.itemanalysis` [(#issue 8)](https://github.com/sjPlot/devel/issues/8).
* Fixed bug in `sji.setValueLabels`.
* Fixed bug in `sjt.frq` with string-variables that contained a larger amount of unique values including `NA`-values, when parameter `skipZeroRows` was set to `auto` (default).
* Minor bug fixes in `sjp.lm` with `type="ma"`.


### Changelog of current stable build 1.6.9

#### New functions
* Added new functions `sjd.norm`, `sjd.chisq`, `sjd.f` and `sjd.t` to plot distribution curves, optionally with shaded areas indicating the p-level area.

#### Changes to functions
* Plotting single predictors of linear models (`type = "pred"` in function `sjp.lm`) now also supports plotting interaction terms and factor levels. Needs parameter `x=TRUE` in `lm`-call to work.
* Added parameter `showCI` to `sjp.frq` to show 95% confidence intervals. Use `error.bar.colors` to change colors of error bars when using bar charts. In case of dot plots, error bars have the same color as dots (see `geom.colors`).
* Added parameter `remove.spaces` to all `sjt`-function to remove leading spaces (parantheses of html-tags), which may make tables less cluttered when importing them into office applications.
* Added parameters `digits` and `digits.stats` to `sjt.stackfrq`, to specifiy digits after decimal point for percentage and statistic values.
* Added parameter `atomic.to.fac` to `sji.SPSS`, so variables with nominal or ordinal scale imported from SPSS data sets are imported as `factors`, not as `atomic`.
* `sjp.scatter` no longer needs both `x` and `y` to be specified, but at least one of them.
* `sjt.grpmean` now shows p-values for each group (retrieved from anova table).
* Added new theme-preset (`theme = "538"`) to `sjp.setTheme`.

#### Bug fixes
* `sjt.grpmean` did not indicate p-values smaller than 0.001 as _p<0.001_, but still as _p=0.000_ - fixed.
* Fixed bug in function `sjs.stdmm`, which was the cause for a bug with `type = "fe.std"` in `sjp.lmer`.
* Fixed bug in `sjp.int` when fitted model does not contain p-values (e.g. when passing a merMod object from lme4).
* `sji.setValueLabels` did not set labels properly when paramerer `labels` was a list - fixed.
* Minor bug fix in `sjp.int`.
* Minor bug fix in `sjp.setTheme`.


### Some ideas for future updates
* Printing tables of (generalized) linear models from models with different coefficients (e.g. to print stepwise regressions).
