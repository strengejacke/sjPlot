sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of several plotting and table output functions for visualizing data. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models (forest plots), mixed effects models, PCA, correlations, cluster analyses, scatter plots etc.

Furthermore, this package contains some tools that are useful when carrying out data analysis or interpreting data (especially intended for people coming from SPSS and/or who are new to R). These tool functions support reading and writing (SPSS) data, variable recoding and weighting, statistical tests, determination of cluster groups, interpretation of interaction terms in regression models etc.


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

### Changelog of current development build 1.6.9-8

#### General
* Renamed `sjs`, `sju`, `sjd` and `sji`-functions into more intuitiv and shorter function names.
* `autoSetValueLabels` and `autoSetVariableLabels` are now global options. E.g., use `options(autoSetValueLabels = FALSE)` to turn off automatic value label detection in plotting and table functions, or `options(autoSetValueLabels = TRUE)` to turn on automatic label detection.
* Removed `plyr` import and replaced with `dplyr` functions.
* Removed `reshape2` import and replaced with `tidyr` functions.

#### New functions
* `write_spss` to write data frames to SPSS sav-files, including value and variable labels.
* `to_sjPlot` to convert data frames imported with the `haven` package ([see GitHub](https://github.com/hadley/haven)) to a more sjPlot-friendly format.
* `to_fac` to convert (numeric or atomic) variables to factors, but keeps value and variable labels. Useful alternative to `as.factor`, when data has been imported from SPSS (e.g. with `read_spss`).

#### Changes to functions
* `sjt.lm` now also prints multiple fitted models with different predictors in each model (e.g. when comparing stepwise regression). See examples in `?sjt.lm`.
* Added parameter `group.pred` in `sjt.lm` and `sjt.glm` to automatically group table rows with factor levels of same factor.
* Improved `set_var_labels`, `get_var_labels`, `set_val_labels` and `get_val_labels` to cope with `haven` package data structure.
* Improved `view_spss` function (former `sji.viewSPSS`).
* Improved automatic label extraction for `sjp.lm`, `sjt.lm`, `sjp.glm` and `sjt.glm`.
* Improved pre-set theme `538` in `sjp.setTheme`.
* Added further pre-set themes to `sjp.setTheme`.
* Minor improvements in `sjp.lm` with `type="ma"`.

#### Bug fixes
* Fixed bug in `sjt.itemanalysis` [(#issue 8)](https://github.com/sjPlot/devel/issues/8).
* Fixed bug in `sji.setValueLabels`.
* Fixed bug in `sjt.frq` with string-variables that contained a larger amount of unique values including `NA`-values, when parameter `skipZeroRows` was set to `auto` (default).
* Minor bug fixes in `sjp.lm` with `type="ma"`.
* `weight` should now also include `NA`s.
