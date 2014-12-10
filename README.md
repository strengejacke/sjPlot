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


### Changelog of development build 1.6.1-4

#### New functions
* New function `sjp.glm.int` to plot probability curves of interaction terms in generalized linear models.
* New function `sjp.glmer.int` to plot probability curves of interaction terms in generalized linear mixed effects models.
* New function `sjp.lmer.int` to plot probability curves of interaction terms in linear mixed effects models.


#### Changes to functions
* Added parameter `ri.nr` to `sjp.glmer` and `sjp.lmer` to select which random effects of which random intercept to plot, in case the model has multiple random intercepts.
* `sjt.xtab` can now show/hide observed values in table cells with `showObserved` parameter.
* `sjt.df` now distinguishes more variable / vector types (like ordinal and categorial, instead of factor only, or double, integer and numeric).
* `sjp.lm1` no longer requires the data frame used to fit the model.
* Added parameter `useResiduals` to `sjp.lm1`, so residuals may be plotted against predictor (for diagnostic purposes).
* Added parameters `showCountValues` and `showPercentageValues` to function `sjp.xtab`, so either percentage value labels, count value labels or both can be printed.
* Removed parameters `maxYlim` and `upperYLim` from `sjp.grpfrq`, `sjp.xtab` and `sjp.frq`, and added `axisLimits.y` as new option to define y-axis-range.
* Parameter `labelPos` now also works for flipped coordinates in `sjp.xtab`.

#### Bug fixes
* Parameter `geom.size` was not applied to bar charts in `sjp.grpfrq` - fixed.
* Q-Q plot of `sjp.lm.ma` used wrong linear q-q line - fixed.


### Changelog of current stable build 1.6

#### General
* Comprehensive online manual available at [strengejacke.de](http://www.strengejacke.de/sjPlot/)
* Most geom-aesthetics and theme-options have been removed from all `sjp`-functions. This allows more flexibility in creating own themes or using other packages that modify theme appearance. Use the new function `sjp.setTheme` to globally change theme- and geom-aesthetics for sjp-functions, or use packages like `ggthemr` or `ggthemes` to tweak the plot appearance.
* Replaced _b_ with beta-sign in model-summary for `sjp.lm` and `sjp.lm1`.
* Renamed `sju.aov1.levene` to `sjs.aov1.levene`.
* Documentation / help files have been revised and cleaned up.

#### New functions
* New function `sjp.setTheme` to globally change theme- and geom-aesthetics for sjp-functions.
* New function `sjp.lmer` to plot random or fixed effects of linear mixed effects models.
* New function `sjp.glmer` to plot random or fixed effects of generalized linear mixed effects models.
* New function `sjt.grpmean` to print grouped means as HTML table.
* New function `sjs.etasq` to compute eta squared value for one-way-anovas.
* New function `sjs.se` to compute standard errors.

#### Changes to functions
* sjp-functions now have a generic `geom.colors` (and, if applicable, also `geom.size`) parameter to change geom-appearance. Most other aesthetics modifications should be made using `sjp.setTheme`.
* All `sjt`-functions now auto-detect the character encoding depending on the platform's os-type (i.e. `encoding`-parameter defaults to `NULL`). Use parameter `encoding` to select a specific character encoding for the sjt-functions.
* `sjp.likert` was completely re-written and now offers to also plot a neutral (_don't know_) category.
* `sji.getValueLabels` and `sji.getVariableLabels` now also accept single variables as parameter.
* `sjp.glm`, `sjp.lm`, `sjt.glm` and `sjt.lm` now automatically retrieve the predictor's variable labels, if possible. Note that factor variables have more "labels" (for each factor level) than possible variable label attributes, so automatic detection of varibale labels does not work in such cases.
* `sjp.corr` was revised and now plots circles only in upper and values only in lower triangle.
* Added further options to `type` parameter in `sjp.glm` to show additional plots with probability curves of coefficients.
* Added parameter `sort.frq` to `sjt.frq` to order frequencies ascending or descending.
* Added parameters `title`, `breakTitleAt` and `useResiduals` to `sjp.reglin`, so residuals may be plotted against predictors (for diagnostic purposes).
* Parameter `labelPos` in `sjp.grpfrq` now also allows centered label positioning.
* Parameter `valueLabelPosOnTop` in `sjp.xtab` was removed and replaced by `labelPos`, which also allows centered label positioning.
* `sjp.lm.ma` now also plots residuals against predictors with scatter plots, if `completeDiagnostic` is TRUE.
* `sju.setNA` now removes value label attributes and value label names from those values that have been set to `NA` (so NA's are not associated with any value labels).
* Changed behaviour of `transformTicks` parameter in `sjp.glm` and `sjp.glmm` to get more pretty breaks for gridlines.
* Renamed parameter `flipCoordinates` to `coord.flip`.
* Renamed parameter `useFacetGrid` to `facet.grid`.

#### Bug fixes
* Number of observations in `sjt.lm` was wrong in table - fixed.
* `sjp.frq`, `sjt.frq` and `sjt.xtab` did not show (leading) categories with zero-counts - fixed.
* Legend labels in `sjp.grpfrq` were not shown (occured during implementation of ggthemr-support) - fixed.
* Fixed bugs with wrong label association for zero-categories in `sjt.frq`.
* Fixed minor bugs in various `sjp`-functions, where ggthemr-theme-color were not applied to bar/point/line-colors.
* Fixed bug with dot plots in `sjp.grpfrq`.
* Fixed bug with retrieving interaction terms from fitted models in `sjp.emm.int`.
* Fixed bug in `sju.strpos` where returned indices in some cases were _1_ instead of correct match.

### Some ideas for future updates
* Plotting interaction terms of mixed effects models depending on random intercept.
* Printing tables of (generalized) linear models from models with different coefficients (e.g. to print stepwise regressions).
