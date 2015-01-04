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


### Changelog of current development build 1.6.7-4

#### Changes to functions
* Added parameter `vars` to `sjp.glmer` to plot probability curves only for selected variables.
* Added type `fe.ri` to `type` parameter of `sjp.lmer` to plot fixed effects slopes for each random intercept (group level).
* Added parameter `pvaluesApaStyle` to various functions. If this parameter is `TRUE`, p-values less than 0.001 are indicated as `p < 0.001` instead of being rounded to `p = 0.000`.
* Added parameter `showTotalN` to `sjt.xtab` to show column and row sums even if parameter `showObserved` is `FALSE`.

#### Bug fixes
* Fixed bug with `type = "dots"` in `sjp.grpfrq`.
* Fixed bug with predictor labelling in `sjp.lm` and `sjp.glm` when parameter `labelPredictors` was not used.

### Changelog of current stable build 1.6.5

#### New functions
* New function `sjp.int`, which can plot regression lines or probability curves of interaction effects in (generalized) linear (mixed effects) models, i.e. fitted models of class `lm`, `glm`, `lmerMod` and `glmerMod` are covered by this function.

#### Changes to functions
* Renamed `sjs.betaCoef` to `sjs.stdb`.
* `sjp.lm.int` was merged with function `sjp.int`.
* Added parameter `ri.nr` to `sjp.glmer` and `sjp.lmer` to select which random effects of which random intercept to plot, in case the model has multiple random intercepts.
* Added parameters `showValueLabels`, `labelDigits` and `showPValueLabels` to `sjp.glmer` and `sjp.lmer` to plot beta or odds ratio values and p-value-stars to fixed effects plots.
* Added paramerter `title.align` to `sjp.setTheme` to change alignment of plot title.
* `sjt.xtab` can now show/hide observed values in table cells with `showObserved` parameter.
* `sjt.df` now distinguishes more variable / vector types (like ordinal and categorial, instead of factor only, or double, integer and numeric).
* `sjp.lm1` no longer requires the data frame used to fit the model.
* Added parameter `useResiduals` to `sjp.lm1`, so residuals may be plotted against predictor (for diagnostic purposes).
* Added parameters `showCountValues` and `showPercentageValues` to function `sjp.xtab`, so either percentage value labels, count value labels or both can be printed.
* Removed parameters `maxYlim` and `upperYLim` from `sjp.grpfrq`, `sjp.xtab` and `sjp.frq`, and added `axisLimits.y` as new option to define y-axis-range.
* Parameter `labelPos` now also works for flipped coordinates in `sjp.xtab`.
* Added parameters to define axis titles and intercept label for y axis to `sjp.glmer` and `sjp.lmer`.

#### General
* Console output of various function that used the `cat` or `print` command were replaced by `message` or `warning`, if more appropriate.

#### Bug fixes
* Parameter `geom.size` was not applied to bar charts in `sjp.grpfrq` - fixed.
* Q-Q plot of `sjp.lm.ma` used wrong linear q-q line - fixed.
* Fixed bug in `sjp.glmer` with plot type `ri.pc`.


### Some ideas for future updates
* Plotting interaction terms of mixed effects models depending on random intercept.
* Printing tables of (generalized) linear models from models with different coefficients (e.g. to print stepwise regressions).
