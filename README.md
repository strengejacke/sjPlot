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


### Changelog of current development build 1.6.8-2

### Bug fixes
* `sjt.grpmean` did not indicate p-values smaller than 0.001 as _p<0.001_, but still as _p=0.000_ - fixed.
* Fixed bug in function `sjs.stdmm`, which was the cause for a bug with `type = "fe.std"` in `sjp.lmer`.
* Fixed bug in `sjp.int` when fitted model does not contain p-values (e.g. when passing a merMod object from lme4).
* Minor bug fix in `sjt.int`.


### Changelog of current stable build 1.6.8

#### Changes to functions
* Plotting standardized beta values in `sjp.lm` can be achieved via the `type` parameter (`type = "std"`). Furthermore, confidence intervals and p-values are shown in the standardized beta plot.
* `sjt.lm` now also prints confidence intervals for standardized beta values.
* `sjp.vif` and `sjp.glm.ma` were merged into `sjp.glm`. Use the `type` parameter to select plot type.
* `sjp.lm1`, `sjp.reglin`, `sjp.vif` and `sjp.lm.ma` were merged into `sjp.lm`. Use the `type` parameter to select plot type.
* Added parameter `vars` to `sjp.glmer` to plot probability curves only for selected variables.
* Added type `fe.ri` to `type` parameter of `sjp.lmer` to plot fixed effects slopes for each random intercept (group level).
* Added type `fe.std` to `type` parameter of `sjp.lmer` to plot standardized coefficients of fixed effects.
* p-values less than 0.001 are indicated as `p < 0.001` instead of being rounded to `p = 0.000`.
* Added parameter `showTotalN` to `sjt.xtab` to show column and row sums even if parameter `showObserved` is `FALSE`.
* Added parameter `digits.summary` to `sjt.grpmean` to use different digits for table values and summary statistics.
* `sji.setValueLabels` now also accepts a vector for the parameter `labels` if `x` is a data frame. Using a vector will apply the labels to each variable of the data frame `x`.
* `sjt.frq` now by default automatically detects whether a variable has a certain proportion of zero-count values and does not print them by default then. Use `skipZeroRows = TRUE` or `skipZeroRows = FALSE` to explicitly show or remove zero-counts from the table.


#### Bug fixes
* Fixed bug with `type = "dots"` in `sjp.grpfrq`.
* Fixed bug with predictor labelling in `sjp.lm` and `sjp.glm` when parameter `labelPredictors` was not used.
* Fixed bug with custom color palettes and neutral categories in `sjp.likert`.


### Some ideas for future updates
* Printing tables of (generalized) linear models from models with different coefficients (e.g. to print stepwise regressions).
