sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of plotting and table output functions for data visualization. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models, mixed effects models, PCA and correlation matrices, cluster analyses, scatter plots, Likert scales, effects plots of interaction terms in regression models, constructing index or score variables and much more.


### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/devel")
```

Please note that the latest development snapshot most likely depends on the latest build of the [sjmisc-package](https://github.com/sjPlot/sjmisc), so you probably want to install it as well:

```r
devtools::install_github("sjPlot/sjmisc")
```

#### Officiale, stable release
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjPlot)](http://cran.r-project.org/package=sjPlot) 
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjPlot)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjPlot")
```

### Documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since core functionality of package depends on the [ggplot-package](http://cran.r-project.org/package=ggplot2), consider citing this package as well.

### Changelog of development build 1.8.1-7

#### General
* `view_spss` is now deprecated. Use `view_df` instead.

#### New functions
* `sjp.poly` to plot polynomial curves for linear regressions.

#### Changes to functions
* Model and table summaries in plotting functions (like `sjp.lm` or `sjp.grpfrq`) are no longer printed by default. Use `showTableSummary = TRUE` or `showModelSummary = TRUE` to print summaries in plots.
* Added more plotting type options (see `type` parameter) to `sjp.glm`, `sjp.glmer`, `sjp.lm` and `sjp.lmer`: `eff` for plotting marginal effects of model terms, and `poly` to plot predicted values of polynomial terms (only for linear (mixed) models).
* Added parameter `int.term` to `sjp.int`, to plot selected interaction terms for `type = "eff"` only. May be used in cases where effect computation takes too long or even crashes due to out-of-memory-problems.
* Added parameter `axisLimits.x` to `sjp.int`, `sjp.frq` and `sjp.grpfrq`.
* Added parameter `showAICc` to `sjt.lm`, `sjt.glm`, `sjt.lme` and `sjt.glmer` to print second-order AIC.
* Improved automatic y-axis-limit detection in `sjp.frq` and `sjp.grpfrq`.
* Minor improvements to `sjp.likert`, `sjp.int`, `sjp.glm`, `sjp.frq` and `sjp.grpfrq`.

#### Bug fixes
* `sjp.int` sometimes crashed with mixed models, due to slow Kenward-Roger-computation of standard errors, provided by the `effects`-package. Fixed, `KR`-parameter, when calling `allEffects`, now defaults to `FALSE`.
* Fixed bug in `view_spss`, where frequencies were not displayed correctly when a category value had zero counts.
* Fixed bug in `sjp.frq` and `sjt.frq`, where non-incremental levels in some cases were not displayed correctly.
* Fixed bug in `sjp.frq` and `sjt.frq`, where categories of ordered factors were messed up.
* Some minor bug fixes.
