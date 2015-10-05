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

### Changelog of development build 1.8.3-7

#### General
* Improved encoding detection for `sjt`-functions.

#### Changes to functions
* Predictor grouping with argument `group.pred` now also works for `sjt.lmer` and `sjt.glmer` (in certain cases may be buggy, so `group.pred` defaults to `FALSE`).
* Argument `vars` in `sjp.lmer` and `sjp.glmer` now also applies when plotting estimates (`type = "fe"` or `type = "re"`).
* `view_df_` gets a `weightBy` argument.
* Argument `showCI` in `sjp.int` accepts numeric values for `type = "eff"`,  indicating the confidence interval value.
* Minor improvements to `view_df`, `sjp.lm` and `sjp.lmm`.
* Improved accuracy of computation of skewness value in `sjt.itemanalysis`.

#### Bug fixes
* Fixed bug where in certain cases, ordered factors were not labelled correctly in `sjp.frq`.
* Value labels were not shown in `sjp.aov1`.
* Axis labels were reversed in `sjp.pca` for `type = "bar"`.
