sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of plotting and table output functions for data visualization. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models, mixed effects models, PCA and correlation matrices, cluster analyses, scatter plots, Likert scales, interpretation of interaction terms in regression models, constructing index or score variables and much more.


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

### Documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.

### Changelog of current development snapshot 1.8-7

#### General
* Deprecated function `sjp.emm.int` was removed. Use `sjp.int` with parameter `type = 'emm'` to plot estimated marginal means.
* Minor improvements for `sjt.lm` and `sjt.glm`.

#### New functions
* `sjt.lmer` to print summary tables of linear mixed models.
* `sjt.glmer` to print summary tables of generalized linear mixed models.

#### Changes to functions
* Added `type = "probc"` to `sjp.glm` as alternative to `type = "prob"`. `type = "probc"` calculated predicted probabilities based on the `predict` function.
* Added `type = "y.prob"` to `sjp.glm` and `sjp.glmer` to plot predicted probabilities of the response.
* Added `type = "resp"` to `sjp.lm` and `sjp.lmer` to plot predicted values of the response.
* `sjt.grpmean` gets a `weightBy` parameter to compute weighted group-means.
* `sjt.glm` gets a `showHosLem` parameter to print results of the Hosmer-Lemeshow-Goodness-of-fit-Test for generalized linear models.
* Added white-background-alternative-themes of 538, 539 and scatter to `sjp.setTheme`.

#### Bug fixes
* Fixed bug with `options(p_zero = TRUE)`, where leading zero was inserted after, instead of before decimal point.
* Fixed formatting bug for pseudo-R2 in `sjt.glm`.
* Fixed bug in `sjp.likert` when data frame had only one column.
* Fixed bug in `sjt.frq` when a data frame contained variables with only NA values.