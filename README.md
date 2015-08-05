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

### Changelog of development build 1.8.2-6

#### New functions
* `sjp.gpt` to plot grouped proportional tables.

#### Changes to functions
* `sjp.lmm` can now also plot standardized estimates.
* `sjp.lm`, `sjp.lmm` and `sjt.lm` can now plot standardized estimates, where standardization is computed following Gelman's approach by dividing estimates by two standard deviations.
* Added `type = "coeff"` to `sjp.lmer` to plot joint random and fixed effects coefficients.
* `type = "poly"` in `sjp.lm` can now deal with fitted models that either use polynomials with `poly` or splines with `bs` (see examples).
* `sjt.df` gets a `big.mark` parameter to add thousands-separators if parameter `describe = TRUE`.
* `sjt.df` and `view_df` now recognize Date and POSIX-classes, if `showType = TRUE`.
* `sjp.poly` now also returns cutpoints of loess curvature, to get maximum / minimum values of loess curvature.
* `sjp.lm` with `type = "ma"` now also returns all plots as list of ggplot-objects.
* `sjp.setTheme` now allows for custom label and title colors when using pre-set-themes.
* Improved automatic y-axis-limit detection in `sjp.frq` and `sjp.grpfrq`.
* Minor improvements to `sjp.lmm` and `sjp.glmm`.

#### Bug fixes
* Fixed bug with computation of predicted probabilities in `sjp.glm` and `sjp.glmer` (only occured when `type = "y.pc"`).
* `sjp.grpfrq` did not show correct number of missings (argument `na.rm = FALSE`), if grouping variable startet with zero.
* Fixed bug with `sjp.frq` and `sjt.frq`, when variable was a labelled factor with lowest factor level smaller than 1.
* Fixed bug in `view_df` with parameter `showFreq = TRUE`, when variable was a character vector.
* Minor bug fixes with p-shapes in `sjp.lmm` and `sjp.glmm`.
* Fixed bug in `sjt`-table functions that occured with invalid multibyte strings.
