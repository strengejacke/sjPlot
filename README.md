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

### Changelog of latest stable build 1.9

#### General
* Fixed many issues related to the latest update of [ggplot2](https://cran.r-project.org/package=ggplot2).
* Argument `show.se` is now deprecated. Use `show.ci` instead.
* Redesign of computation of frequency tables for `sjp.frq` and `sjt.frq`, being more robust and generally working with labelled, non-labelled, numeric, character vectors and factors.
* Redesign of computation of frequency tables for `sjp.grpfrq`, `sjp.xtab` and `sjt.xtab`, being more robust and generally working with labelled, non-labelled, numeric, character vectors and factors.
* Better automatic handling of variable and value labels that are used for labelling plot axes and titles or table columns.

#### Changes to functions due to new ggplot2-version
* `sjp.grpfrq` no longer has plot type `type = "histogram"`. Maybe re-implemented in a later update. Due to this change, arguments like `showMeanIntercept` and similar were removed.
* Plotting functions no longer have argument `labelPosition`. Instead, use arguments `vjust` and `hjust`, which correspond to the same ggplot2-aesthetics according to their possible values.


#### Changes to functions
* `sjp.lm` gets a `group.estimates` argument to group estimates in forest plots and colour them according to group assignment. Use arguments `show.legend` and `legendTitle` to modify group legend.
* `sjp.lmer` and `sjp.glmer` can now plot random effect parts of random slope-intercept models (with `type = "rs.ri"`), where regression lines or predicted probabilities of random intercept and slopes are plotted.
* Intercept line plotting in `sjp.int` for `type = "cond"` was removed.
* Line geoms for `type = "cond"` in `sjp.int` now always start at y-position zero, to better indicate the effective change of interaction effect compared to base reference. Now, the y-position indicates the change in the reponse due to the interaction effect.
* `sjp.int` gets a `geom.size` argument to specify line width.

#### Bug fixes
* Argument `ci.hyphen` in function `sjt.lm` and `sjt.lmer` was not correctly applied to confidence intervals of standardized beta values.