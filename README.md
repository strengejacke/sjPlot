sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of plotting and table output functions for data visualization. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models, mixed effects models, PCA and correlation matrices, cluster analyses, scatter plots, Likert scales, effects plots of interaction terms in regression models, constructing index or score variables and much more.


## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/devel")
```

Please note that the latest development snapshot most likely depends on the latest builds of the [sjmisc-package](https://github.com/sjPlot/sjmisc) and [sjstats-package](https://github.com/sjPlot/sjstats), so you probably want to install these as well:

```r
devtools::install_github("sjPlot/sjmisc")
devtools::install_github("sjPlot/sjstats")
```

### Officiale, stable release

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjPlot)](https://cran.r-project.org/package=sjPlot) 
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjPlot)](http://cranlogs.r-pkg.org/)
&#160;&#160;
[![total](http://cranlogs.r-pkg.org/badges/grand-total/sjPlot)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjPlot")
```

## Documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)


## Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since core functionality of package depends on the [ggplot-package](https://cran.r-project.org/package=ggplot2), consider citing this package as well.
