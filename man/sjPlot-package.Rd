\encoding{UTF-8} 
\name{sjPlot-package}
\alias{sjPlot-package}
\alias{sjPlot}
\docType{package}
\title{Data Visualization for Statistics in Social Science}
\description{
  Collection of plotting and table output functions for data visualization. Results of various 
  statistical analyses (that are commonly used in social sciences) can be visualized using this package, 
  including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models 
  (forest plots), mixed effects models, PCA, correlations, cluster analyses, scatter plots etc.
  
  Furthermore, this package contains some tools that are useful when carrying out data
  analysis or interpreting data (especially intended for people coming from SPSS and/or who
  are new to R). These tool functions support importing (SPSS) data, 
  variable recoding and weighting, statistical tests, determination of cluster groups, 
  interpretation of interaction terms in regression models etc.
  
  \emph{What does this package do?}
  
  In short, the functions in this package mostly do two things:
  \enumerate{
    \item compute basic or advanced statistical analyses
    \item either plot the results as ggplot-figure or print them as html-table
  }
  
  However, meanwhile the amount of functions has increased, hence you’ll also find 
  some utility functions beside the plotting functions (see below).
  
  \emph{How does this package help me?}
  
  Basically, this package either helps those users who...
  \itemize{
    \item have difficulties using and/or understanding all possibilities that ggplot offers to create plots, simply by providing intuitive function parameters, which allow for manipulating the appearance of plots;
    \item don’t want to set up complex ggplot-objects each time from the scratch;
    \item want quick inspections of (basic) statistics via (html-)tables that are shown in the IDE's viewer pane or default browser; or
    \item want easily create beautiful table outputs that can be imported in office applications.
  }
  
  For advanced users, each functions returns either the prepared ggplot-object (in case of
  \code{sjp}-plotting functions) or the HTML-tables (in case of \code{sjt}-table-output functions), which 
  than can be manipulated even further (for instance, for ggplot-objects, you can specify certain parameters 
  that cannot be modified via the sjPlot package or html-tables could be integrated into knitr-documents).
  
  The functions follow specific naming conventions:
  \itemize{
    \item \code{sjc} - cluster analysis functions
    \item \code{sji} - data import functions
    \item \code{sjp} - plotting functions
    \item \code{sjt} - (HTML) table output functions
    \item \code{sjs} - statistical (tests) functions
    \item \code{sju} - utility and recode functions
  }
}
\details{
\tabular{ll}{
Package: \tab sjPlot\cr
Type: \tab Package\cr
Version: \tab 1.6.8-4\cr
Date: \tab 2015-02-10\cr
License: \tab GPL-3\cr
}
}
\author{
Daniel Lüdecke \email{d.luedecke@uke.de}
}
\references{
Documentation: \url{http://www.strengejacke.de/sjPlot} \cr
Weblog: \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr
Github: \url{https://github.com/sjPlot/devel} \cr
Bug reports: \url{https://github.com/sjPlot/devel/issues}
}
