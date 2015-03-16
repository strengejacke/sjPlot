\encoding{UTF-8} 
\name{sjPlot-package}
\alias{sjPlot-package}
\alias{sjPlot}
\docType{package}
\title{Data Visualization for Statistics in Social Science}
\description{
  Collection of plotting and table output functions for data visualization. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models, mixed effects models, PCA and correlation matrices, cluster analyses, scatter plots, Likert scales etc.

  Furthermore, this package contains some tools that are useful when carrying out data analysis or interpreting data (especially intended for people coming from SPSS and/or who are new to R). These tool functions support reading and writing data (SPSS, SAS and STATA), variable recoding and weighting, statistical tests, interpretation of interaction terms in regression models, reliability tests and constructing index or score variables and much more.

  \emph{What does this package do?}
  
  In short, the functions in this package mostly do two things:
  \enumerate{
    \item compute basic or advanced statistical analyses
    \item either plot the results as ggplot-figure or print them as html-table
  }
  
  However, meanwhile the amount of functions has increased, hence you’ll also find some utility functions beside the plotting functions.
  
  \emph{How does this package help me?}
  
  One of the more challenging tasks when working with R is to get nicely formatted output of statistical analyses, either in graphical or table format. The sjPlot-package takes over these tasks and makes it very easy to create beautiful figures or tables in a very intuitive way.

  There are many examples for each function in the related help files and a comprehensive \href{http://www.strengejacke.de/sjPlot}{online documentation}.
  
  \emph{A note on the package functions}

  The main functions follow specific naming conventions, hence starting with a specific prefix, which indicates what kind of task these functions perform.
  \itemize{
    \item \code{sjc} - cluster analysis functions
    \item \code{sjp} - plotting functions
    \item \code{sjt} - (HTML) table output functions
  }
}
\details{
\tabular{ll}{
Package: \tab sjPlot\cr
Type: \tab Package\cr
Version: \tab 1.7-3\cr
Date: \tab 2015-03-16\cr
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
