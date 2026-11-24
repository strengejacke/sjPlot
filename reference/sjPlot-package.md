# Data Visualization for Statistics in Social Science

Collection of plotting and table output functions for data
visualization. Results of various statistical analyses (that are
commonly used in social sciences) can be visualized using this package,
including simple and cross tabulated frequencies, histograms, box plots,
(generalized) linear models, mixed effects models, PCA and correlation
matrices, cluster analyses, scatter plots, Likert scales, effects plots
of interaction terms in regression models, constructing index or score
variables and much more.

The package supports labelled data, i.e. value and variable labels from
labelled data (like vectors or data frames) are automatically used to
label the output. Own labels can be specified as well.

*What does this package do?*

In short, the functions in this package mostly do two things:

1.  compute basic or advanced statistical analyses

2.  either plot the results as ggplot-figure or print them as html-table

*How does this package help me?*

One of the more challenging tasks when working with R is to get nicely
formatted output of statistical analyses, either in graphical or table
format. The sjPlot-package takes over these tasks and makes it easy to
create beautiful figures or tables.

There are many examples for each function in the related help files and
a comprehensive online documentation at
<https://strengejacke.github.io/sjPlot/>.

*A note on the package functions*

The main functions follow specific naming conventions, hence starting
with a specific prefix, which indicates what kind of task these
functions perform.

- `sjc` - cluster analysis functions

- `sjp` - plotting functions

- `sjt` - (HTML) table output functions

## Author

Daniel Lüdecke <d.luedecke@uke.de>
