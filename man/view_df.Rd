% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_df.R
\name{view_df}
\alias{view_df}
\title{View structure of labelled data frames}
\usage{
view_df(
  x,
  weight.by = NULL,
  alternate.rows = TRUE,
  show.id = TRUE,
  show.type = FALSE,
  show.values = TRUE,
  show.string.values = FALSE,
  show.labels = TRUE,
  show.frq = FALSE,
  show.prc = FALSE,
  show.wtd.frq = FALSE,
  show.wtd.prc = FALSE,
  show.na = FALSE,
  max.len = 15,
  sort.by.name = FALSE,
  wrap.labels = 50,
  verbose = FALSE,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
}
\arguments{
\item{x}{A (labelled) data frame, imported by \code{\link[sjlabelled]{read_spss}},
\code{\link[sjlabelled]{read_sas}} or \code{\link[sjlabelled]{read_stata}} function,
or any similar labelled data frame (see \code{\link[sjlabelled]{set_label}}
and \code{\link[sjlabelled]{set_labels}}).}

\item{weight.by}{Name of variable in \code{x} that indicated the vector of
weights that will be applied to weight all  observations. Default is
\code{NULL}, so no weights are used.}

\item{alternate.rows}{Logical, if \code{TRUE}, rows are printed in
alternatig colors (white and light grey by default).}

\item{show.id}{Logical, if \code{TRUE} (default), the variable ID is shown in
the first column.}

\item{show.type}{Logical, if \code{TRUE}, adds information about the
variable type to the variable column.}

\item{show.values}{Logical, if \code{TRUE} (default), the variable values
are shown as additional column.}

\item{show.string.values}{Logical, if \code{TRUE}, elements of character vectors
are also shown. By default, these are omitted due to possibly overlengthy
tables.}

\item{show.labels}{Logical, if \code{TRUE} (default), the value labels are
shown as additional column.}

\item{show.frq}{Logical, if \code{TRUE}, an additional column with
frequencies for each variable is shown.}

\item{show.prc}{Logical, if \code{TRUE}, an additional column with percentage
of frequencies for each variable is shown.}

\item{show.wtd.frq}{Logical, if \code{TRUE}, an additional column with weighted
frequencies for each variable is shown. Weights strem from \code{weight.by}.}

\item{show.wtd.prc}{Logical, if \code{TRUE}, an additional column with weighted
percentage of frequencies for each variable is shown. Weights strem from
\code{weight.by}.}

\item{show.na}{logical, if \code{TRUE}, \code{\link{NA}}'s (missing values)
are added to the output.}

\item{max.len}{Numeric, indicates how many values and value labels per variable
are shown. Useful for variables with many different values, where the output
can be truncated.}

\item{sort.by.name}{Logical, if \code{TRUE}, rows are sorted according to the
variable names. By default, rows (variables) are ordered according to their
order in the data frame.}

\item{wrap.labels}{Numeric, determines how many chars of the value, variable
or axis labels are displayed in one line and when a line break is inserted.}

\item{verbose}{Logical, if \code{TRUE}, a progress bar is displayed
while creating the output.}

\item{CSS}{A \code{\link{list}} with user-defined style-sheet-definitions,
according to the \href{https://www.w3.org/Style/CSS/}{official CSS syntax}.
See 'Details' or \href{https://strengejacke.github.io/sjPlot/articles/table_css.html}{this package-vignette}.}

\item{encoding}{Character vector, indicating the charset encoding used
for variable and value labels. Default is \code{"UTF-8"}. For Windows
Systems, \code{encoding = "Windows-1252"} might be necessary for proper
display of special characters.}

\item{file}{Destination file, if the output should be saved as file.
If \code{NULL} (default), the output will be saved as temporary file and
opened either in the IDE's viewer pane or the default web browser.}

\item{use.viewer}{Logical, if \code{TRUE}, the HTML table is shown in the IDE's
viewer pane. If \code{FALSE} or no viewer available, the HTML table is
opened in a web browser.}

\item{remove.spaces}{Logical, if \code{TRUE}, leading spaces are removed from all lines in the final string
that contains the html-data. Use this, if you want to remove parantheses for html-tags. The html-source
may look less pretty, but it may help when exporting html-tables to office tools.}
}
\value{
Invisibly returns
         \itemize{
           \item the web page style sheet (\code{page.style}),
           \item the web page content (\code{page.content}),
           \item the complete html-output (\code{page.complete}) and
           \item the html-table with inline-css for use with knitr (\code{knitr})
           }
           for further use.
}
\description{
Save (or show) content of an imported SPSS, SAS or Stata data file,
               or any similar labelled \code{data.frame}, as HTML table.
               This quick overview shows variable ID number, name, label,
               type and associated value labels. The result can be
               considered as "codeplan" of the data frame.
}
\examples{
\dontrun{
# init dataset
data(efc)

# view variables
view_df(efc)

# view variables w/o values and value labels
view_df(efc, show.values = FALSE, show.labels = FALSE)

# view variables including variable typed, orderd by name
view_df(efc, sort.by.name = TRUE, show.type = TRUE)

# User defined style sheet
view_df(efc,
        CSS = list(css.table = "border: 2px solid;",
                   css.tdata = "border: 1px solid;",
                   css.arc = "color:blue;"))}

}
