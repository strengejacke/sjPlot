# Print regression models as HTML table

`tab_model()` creates HTML tables from regression models.

## Usage

``` r
tab_model(
  ...,
  transform,
  show.intercept = TRUE,
  show.est = TRUE,
  show.ci = 0.95,
  show.ci50 = FALSE,
  show.se = NULL,
  show.std = NULL,
  std.response = TRUE,
  show.p = TRUE,
  show.stat = FALSE,
  show.df = FALSE,
  show.zeroinf = TRUE,
  show.r2 = TRUE,
  show.icc = TRUE,
  show.re.var = TRUE,
  show.ngroups = TRUE,
  show.fstat = FALSE,
  show.aic = FALSE,
  show.aicc = FALSE,
  show.dev = FALSE,
  show.loglik = FALSE,
  show.obs = TRUE,
  show.reflvl = FALSE,
  terms = NULL,
  rm.terms = NULL,
  order.terms = NULL,
  keep = NULL,
  drop = NULL,
  title = NULL,
  pred.labels = NULL,
  dv.labels = NULL,
  wrap.labels = 25,
  bootstrap = FALSE,
  iterations = 1000,
  seed = NULL,
  vcov.fun = NULL,
  vcov.args = NULL,
  string.pred = "Predictors",
  string.est = "Estimate",
  string.std = "std. Beta",
  string.ci = "CI",
  string.se = "std. Error",
  string.std_se = "standardized std. Error",
  string.std_ci = "standardized CI",
  string.p = "p",
  string.std.p = "std. p",
  string.df = "df",
  string.stat = "Statistic",
  string.std.stat = "std. Statistic",
  string.resp = "Response",
  string.intercept = "(Intercept)",
  strings = NULL,
  ci.hyphen = "&nbsp;&ndash;&nbsp;",
  minus.sign = "&#45;",
  collapse.ci = FALSE,
  collapse.se = FALSE,
  linebreak = TRUE,
  col.order = c("est", "se", "std.est", "std.se", "ci", "std.ci", "ci.inner", "ci.outer",
    "stat", "std.stat", "p", "std.p", "df.error", "response.level"),
  digits = 2,
  digits.p = 3,
  digits.rsq = 3,
  digits.re = 2,
  emph.p = TRUE,
  p.val = NULL,
  df.method = NULL,
  p.style = c("numeric", "stars", "numeric_stars", "scientific", "scientific_stars"),
  p.threshold = c(0.05, 0.01, 0.001),
  p.adjust = NULL,
  case = "parsed",
  auto.label = TRUE,
  prefix.labels = c("none", "varname", "label"),
  bpe = "median",
  CSS = css_theme("regression"),
  file = NULL,
  use.viewer = TRUE,
  encoding = "UTF-8"
)
```

## Arguments

- ...:

  One or more regression models, including glm's or mixed models. May
  also be a `list` with fitted models. See 'Examples'.

- transform:

  A character vector, naming a function that will be applied on
  estimates and confidence intervals. By default, `transform` will
  automatically use `"exp"` as transformation for applicable classes of
  `model` (e.g. logistic or poisson regression). Estimates of linear
  models remain untransformed. Use `NULL` if you want the raw,
  non-transformed estimates.

- show.intercept:

  Logical, if `TRUE`, the intercepts are printed.

- show.est:

  Logical, if `TRUE`, the estimates are printed.

- show.ci:

  Either logical, and if `TRUE`, the confidence intervals is printed to
  the table; if `FALSE`, confidence intervals are omitted. Or numeric,
  between 0 and 1, indicating the range of the confidence intervals.

- show.ci50:

  Logical, if `TRUE`, for Bayesian models, a second credible interval is
  added to the table output.

- show.se:

  Logical, if `TRUE`, the standard errors are also printed. If robust
  standard errors are required, use arguments `vcov.fun`, and
  `vcov.args` (see
  [`standard_error`](https://easystats.github.io/parameters/reference/standard_error.html)
  for details).

- show.std:

  Indicates whether standardized beta-coefficients should also printed,
  and if yes, which type of standardization is done. See 'Details'.

- std.response:

  Logical, whether the response variable will also be standardized if
  standardized coefficients are requested. Setting both
  `std.response = TRUE` and `show.std = TRUE` will behave as if the
  complete data was standardized before fitting the model.

- show.p:

  Logical, if `TRUE`, p-values are also printed.

- show.stat:

  Logical, if `TRUE`, the coefficients' test statistic is also printed.

- show.df:

  Logical, if `TRUE` and `p.val = "kr"`, the p-values for linear mixed
  models are based on df with Kenward-Rogers approximation. These
  df-values are printed. See
  [`p_value`](https://easystats.github.io/parameters/reference/p_value.html)
  for details.

- show.zeroinf:

  Logical, if `TRUE` and model has a zero-inflated model part, this is
  also printed to the table.

- show.r2:

  Logical, if `TRUE`, the r-squared value is also printed. Depending on
  the model, these might be pseudo-r-squared values, or Bayesian
  r-squared etc. See
  [`r2`](https://easystats.github.io/performance/reference/r2.html) for
  details.

- show.icc:

  Logical, if `TRUE`, prints the intraclass correlation coefficient for
  mixed models. See
  [`icc`](https://easystats.github.io/performance/reference/icc.html)
  for details.

- show.re.var:

  Logical, if `TRUE`, prints the random effect variances for mixed
  models. See
  [`get_variance`](https://easystats.github.io/insight/reference/get_variance.html)
  for details.

- show.ngroups:

  Logical, if `TRUE`, shows number of random effects groups for mixed
  models.

- show.fstat:

  Logical, if `TRUE`, the F-statistics for each model is printed in the
  table summary. This option is not supported by all model types.

- show.aic:

  Logical, if `TRUE`, the AIC value for each model is printed in the
  table summary.

- show.aicc:

  Logical, if `TRUE`, the second-order AIC value for each model is
  printed in the table summary.

- show.dev:

  Logical, if `TRUE`, shows the deviance of the model.

- show.loglik:

  Logical, if `TRUE`, shows the log-Likelihood of the model.

- show.obs:

  Logical, if `TRUE`, the number of observations per model is printed in
  the table summary.

- show.reflvl:

  Logical, if `TRUE`, an additional row is inserted to the table before
  each predictor of type [`factor`](https://rdrr.io/r/base/factor.html),
  which will indicate the reference level of the related factor.

- terms:

  Character vector with names of those terms (variables) that should be
  printed in the table. All other terms are removed from the output. If
  `NULL`, all terms are printed. Note that the term names must match the
  names of the model's coefficients. For factors, this means that the
  variable name is suffixed with the related factor level, and each
  category counts as one term. E.g. `rm.terms = "t_name [2,3]"` would
  remove the terms `"t_name2"` and `"t_name3"` (assuming that the
  variable `t_name` is categorical and has at least the factor levels
  `2` and `3`). Another example for the *iris*-dataset:
  `terms = "Species"` would not work, instead use
  `terms = "Species [versicolor,virginica]"`.

- rm.terms:

  Character vector with names that indicate which terms should be
  removed from the output Counterpart to `terms`. `rm.terms = "t_name"`
  would remove the term *t_name*. Default is `NULL`, i.e. all terms are
  used. For factors, levels that should be removed from the plot need to
  be explicitly indicated in square brackets, and match the model's
  coefficient names, e.g. `rm.terms = "t_name [2,3]"` would remove the
  terms `"t_name2"` and `"t_name3"` (assuming that the variable `t_name`
  was categorical and has at least the factor levels `2` and `3`).

- order.terms:

  Numeric vector, indicating in which order the coefficients should be
  plotted. See examples in [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html).

- keep, drop:

  Character containing a regular expression pattern that describes the
  parameters that should be included (for `keep`) or excluded (for
  `drop`) in the returned data frame. `keep` may also be a named list of
  regular expressions. All non-matching parameters will be removed from
  the output. If `keep` has more than one element, these will be merged
  with an `OR` operator into a regular expression pattern like this:
  `"(one|two|three)"`. See further details in
  [`?parameters::model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html).

- title:

  String, will be used as table caption.

- pred.labels:

  Character vector with labels of predictor variables. If not `NULL`,
  `pred.labels` will be used in the first table column with the
  predictors' names. By default, if `auto.label = TRUE` and [data is
  labelled](https://strengejacke.github.io/sjlabelled/articles/intro_sjlabelled.html),
  [`term_labels`](https://strengejacke.github.io/sjlabelled/reference/term_labels.html)
  is called to retrieve the labels of the coefficients, which will be
  used as predictor labels. If data is not labelled,
  [format_parameters()](https://easystats.github.io/parameters/reference/format_parameters.html)
  is used to create pretty labels. If `pred.labels = ""` or
  `auto.label = FALSE`, the raw variable names as used in the model
  formula are used as predictor labels. If `pred.labels` is a named
  vector, predictor labels (by default, the names of the model's
  coefficients) will be matched with the names of `pred.labels`. This
  ensures that labels always match the related predictor in the table,
  no matter in which way the predictors are sorted. See 'Examples'.

- dv.labels:

  Character vector with labels of dependent variables of all fitted
  models. If `dv.labels = ""`, the row with names of dependent variables
  is omitted from the table.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- bootstrap:

  Logical, if `TRUE`, returns bootstrapped estimates..

- iterations:

  Numeric, number of bootstrap iterations (default is 1000).

- seed:

  Numeric, the number of the seed to replicate bootstrapped estimates.
  If `NULL`, uses random seed.

- vcov.fun:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix. See
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

- vcov.args:

  List of arguments to be passed to the function identified by the
  `vcov.fun` argument. This function is typically supplied by the
  sandwich or clubSandwich packages. Please refer to their documentation
  (e.g.,
  [`?sandwich::vcovHAC`](https://zeileis.codeberg.page/sandwich/reference/vcovHAC.html))
  to see the list of available arguments.

- string.pred:

  Character vector,used as headline for the predictor column. Default is
  `"Predictors"`.

- string.est:

  Character vector, used for the column heading of coefficients. Default
  is based on the response scale, e.g. for logistic regression models,
  `"Odds Ratios"` will be chosen, while for Poisson models it is
  `"Incidence Rate Ratios"` etc. Default if not specified is
  `"Estimate"`.

- string.std:

  Character vector, used for the column heading of standardized beta
  coefficients. Default is `"std. Beta"`.

- string.ci:

  Character vector, used for the column heading of confidence interval
  values. Default is `"CI"`.

- string.se:

  Character vector, used for the column heading of standard error
  values. Default is `"std. Error"`.

- string.std_se:

  Character vector, used for the column heading of standard error of
  standardized coefficients. Default is `"standardized std. Error"`.

- string.std_ci:

  Character vector, used for the column heading of confidence intervals
  of standardized coefficients. Default is `"standardized std. Error"`.

- string.p:

  String value, used for the column heading of p values. Default is
  `"p"`.

- string.std.p:

  Character vector, used for the column heading of p values. Default is
  `"std. p"`.

- string.df:

  Character vector, used for the column heading of degrees of freedom.
  Default is `"df"`.

- string.stat:

  Character vector, used for the test statistic. Default is
  `"Statistic"`.

- string.std.stat:

  Character vector, used for the test statistic. Default is
  `"std. Statistic"`.

- string.resp:

  Character vector, used for the column heading of of the response level
  for multinominal or categorical models. Default is `"Response"`.

- string.intercept:

  Character vector, used as name for the intercept parameter. Default is
  `"(Intercept)"`.

- strings:

  Named character vector, as alternative to arguments like `string.ci`
  or `string.p` etc. The name (lhs) must be one of the string-indicator
  from the aforementioned arguments, while the value (rhs) is the string
  that is used as column heading. E.g.,
  `strings = c(ci = "Conf.Int.", se = "std. Err")` would be equivalent
  to setting `string.ci = "Conf.Int.", string.se = "std. Err"`.

- ci.hyphen:

  Character vector, indicating the hyphen for confidence interval range.
  May be an HTML entity. See 'Examples'.

- minus.sign:

  string, indicating the minus sign for negative numbers. May be an HTML
  entity. See 'Examples'.

- collapse.ci:

  Logical, if `FALSE`, the CI values are shown in a separate table
  column.

- collapse.se:

  Logical, if `FALSE`, the SE values are shown in a separate table
  column.

- linebreak:

  Logical, if `TRUE` and `collapse.ci = FALSE` or `collapse.se = FALSE`,
  inserts a line break between estimate and CI resp. SE values. If
  `FALSE`, values are printed in the same line as estimate values.

- col.order:

  Character vector, indicating which columns should be printed and in
  which order. Column names that are excluded from `col.order` are not
  shown in the table output. However, column names that are included,
  are only shown in the table when the related argument (like `show.est`
  for `"estimate"`) is set to `TRUE` or another valid value. Table
  columns are printed in the order as they appear in `col.order`.

- digits:

  Amount of decimals for estimates

- digits.p:

  Amount of decimals for p-values

- digits.rsq:

  Amount of decimals for r-squared values

- digits.re:

  Amount of decimals for random effects part of the summary table.

- emph.p:

  Logical, if `TRUE`, significant p-values are shown bold faced.

- df.method, p.val:

  Method for computing degrees of freedom for p-values, standard errors
  and confidence intervals (CI). Only applies to mixed models. Use
  `df.method = "wald"` for a faster, but less precise computation. This
  will use the residual degrees of freedom (as returned by
  [`df.residual()`](https://rdrr.io/r/stats/df.residual.html)) for
  linear mixed models, and `Inf` degrees if freedom for all other model
  families. `df.method = "kenward"` (or `df.method = "kr"`) uses
  Kenward-Roger approximation for the degrees of freedom.
  `df.method = "satterthwaite"` uses Satterthwaite's approximation and
  `"ml1"` uses a "m-l-1" heuristic see
  [`degrees_of_freedom`](https://easystats.github.io/parameters/reference/degrees_of_freedom.html)
  for details). Use `show.df = TRUE` to show the approximated degrees of
  freedom for each coefficient.

- p.style:

  Character, indicating if p-values should be printed as numeric value
  (`"numeric"`), as 'stars' (asterisks) only (`"stars"`), or scientific
  (`"scientific"`). Scientific and numeric style can be combined with
  "stars", e.g. `"numeric_stars"`

- p.threshold:

  Numeric vector of length 3, indicating the treshold for annotating
  p-values with asterisks. Only applies if `p.style = "asterisk"`.

- p.adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for details.

- case:

  Desired target case. Labels will automatically converted into the
  specified character case. See `snakecase::to_any_case()` for more
  details on this argument. By default, if `case` is not specified, it
  will be set to `"parsed"`, unless `prefix.labels` is not `"none"`. If
  `prefix.labels` is either `"label"` (or `"l"`) or `"varname"` (or
  `"v"`) and `case` is not specified, it will be set to `NULL` - this is
  a more convenient default when prefixing labels.

- auto.label:

  Logical, if `TRUE` (the default), and [data is
  labelled](https://strengejacke.github.io/sjlabelled/articles/intro_sjlabelled.html),
  [`term_labels`](https://strengejacke.github.io/sjlabelled/reference/term_labels.html)
  is called to retrieve the labels of the coefficients, which will be
  used as predictor labels. If data is not labelled,
  [format_parameters()](https://easystats.github.io/parameters/reference/format_parameters.html)
  is used to create pretty labels. If `auto.label = FALSE`, original
  variable names and value labels (factor levels) are used.

- prefix.labels:

  Indicates whether the value labels of categorical variables should be
  prefixed, e.g. with the variable name or variable label. See argument
  `prefix` in
  [`term_labels`](https://strengejacke.github.io/sjlabelled/reference/term_labels.html)
  for details.

- bpe:

  For **Stan**-models (fitted with the rstanarm- or brms-package), the
  Bayesian point estimate is, by default, the median of the posterior
  distribution. Use `bpe` to define other functions to calculate the
  Bayesian point estimate. `bpe` needs to be a character naming the
  specific function, which is passed to the `fun`-argument in
  [`typical_value`](https://strengejacke.github.io/sjmisc/reference/typical_value.html).
  So, `bpe = "mean"` would calculate the mean value of the posterior
  distribution.

- CSS:

  A [`list`](https://rdrr.io/r/base/list.html) with user-defined
  style-sheet-definitions, according to the [official CSS
  syntax](https://www.w3.org/Style/CSS/). See 'Details' or [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/table_css.html).

- file:

  Destination file, if the output should be saved as file. If `NULL`
  (default), the output will be saved as temporary file and opened
  either in the IDE's viewer pane or the default web browser.

- use.viewer:

  Logical, if `TRUE`, the HTML table is shown in the IDE's viewer pane.
  If `FALSE` or no viewer available, the HTML table is opened in a web
  browser.

- encoding:

  Character vector, indicating the charset encoding used for variable
  and value labels. Default is `"UTF-8"`. For Windows Systems,
  `encoding = "Windows-1252"` might be necessary for proper display of
  special characters.

## Value

Invisibly returns

- the web page style sheet (`page.style`),

- the web page content (`page.content`),

- the complete html-output (`page.complete`) and

- the html-table with inline-css for use with knitr (`knitr`)

for further use.

## Details

### Standardized Estimates

Default standardization is done by completely refitting the model on the
standardized data. Hence, this approach is equal to standardizing the
variables before fitting the model, which is particularly recommended
for complex models that include interactions or transformations (e.g.,
polynomial or spline terms). When `show.std = "std2"`, standardization
of estimates follows Gelman's (2008) suggestion, rescaling the estimates
by dividing them by two standard deviations instead of just one.
Resulting coefficients are then directly comparable for untransformed
binary predictors. For backward compatibility reasons, `show.std` also
may be a logical value; if `TRUE`, normal standardized estimates are
printed (same effect as `show.std = "std"`). Use `show.std = NULL`
(default) or `show.std = FALSE`, if no standardization is required.

### How do I use `CSS`-argument?

With the `CSS`-argument, the visual appearance of the tables can be
modified. To get an overview of all style-sheet-classnames that are used
in this function, see return value `page.style` for details. Arguments
for this list have following syntax:

1.  the class-names with `"css."`-prefix as argument name and

2.  each style-definition must end with a semicolon

You can add style information to the default styles by using a +
(plus-sign) as initial character for the argument attributes. Examples:

- `css.table = 'border:2px solid red;'` for a solid 2-pixel table border
  in red.

- `css.summary = 'font-weight:bold;'` for a bold fontweight in the
  summary row.

- `css.lasttablerow = 'border-bottom: 1px dotted blue;'` for a blue
  dotted border of the last table row.

- `css.colnames = '+color:green'` to add green color formatting to
  column names.

- `css.arc = 'color:blue;'` for a blue text color each 2nd row.

- `css.caption = '+color:red;'` to add red font-color to the default
  table caption style.

## Note

The HTML tables can either be saved as file and manually opened (use
argument `file`) or they can be saved as temporary files and will be
displayed in the RStudio Viewer pane (if working with RStudio) or opened
with the default web browser. Displaying resp. opening a temporary file
is the default behaviour (i.e. `file = NULL`).  
  
Examples are shown in these three vignettes: [Summary of Regression
Models as HTML
Table](https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html),
[Summary of Mixed Models as HTML
Table](https://strengejacke.github.io/sjPlot/articles/tab_mixed.html)
and [Summary of Bayesian Models as HTML
Table](https://strengejacke.github.io/sjPlot/articles/tab_bayes.html).
