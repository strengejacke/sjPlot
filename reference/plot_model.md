# Plot regression models

`plot_model()` creates plots from regression models, either estimates
(as so-called forest or dot whisker plots) or marginal effects.

## Usage

``` r
plot_model(
  model,
  type = c("est", "re", "eff", "emm", "pred", "int", "std", "std2", "slope", "resid",
    "diag"),
  transform,
  terms = NULL,
  sort.est = NULL,
  rm.terms = NULL,
  group.terms = NULL,
  order.terms = NULL,
  pred.type = c("fe", "re"),
  mdrt.values = c("minmax", "meansd", "zeromax", "quart", "all"),
  ri.nr = NULL,
  title = NULL,
  axis.title = NULL,
  axis.labels = NULL,
  legend.title = NULL,
  wrap.title = 50,
  wrap.labels = 25,
  axis.lim = NULL,
  grid.breaks = NULL,
  ci.lvl = NULL,
  se = NULL,
  vcov.fun = NULL,
  vcov.args = NULL,
  colors = "Set1",
  show.intercept = FALSE,
  show.values = FALSE,
  show.p = TRUE,
  show.data = FALSE,
  show.legend = TRUE,
  show.zeroinf = TRUE,
  value.offset = NULL,
  value.size,
  jitter = NULL,
  digits = 2,
  dot.size = NULL,
  line.size = NULL,
  vline.color = NULL,
  p.threshold = c(0.05, 0.01, 0.001),
  p.val = NULL,
  p.adjust = NULL,
  grid,
  case,
  auto.label = TRUE,
  prefix.labels = c("none", "varname", "label"),
  bpe = "median",
  bpe.style = "line",
  bpe.color = "white",
  ci.style = c("whisker", "bar"),
  std.response = TRUE,
  ...
)

get_model_data(
  model,
  type = c("est", "re", "eff", "pred", "int", "std", "std2", "slope", "resid", "diag"),
  transform,
  terms = NULL,
  sort.est = NULL,
  rm.terms = NULL,
  group.terms = NULL,
  order.terms = NULL,
  pred.type = c("fe", "re"),
  ri.nr = NULL,
  ci.lvl = NULL,
  colors = "Set1",
  grid,
  case = "parsed",
  digits = 2,
  ...
)
```

## Arguments

- model:

  A regression model object. Depending on the `type`, many kinds of
  models are supported, e.g. from packages like stats, lme4, nlme,
  rstanarm, survey, glmmTMB, MASS, brms etc.

- type:

  Type of plot. There are three groups of plot-types:  
    
  *Coefficients* ([related
  vignette](https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html))

  `type = "est"`

  :   Forest-plot of estimates. If the fitted model only contains one
      predictor, slope-line is plotted.

  `type = "re"`

  :   For mixed effects models, plots the random effects.

  `type = "std"`

  :   Forest-plot of standardized coefficients.

  `type = "std2"`

  :   Forest-plot of standardized coefficients, however, standardization
      is done by dividing by two SD (see 'Details').

  *Marginal Effects* ([related
  vignette](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html))

  `type = "pred"`

  :   Predicted values (marginal effects) for specific model terms. See
      [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
      for details.

  `type = "eff"`

  :   Similar to `type = "pred"`, however, discrete predictors are held
      constant at their proportions (not reference level). See
      [`ggeffect`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
      for details.

  `type = "emm"`

  :   Similar to `type = "eff"`, see
      [`ggemmeans`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
      for details.

  `type = "int"`

  :   Marginal effects of interaction terms in `model`.

  *Model diagnostics*

  `type = "slope"`

  :   Slope of coefficients for each single predictor, against the
      response (linear relationship between each model term and
      response). See 'Details'.

  `type = "resid"`

  :   Slope of coefficients for each single predictor, against the
      residuals (linear relationship between each model term and
      residuals). See 'Details'.

  `type = "diag"`

  :   Check model assumptions. See 'Details'.

  **Note:** For mixed models, the diagnostic plots like linear
  relationship or check for Homoscedasticity, do **not** take the
  uncertainty of random effects into account, but is only based on the
  fixed effects part of the model.

- transform:

  A character vector, naming a function that will be applied on
  estimates and confidence intervals. By default, `transform` will
  automatically use `"exp"` as transformation for applicable classes of
  `model` (e.g. logistic or poisson regression). Estimates of linear
  models remain untransformed. Use `NULL` if you want the raw,
  non-transformed estimates.

- terms:

  Character vector with the names of those terms from `model` that
  should be plotted. This argument depends on the plot-type:

  *Coefficients*

  :   Select terms that should be plotted. All other term are removed
      from the output. Note that the term names must match the names of
      the model's coefficients. For factors, this means that the
      variable name is suffixed with the related factor level, and each
      category counts as one term. E.g. `rm.terms = "t_name [2,3]"`
      would remove the terms `"t_name2"` and `"t_name3"` (assuming that
      the variable `t_name` is categorical and has at least the factor
      levels `2` and `3`). Another example for the *iris*-dataset:
      `terms = "Species"` would not work, instead you would write
      `terms = "Species [versicolor,virginica]"` to remove these two
      levels, or `terms = "Speciesversicolor"` if you just want to
      remove the level *versicolor* from the plot.

  *Marginal Effects*

  :   Here `terms` indicates for which terms marginal effects should be
      displayed. At least one term is required to calculate effects,
      maximum length is three terms, where the second and third term
      indicate the groups, i.e. predictions of first term are grouped by
      the levels of the second (and third) term. `terms` may also
      indicate higher order terms (e.g. interaction terms). Indicating
      levels in square brackets allows for selecting only specific
      groups. Term name and levels in brackets must be separated by a
      whitespace character, e.g. `terms = c("age", "education [1,3]")`.
      It is also possible to specify a range of numeric values for the
      predictions with a colon, for instance
      `terms = c("education [1,3]", "age [30:50]")`. Furthermore, it is
      possible to specify a function name. Values for predictions will
      then be transformed, e.g. `terms = "income [exp]"`. This is useful
      when model predictors were transformed for fitting the model and
      should be back-transformed to the original scale for predictions.
      Finally, numeric vectors for which no specific values are given, a
      "pretty range" is calculated, to avoid memory allocation problems
      for vectors with many unique values. If a numeric vector is
      specified as second or third term (i.e. if this vector represents
      a grouping structure), representative values (see
      [`values_at`](https://strengejacke.github.io/ggeffects/reference/values_at.html))
      are chosen. If all values for a numeric vector should be used to
      compute predictions, you may use e.g. terms = "age \[all\]". For
      more details, see
      [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html).

- sort.est:

  Determines in which way estimates are sorted in the plot:

  - If `NULL` (default), no sorting is done and estimates are sorted in
    the same order as they appear in the model formula.

  - If `TRUE`, estimates are sorted in descending order, with highest
    estimate at the top.

  - If `sort.est = "sort.all"`, estimates are re-sorted for each
    coefficient (only applies if `type = "re"` and `grid = FALSE`), i.e.
    the estimates of the random effects for each predictor are sorted
    and plotted to an own plot.

  - If `type = "re"`, specify a predictor's / coefficient's name to sort
    estimates according to this random effect.

- rm.terms:

  Character vector with names that indicate which terms should be
  removed from the plot. Counterpart to `terms`. `rm.terms = "t_name"`
  would remove the term *t_name*. Default is `NULL`, i.e. all terms are
  used. For factors, levels that should be removed from the plot need to
  be explicitely indicated in square brackets, and match the model's
  coefficient names, e.g. `rm.terms = "t_name [2,3]"` would remove the
  terms `"t_name2"` and `"t_name3"` (assuming that the variable `t_name`
  was categorical and has at least the factor levels `2` and `3`).
  Another example for the *iris* dataset would be
  `rm.terms = "Species [versicolor,virginica]"`. Note that the
  `rm.terms`-argument does not apply to *Marginal Effects* plots.

- group.terms:

  Numeric vector with group indices, to group coefficients. Each group
  of coefficients gets its own color (see 'Examples').

- order.terms:

  Numeric vector, indicating in which order the coefficients should be
  plotted. See examples in [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html).

- pred.type:

  Character, only applies for *Marginal Effects* plots with mixed
  effects models. Indicates whether predicted values should be
  conditioned on random effects (`pred.type = "re"`) or fixed effects
  only (`pred.type = "fe"`, the default). For details, see documentation
  of the `type`-argument in
  [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html).

- mdrt.values:

  Indicates which values of the moderator variable should be used when
  plotting interaction terms (i.e. `type = "int"`).

  `"minmax"`

  :   (default) minimum and maximum values (lower and upper bounds) of
      the moderator are used to plot the interaction between independent
      variable and moderator(s).

  `"meansd"`

  :   uses the mean value of the moderator as well as one standard
      deviation below and above mean value to plot the effect of the
      moderator on the independent variable (following the convention
      suggested by Cohen and Cohen and popularized by Aiken and West
      (1991), i.e. using the mean, the value one standard deviation
      above, and the value one standard deviation below the mean as
      values of the moderator, see [Grace-Martin K: 3 Tips to Make
      Interpreting Moderation Effects
      Easier](https://www.theanalysisfactor.com/3-tips-interpreting-moderation/)).

  `"zeromax"`

  :   is similar to the `"minmax"` option, however, `0` is always used
      as minimum value for the moderator. This may be useful for
      predictors that don't have an empirical zero-value, but absence of
      moderation should be simulated by using 0 as minimum.

  `"quart"`

  :   calculates and uses the quartiles (lower, median and upper) of the
      moderator value.

  `"all"`

  :   uses all values of the moderator variable.

- ri.nr:

  Numeric vector. If `type = "re"` and fitted model has more than one
  random intercept, `ri.nr` indicates which random effects of which
  random intercept (or: which list elements of
  [`ranef`](https://rdrr.io/pkg/nlme/man/random.effects.html)) will be
  plotted. Default is `NULL`, so all random effects will be plotted.

- title:

  Character vector, used as plot title. By default,
  [`response_labels`](https://strengejacke.github.io/sjlabelled/reference/term_labels.html)
  is called to retrieve the label of the dependent variable, which will
  be used as title. Use `title = ""` to remove title.

- axis.title:

  Character vector of length one or two (depending on the plot function
  and type), used as title(s) for the x and y axis. If not specified, a
  default labelling is chosen. **Note:** Some plot types may not support
  this argument sufficiently. In such cases, use the returned
  ggplot-object and add axis titles manually with
  [`labs`](https://ggplot2.tidyverse.org/reference/labs.html). Use
  `axis.title = ""` to remove axis titles.

- axis.labels:

  Character vector with labels for the model terms, used as axis labels.
  By default,
  [`term_labels`](https://strengejacke.github.io/sjlabelled/reference/term_labels.html)
  is called to retrieve the labels of the coefficients, which will be
  used as axis labels. Use `axis.labels = ""` or `auto.label = FALSE` to
  use the variable names as labels instead. If `axis.labels` is a named
  vector, axis labels (by default, the names of the model's
  coefficients) will be matched with the names of `axis.label`. This
  ensures that labels always match the related axis value, no matter in
  which way axis labels are sorted.

- legend.title:

  Character vector, used as legend title for plots that have a legend.

- wrap.title:

  Numeric, determines how many chars of the plot title are displayed in
  one line and when a line break is inserted.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- axis.lim:

  Numeric vector of length 2, defining the range of the plot axis.
  Depending on plot-type, may effect either x- or y-axis. For *Marginal
  Effects* plots, `axis.lim` may also be a list of two vectors of length
  2, defining axis limits for both the x and y axis.

- grid.breaks:

  Numeric value or vector; if `grid.breaks` is a single value, sets the
  distance between breaks for the axis at every `grid.breaks`'th
  position, where a major grid line is plotted. If `grid.breaks` is a
  vector, values will be used to define the axis positions of the major
  grid lines.

- ci.lvl:

  Numeric, the level of the confidence intervals (error bars). Use
  `ci.lvl = NA` to remove error bars. For `stanreg`-models, `ci.lvl`
  defines the (outer) probability for the *credible interval* that is
  plotted (see
  [`ci`](https://easystats.github.io/bayestestR/reference/ci.html)). By
  default, `stanreg`-models are printed with two intervals: the "inner"
  interval, which defaults to the 50%-CI; and the "outer" interval,
  which defaults to the 89%-CI. `ci.lvl` affects only the outer interval
  in such cases. See `prob.inner` and `prob.outer` under the
  `...`-argument for more details.

- se:

  Logical, if `TRUE`, the standard errors are also printed. If robust
  standard errors are required, use arguments `vcov.fun` and `vcov.args`
  (see
  [`standard_error`](https://easystats.github.io/parameters/reference/standard_error.html)
  for details). `se` overrides `ci.lvl`: if not `NULL`, arguments
  `ci.lvl` and `transform` will be ignored. Currently, `se` only applies
  to *Coefficients* plots.

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

- colors:

  May be a character vector of color values in hex-format, valid color
  value names (see `demo("colors")`) or a name of a pre-defined color
  palette. Following options are valid for the `colors` argument:

  - If not specified, a default color brewer palette will be used, which
    is suitable for the plot style.

  - If `"gs"`, a greyscale will be used.

  - If `"bw"`, and plot-type is a line-plot, the plot is black/white and
    uses different line types to distinguish groups (see [this
    package-vignette](https://strengejacke.github.io/sjPlot/articles/blackwhitefigures.html)).

  - If `colors` is any valid color brewer palette name, the related
    palette will be used. Use
    [`RColorBrewer::display.brewer.all()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)
    to view all available palette names.

  - There are some pre-defined color palettes in this package, see
    [`sjPlot-themes`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)
    for details.

  - Else specify own color values or names as vector (e.g.
    `colors = "#00ff00"` or `colors = c("firebrick", "blue")`).

- show.intercept:

  Logical, if `TRUE`, the intercept of the fitted model is also plotted.
  Default is `FALSE`. If `transform = "exp"`, please note that due to
  exponential transformation of estimates, the intercept in some cases
  is non-finite and the plot can not be created.

- show.values:

  Logical, whether values should be plotted or not.

- show.p:

  Logical, adds asterisks that indicate the significance level of
  estimates to the value labels.

- show.data:

  Logical, for *Marginal Effects* plots, also plots the raw data points.

- show.legend:

  For *Marginal Effects* plots, shows or hides the legend.

- show.zeroinf:

  Logical, if `TRUE`, shows the zero-inflation part of hurdle- or
  zero-inflated models.

- value.offset:

  Numeric, offset for text labels to adjust their position relative to
  the dots or lines.

- value.size:

  Numeric, indicates the size of value labels. Can be used for all plot
  types where the argument `show.values` is applicable, e.g.
  `value.size = 4`.

- jitter:

  Numeric, between 0 and 1. If `show.data = TRUE`, you can add a small
  amount of random variation to the location of each data point.
  `jitter` then indicates the width, i.e. how much of a bin's width will
  be occupied by the jittered values.

- digits:

  Numeric, amount of digits after decimal point when rounding estimates
  or values.

- dot.size:

  Numeric, size of the dots that indicate the point estimates.

- line.size:

  Numeric, size of the lines that indicate the error bars.

- vline.color:

  Color of the vertical "zero effect" line. Default color is inherited
  from the current theme.

- p.threshold:

  Numeric vector of length 3, indicating the treshold for annotating
  p-values with asterisks. Only applies if `p.style = "asterisk"`.

- p.val:

  Character specifying method to be used to calculate p-values. Defaults
  to "profile" for glm/polr models, otherwise "wald".

- p.adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for details.

- grid:

  Logical, if `TRUE`, multiple plots are plotted as grid layout.

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

- bpe.style:

  For **Stan**-models (fitted with the rstanarm- or brms-package), the
  Bayesian point estimate is indicated as a small, vertical line by
  default. Use `bpe.style = "dot"` to plot a dot instead of a line for
  the point estimate.

- bpe.color:

  Character vector, indicating the color of the Bayesian point estimate.
  Setting `bpe.color = NULL` will inherit the color from the mapped
  aesthetic to match it with the geom's color.

- ci.style:

  Character vector, defining whether inner and outer intervals for
  Bayesion models are shown in boxplot-style (`"whisker"`) or in bars
  with different alpha-levels (`"bar"`).

- std.response:

  Logical, whether the response variable will also be standardized if
  standardized coefficients are requested. Setting both
  `std.response = TRUE` and `show.std = TRUE` will behave as if the
  complete data was standardized before fitting the model.

- ...:

  Other arguments, passed down to various functions. Here is a list of
  supported arguments and their description in detail.

  `prob.inner` and `prob.outer`

  :   For **Stan**-models (fitted with the rstanarm- or brms-package)
      and coefficients plot-types, you can specify numeric values
      between 0 and 1 for `prob.inner` and `prob.outer`, which will then
      be used as inner and outer probabilities for the uncertainty
      intervals (HDI). By default, the inner probability is 0.5 and the
      outer probability is 0.89 (unless `ci.lvl` is specified - in this
      case, `ci.lvl` is used as outer probability).

  `size.inner`

  :   For **Stan**-models and *Coefficients* plot-types, you can specify
      the width of the bar for the inner probabilities. Default is
      `0.1`. Setting `size.inner = 0` removes the inner probability
      regions.

  `width`, `alpha`, and `scale`

  :   Passed down to
      [`ggplot2::geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
      or `geom_density_ridges()`, for forest or diagnostic plots.

  `width`, `alpha`, `dot.alpha`, `dodge` and `log.y`

  :   Passed down to
      [`plot.ggeffects`](https://strengejacke.github.io/ggeffects/reference/plot.html)
      for *Marginal Effects* plots.

  `show.loess`

  :   Logical, for diagnostic plot-types `"slope"` and `"resid"`, adds
      (or hides) a loess-smoothed line to the plot.

  *Marginal Effects* plot-types

  :   When plotting marginal effects, arguments are also passed down to
      [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html),
      [`ggeffect`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
      or
      [`plot.ggeffects`](https://strengejacke.github.io/ggeffects/reference/plot.html).

  Case conversion of labels

  :   For case conversion of labels (see argument `case`), arguments
      `sep_in` and `sep_out` will be passed down to
      `snakecase::to_any_case()`. This only applies to automatically
      retrieved term labels, *not* if term labels are provided by the
      `axis.labels`-argument.

## Value

Depending on the plot-type, `plot_model()` returns a `ggplot`-object or
a list of such objects. `get_model_data` returns the associated data
with the plot-object as tidy data frame, or (depending on the plot-type)
a list of such data frames.

## Details

### Different Plot Types

- `type = "std"`:

  Plots standardized estimates. See details below.

- `type = "std2"`:

  Plots standardized estimates, however, standardization follows
  Gelman's (2008) suggestion, rescaling the estimates by dividing them
  by two standard deviations instead of just one. Resulting coefficients
  are then directly comparable for untransformed binary predictors.

- `type = "pred"`:

  Plots estimated marginal means (or marginal effects). Simply wraps
  [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html).
  See also [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html).

- `type = "eff"`:

  Plots estimated marginal means (or marginal effects). Simply wraps
  [`ggeffect`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html).
  See also [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html).

- `type = "int"`:

  A shortcut for marginal effects plots, where interaction terms are
  automatically detected and used as `terms`-argument. Furthermore, if
  the moderator variable (the second - and third - term in an
  interaction) is continuous, `type = "int"` automatically chooses
  useful values based on the `mdrt.values`-argument, which are passed to
  `terms`. Then,
  [`ggpredict`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
  is called. `type = "int"` plots the interaction term that appears
  first in the formula along the x-axis, while the second (and possibly
  third) variable in an interaction is used as grouping factor(s)
  (moderating variable). Use `type = "pred"` or `type = "eff"` and
  specify a certain order in the `terms`-argument to indicate which
  variable(s) should be used as moderator. See also [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/plot_interactions.html).

- `type = "slope"` and `type = "resid"`:

  Simple diagnostic-plots, where a linear model for each single
  predictor is plotted against the response variable, or the model's
  residuals. Additionally, a loess-smoothed line is added to the plot.
  The main purpose of these plots is to check whether the relationship
  between outcome (or residuals) and a predictor is roughly linear or
  not. Since the plots are based on a simple linear regression with only
  one model predictor at the moment, the slopes (i.e. coefficients) may
  differ from the coefficients of the complete model.

- `type = "diag"`:

  For **Stan-models**, plots the prior versus posterior samples. For
  **linear (mixed) models**, plots for multicollinearity-check (Variance
  Inflation Factors), QQ-plots, checks for normal distribution of
  residuals and homoscedasticity (constant variance of residuals) are
  shown. For **generalized linear mixed models**, returns the QQ-plot
  for random effects.

### Standardized Estimates

Default standardization is done by completely refitting the model on the
standardized data. Hence, this approach is equal to standardizing the
variables before fitting the model, which is particularly recommended
for complex models that include interactions or transformations (e.g.,
polynomial or spline terms). When `type = "std2"`, standardization of
estimates follows Gelman's (2008) suggestion, rescaling the estimates by
dividing them by two standard deviations instead of just one. Resulting
coefficients are then directly comparable for untransformed binary
predictors.

## References

Gelman A (2008) "Scaling regression inputs by dividing by two standard
deviations." *Statistics in Medicine 27: 2865-2873.*  
  
Aiken and West (1991). Multiple Regression: Testing and Interpreting
Interactions.

## Examples
