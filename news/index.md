# Changelog

## sjPlot 2.9.0

CRAN release: 2025-07-10

- Fixed random-effect intervals in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  for `glmmTMB` models with one or more grouping factors
  ([\#978](https://github.com/strengejacke/sjPlot/issues/978)).

- Fixed corrupted labels in the inline HTML output of
  [`view_df()`](https://strengejacke.github.io/sjPlot/reference/view_df.md)
  ([\#953](https://github.com/strengejacke/sjPlot/issues/953)).

- Fix namespace clash with new ggplot2 version.

- Fix confusing warning message.

- Fix incorrect labeling of coefficients when `transform = NULL` with a
  probit model.

- Corrected documentation for
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  and
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  regarding the `p.adjust` argument.

## sjPlot 2.8.17

CRAN release: 2024-11-29

### Bug fixes

- Fixed issues with renamed arguments in upstream package *ggeffects*.

- Several minor bug fixes.

## sjPlot 2.8.15

CRAN release: 2023-08-17

- Minor fix in vignettes.

## sjPlot 2.8.14

CRAN release: 2023-04-02

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md),
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  get a `std.response` argument, to include or exclude the response
  variable from standardization.

### Bug fixes

- Fixed test issues when no internet connection avaiable.

- Minor fixes and improvements.

## sjPlot 2.8.13

CRAN release: 2023-03-13

### Bug fixes

- Fixed issue with `knit_print()` generics.

## sjPlot 2.8.12

CRAN release: 2022-11-19

### General

- Minor changes to work properly with updates of upstream packages and
  various smaller bug fixes and improvements.

## sjPlot 2.8.11

CRAN release: 2022-08-07

### General

- Minor changes to work properly with updates of upstream packages and
  various smaller bug fixes and improvements.

## sjPlot 2.8.10

CRAN release: 2021-11-26

### General

- Minor changes to work properly with updates of downstream packages and
  various smaller bug fixes and improvements.

## sjPlot 2.8.9

CRAN release: 2021-07-10

### Bug fixes

- Fixed issues from CRAN checks.

## sjPlot 2.8.8

CRAN release: 2021-05-25

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now works properly with forthcoming *parameters* update.

### Bug fixes

- [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  did not work properly for Bayesian models.

## sjPlot 2.8.7

CRAN release: 2021-01-10

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  also gains an `encoding` argument.
- [`tab_df()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  and
  [`tab_dfs()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  no longer set the argument `show.rownames` to `TRUE`. Therefore, both
  functions now use row numbers as row names, if no other rownames are
  present.
- [`tab_dfs()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  also gains a `digits` argument.

### Bug fixes

- Argument `df.method` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  did not accept all available options that were documented.
- Fix CRAN check issues (warnings in new R-devel).

## sjPlot 2.8.6

CRAN release: 2020-10-28

### Changes to functions

- When `dv.labels = ""` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md),
  the row with names of dependent variables is omitted.

### Bug fixes

- Fix CRAN check issues (removed cross-references to archived packages).
- The `minus.sign` argument in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now works.
- `show.std = TRUE` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  did not exponentiate standardized coefficients for non-Gaussian
  models.

## sjPlot 2.8.5

CRAN release: 2020-09-24

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gains an argument `df.method`, which will replace the less generic
  `p.val` argument in the future. Currently, `df.method` is an alias of
  `p.val`.

### Bug fixes

- Fixed issue with wrong n’s in
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md)
  when weights were applied.
- Fixed issue
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md)
  when weights were applied and items should be sorted.
- Fixed issue in
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  for models without intercept.
- Fixed issue for wrong legend labelling in
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  when showing p-stars.
- Fixed issue in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  with `type = "int"` in detecting interaction terms when these were
  partly in parenthesis (like `a * (b + c)`).
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with arguments `show.stat = TRUE` and `show.std = TRUE`, where the
  related statistic and CI columns for standardized coefficients were
  not shown.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  for *brmsfit* models that did no longer show random effects
  information after the last update from the *performance* package.
- Fixed issue with argument `show.rownames` in
  [`tab_df()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md).

## sjPlot 2.8.4

CRAN release: 2020-05-24

### Changes to functions

- The robust estimation (argument `vcov.fun` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  or
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md))
  now also uses and thus accepts estimation-types from package
  *clubSandwich*.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now accepts all options for `p.val` that are supported by
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
- The `p.style` argument in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  was slightly revised, and now also accepts `"scientific"` as option
  for scientific notation of p-values.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `digits.re` argument to define decimal part of the random
  effects summary.
- [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  gains `value.size` and `line.size` arguments, similar to
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md).
- [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  should sort coefficients in their natural order now.

### Bug fixes

- Fixed bug in
  [`plot_xtab()`](https://strengejacke.github.io/sjPlot/reference/plot_xtab.md)
  with wrong order of legend labels.
- Fixed bug in
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  with wrong axis title for exponentiated coefficients.
- Fixed bug in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  that did not show standard error of standardized coefficients when
  `show.se = TRUE`.

## sjPlot 2.8.3

CRAN release: 2020-03-09

### General

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  and
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now support *clogit* models (requires latest update of package
  **insight**).

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `p.adjust` argument to adjust p-values for multiple
  comparisons.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md),
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  get a `robust`-argument to easily compute standard errors, confidence
  intervals and p-values based on robust estimation of the
  variance-covariance matrix. `robust` is just a convenient shortcut for
  `vcov.fun` and `vcov.type`.

### Bug fixes

- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  and
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  for certain cases when coefficients could not be estimated and were
  `NA`.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with `collapse.ci` for Bayesian models.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  when `p.val="kr"` and `show.df=TRUE`.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with formatting issues of p-values when standardized coefficients
  where requested.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  due to changes in other packages *sjPlot* depends on.

## sjPlot 2.8.2

CRAN release: 2020-01-23

### Function renaming

- [`sjt.itemanalysis()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md)
  is now named
  [`tab_itemscale()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md).
- [`sjt.xtab()`](https://strengejacke.github.io/sjPlot/reference/tab_xtab.md)
  is now named
  [`tab_xtab()`](https://strengejacke.github.io/sjPlot/reference/tab_xtab.md).

### Changes to functions

- Improved handling for
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  of robust estimation in general and Kenward-Roger or Satterthwaite
  approximations in particular for linear mixed models.
- Revised code to cope with forthcoming *tidyselect*-update.

### Bug fixes

- Improved
  [`tab_df()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  now uses value labels for factors instead of numeric values.
- Fixed some issues related to the lates *brms*-update.

## sjPlot 2.8.1

CRAN release: 2019-12-03

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets arguments `bootstrap`, `iterations` and `seed` to return
  bootstrapped estimates.

### Bug fixes

- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with detecting labels when `auto.label = TRUE`.
- Fixed issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  for negative binomial hurdle mixed models (i.e. *glmmTMB* models with
  truncated negative-binomial family).
- Fixed bug in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with `show.reflvl = TRUE`.
- Fixed bug in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  where labels for coefficients where not matching the correct
  coefficients.

## sjPlot 2.8.0

CRAN release: 2019-11-18

### Breaking changes

- Cluster functions have been removed, as these are now re-implemented
  in the **parameters** package.

### General

- Standardization of model parameters (in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  or
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md))
  now uses standardization based on refitting the model.

### Changes to functions

- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  gets `type = "emm"` as marginal effects plot type, which is similar to
  `type = "eff"`. See [Plotting Marginal Effects of Regression
  Models](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html)
  for details.
- The `verbose`-argument in
  [`view_df()`](https://strengejacke.github.io/sjPlot/reference/view_df.md)
  now defaults to `FALSE`.
- Updated and re-arranged internal color palette, especially to have a
  better behaviour when selecting colors from continuous palettes (see
  `show_pals()`).

### Bug fixes

- `sort.est = NULL` in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now preserves original order of coefficients.
- Fixed bug in automatic axis labelling for
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  for non-labelled, numeric values.
- Fixed bug in
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  when plotting factors.
- Arguments `string.std_ci` and `string.std_se` are no longer ignored in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).

## sjPlot 2.7.2

CRAN release: 2019-09-29

### General

- Replaced `performance::principal_component()` by
  `parameters::principal_component()`.
- Fixed CRAN check issues, due to the latest *bayestestR* update.

### Function renaming

- `sjp.grpfrq()` is now names
  [`plot_grpfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_grpfrq.md).
- `sjp.xtab()` is now names
  [`plot_xtab()`](https://strengejacke.github.io/sjPlot/reference/plot_xtab.md).

### Changes to functions

- [`plot_grid()`](https://strengejacke.github.io/sjPlot/reference/plot_grid.md)
  gets a `tags`-argument to add tags to plot-panels.

### Bug fixes

- Fixed bug in
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md)
  for data frames with many missing values.
- Fixed bug with sorting frequencies in
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  when vector had more labels than values.
- Fixed bug in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  where `show.reflvl = TRUE` did not insert the reference category in
  first place, but in alphabetical order.

## sjPlot 2.7.1

CRAN release: 2019-09-07

### General

- Minor revisions to meet the changes in the forthcoming update from
  tidyr.
- new color palettes were added (see
  [`show_sjplot_pals()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)).

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now supports *gamlss* models.
- [`tab_df()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  gets a `digits` argument, to round numeric values in output.

### Bug fixes

- Fixed bug in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  with `show.df = TRUE` for *lmerModLmerTest*.
- Fixed bug in
  [`tab_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/tab_stackfrq.md)
  when items had different amount of valid values.

## sjPlot 2.7.0

CRAN release: 2019-08-02

### Renamed functions

- `sjp.stackfrq()` was renamed to
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md).
- `sjt.stackfrq()` was renamed to
  [`tab_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/tab_stackfrq.md).

### Changes to functions

#### `plot_likert()`

- showed category labels in the top and bottom legends in two rows if
  there are more than six categories. Also, the categories are ordered
  column wise instead of row wise. This behaviour can now be controlled
  for grouped likert plots, using `group.legend.options`. The ordering
  now defaults to row wise and the user can force all categories onto a
  single row.
- now automatically adjusts labels to avoid overlapping.

#### `tab_model()`

- now supports `wbm()`-models from the *panelr*-package.
- gets a `show.aicc`-argument to show the second order AIC.
- gets a `show.reflvl`-argument to show the reference level of factors.
- gets a `string.std_se` and `string.std_ci`-argument to change the
  column header for standard errors and confidence intervals of
  standardized coefficients.
- no longer prints a message that default p-values for mixed models are
  based on Wald approximation.
- `show.ci50` defaults to `FALSE` now.

#### `sjt.itemanalysis()`

- [`sjt.itemanalysis()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md)
  now works on ordered factors. A clearer error message was added when
  unordered factors are used. The old error message was not helpful.
- The `factor.groups` argument can now be `"auto"` to detect factor
  groups based on a pca with Varimax rotation.

#### `sjp.stackrq()`

- `sjp.stackfrq()` was renamed to
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md).
- `sjp.stackfrq()` (now named:
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md))
  gets a `show.n`-argument to also show count values. This option can be
  combined with `show.prc`.
- `sjp.stackfrq()` (now named:
  [`plot_stackfrq()`](https://strengejacke.github.io/sjPlot/reference/plot_stackfrq.md))
  now also works on grouped data frames.

#### changes to other functions

- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now supports `wbm()`-models from the *panelr*-package.

### Bug fixes

- `plot_model(type = "int")` now also recognized interaction terms with
  `:` in formula.
- Argument `string.est` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  did not overwrite the default label for the estimate-column-header.
- Minor fix in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  for mixed models that can’t compute R2.
- Fix issue in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  when printing robust standard errors and CI (i.e. when using arguments
  `vcov*`).
- The
  [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md)
  option `reverse.scale = TRUE` resulted in `values = "sum.inside"`
  being outside and the other way around. This is fixed now.
- [`view_df()`](https://strengejacke.github.io/sjPlot/reference/view_df.md)
  mixed up labels and frequency values when value labels were present,
  but no such values were in the data.
- Argument `wrap.labels` in
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  did not properly work for factor levels.
- Fix issue in
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  that stopped for some models.
- Fix issue in `sjt.stackfrq()`, when `show.na = TRUE` and some items
  had zero-values.

## sjPlot 2.6.3

CRAN release: 2019-04-27

### General

- Export
  [`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html), to
  meet changes in dplyr 0.8.0.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now support `MixMod`-objects from package **GLMMadpative**, `mlogit`-
  and `gmnl`-models.

### Renamed functions

- `sjp.kfold_cv()` was renamed to `plot_kfold_cv()`.
- `sjp.frq()` was renamed to
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md).

### Changes to functions

#### tab_model()

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `show.ngrps`-argument, which adds back the functionality to
  print the number of random effects groups for mixed models.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `show.loglik`-argument, which adds back the functionality to
  print the model’s log-Likelihood.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `strings`-argument, as convenient shortcut for setting
  column-header strings.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets additional arguments `vcov.fun`, `vcov.type` and `vcov.args` that
  are passed down to
  [`sjstats::robust()`](https://strengejacke.github.io/sjstats/reference/r2.html),
  to calculate different types of (clustered) robust standard errors.
- The `p.style`-argument now also allows printing both numeric p-values
  and asterisks, by using `p.style = "both"`.

#### plot_likert()

- [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md)
  gets a `reverse.scale` argument to reverse the order of categories, so
  positive and negative values switch position.
- [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md)
  gets a `groups` argument, to group items in the plot (thanks to
  [@ndevln](https://github.com/ndevln)).
- Argument `grid.range` in
  [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md)
  now may also be a vector of length 2, to define diffent length for the
  left and right x-axis scales.

#### Other

- [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  (former `sjp.frq()`) now has pipe-consistent syntax, enables plotting
  multiple variables in one function call and supports grouped data
  frames.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  gets additional arguments `vcov.fun`, `vcov.type` and `vcov.args` that
  are passed down to
  [`sjstats::robust()`](https://strengejacke.github.io/sjstats/reference/r2.html),
  to calculate different types of (clustered) robust standard errors.
- [`sjt.xtab()`](https://strengejacke.github.io/sjPlot/reference/tab_xtab.md),
  `sjp.xtab()`,
  [`plot_frq()`](https://strengejacke.github.io/sjPlot/reference/plot_frq.md)
  and `sjp.grpfrq()` get a `drop.empty()`-argument, to drop values /
  factor levels with no observations from output.

### Bug fixes

- Legend labels were inverted for **brms**-models in
  `plot_model(..., type = "diag")`.
- Legend labels were duplicated for marginal effects plots when
  `color ="bw"` and `legend.title` was specified.
- Fixed encoding issues with help-files.
- [`view_df()`](https://strengejacke.github.io/sjPlot/reference/view_df.md)
  did not truncate frequency- and percentage-values for variables where
  value labels were truncated to a certain maximum number.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  did not print number of observations for `coxph`-models.

## sjPlot 2.6.2

CRAN release: 2018-12-18

### General

- Revised some help-files and vignettes.

### Removed / Defunct

Following functions are now defunct:

- `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and `sjt.glmer()`. Please use
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  instead.

### Changes to functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  supports printing simplex parameters of monotonic effects of **brms**
  models.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `prefix.labels`-argument to add a prefix to the labels of
  categorical terms.
- The `rotation`-argument in `sjt.pca()` and `sjp.pca()` now supports
  all rotations from
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html).

### Bug fixes

- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  no longer automatically changes the plot-type to `"slope"` for models
  with only one predictor that is categorical and has more than two
  levels.
- `type = "eff"` and `type = "pred"` in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  did not work when `terms` was not specified.
- If robust standard errors are requested in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md),
  the confidence intervals and p-values are now re-calculated and
  adjusted based on the robust standard errors.
- `colors = "bw"` was not recognized correctly for
  `plot_model(..., type = "int")`.
- Fix issue in `sjp.frq()` with correct axis labels for non-labelled
  character vectors.

## sjPlot 2.6.1

CRAN release: 2018-10-14

### General

- Removed defunct functions.

### Deprecated

- `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and `sjt.glmer()` are now
  deprecated. Please use
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  instead.

### Changes to functions

- Arguments `dot.size` and `line.size` in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now also apply to marginal effects and diagnostic plots.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now uses a free x-axis scale in facets for models with zero-inflated
  part.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now shows multiple plots for models with zero-inflated parts when
  `grids = FALSE`.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `p.style` and `p.threshold` argument to indicate significance
  levels as asteriks, and to determine the threshold for which an
  estimate is considered as significant.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  get a `p.threshold` argument to determine the threshold for which an
  estimate is considered as significant.

### Bug fixes

- Fixed bug from the last update that made value labels disappear for
  [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md).
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  now also accepts multiple model-objects stored in a `list` as
  argument, as stated in the help-file.
- The `file`-argument now works again in
  [`sjt.itemanalysis()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md).
- Argument `show.ci` in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  did not compute confidence intervals for different levels.

## sjPlot 2.6.0

CRAN release: 2018-08-23

### General

- `sjp.scatter()` was revised and renamed to
  [`plot_scatter()`](https://strengejacke.github.io/sjPlot/reference/plot_scatter.md).
  [`plot_scatter()`](https://strengejacke.github.io/sjPlot/reference/plot_scatter.md)
  is pipe-friendly, and also works on grouped data frames.
- `sjp.gpt()` was revised and renamed to
  [`plot_gpt()`](https://strengejacke.github.io/sjPlot/reference/plot_gpt.md).
  [`plot_gpt()`](https://strengejacke.github.io/sjPlot/reference/plot_gpt.md)
  is pipe-friendly, and also works on grouped data frames.
- Reduce package dependencies.

### Renamed functions

- `sjp.scatter()` was renamed to
  [`plot_scatter()`](https://strengejacke.github.io/sjPlot/reference/plot_scatter.md).
- `sjp.likert()` was renamed to
  [`plot_likert()`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md).
- `sjp.gpt()` was renamed to
  [`plot_gpt()`](https://strengejacke.github.io/sjPlot/reference/plot_gpt.md).
- `sjp.resid()` was renamed to
  [`plot_residuals()`](https://strengejacke.github.io/sjPlot/reference/plot_residuals.md).

### Changes to functions

- Improved support for `brmsfit`-objects with categorical-family for
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `show.adj.icc`-argument, to also show the adjusted ICC for
  mixed models.
- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  gets a `col.order`-argument, reorder the table columns.
- Argument `hide.progress` in
  [`view_df()`](https://strengejacke.github.io/sjPlot/reference/view_df.md)
  is deprecated. Please use `verbose` now.
- The `statistics`-argument in
  [`sjt.xtab()`](https://strengejacke.github.io/sjPlot/reference/tab_xtab.md)
  gets a `"fisher"`-option, to force Fisher’s Exact Test to be used.

### Removed / Defunct

Following functions are now defunct:

- `sjp.lm()`, `sjp.glm()`, `sjp.lmer()`, `sjp.glmer()` and `sjp.int()`.
  Please use
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  instead.
- `sjt.frq()`. Please use `sjmisc::frq(out = "v")` instead.

### Bug fixes

- Due to changes in the *broom* and *lmerTest* packages, tidiers did no
  longer work for `lmerModLmerTest` objects.
- Fix issue with standardized coefficient (argument `show.std`) in
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).

## sjPlot 2.5.0

CRAN release: 2018-07-12

### New functions

- [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  as replacement for `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and
  `sjt.glmer()`. Furthermore,
  [`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
  is designed to work with the same model-objects as
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md).
- New colour scales for ggplot-objects:
  [`scale_fill_sjplot()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)
  and
  [`scale_color_sjplot()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md).
  These provide predifined colour palettes from this package.
- [`show_sjplot_pals()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)
  to show all predefined colour palettes provided by this package.
- [`sjplot_pal()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)
  to return colour values of a specific palette.

### Deprecated

Following functions are now deprecated:

- `sjp.lm()`, `sjp.glm()`, `sjp.lmer()`, `sjp.glmer()` and `sjp.int()`.
  Please use
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  instead.
- `sjt.frq()`. Please use `sjmisc::frq(out = "v")` instead.

### Removed / Defunct

Following functions are now defunct:

- `sjt.grpmean()`, `sjt.mwu()` and `sjt.df()`. The replacements are
  `sjstats::grpmean()`, `sjstats::mwu()` and
  [`tab_df()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md)
  resp.
  [`tab_dfs()`](https://strengejacke.github.io/sjPlot/reference/tab_df.md).

### Changes to functions

- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  get a `prefix.labels`-argument, to prefix automatically retrieved term
  labels with either the related variable name or label.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  gets a `show.zeroinf`-argument to show or hide the zero-inflation-part
  of models in the plot.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  gets a `jitter`-argument to add some random variation to data points
  for those plot types that accept `show.data = TRUE`.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  gets a `legend.title`-argument to define the legend title for plots
  that display a legend.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now passes more arguments in `...` down to
  [`ggeffects::plot()`](https://strengejacke.github.io/ggeffects/reference/plot.html)
  for marginal effects plots.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now plots the zero-inflated part of the model for `brmsfit`-objects.
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now plots multivariate response models, i.e. models with multiple
  outcomes.
- Diagnostic plots in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  (`type = "diag"`) can now also be used with `brmsfit`-objects.
- Axis limits of diagnostic plots in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  (`type = "diag"`) for Stan-models (`brmsfit` or `stanreg` resp.
  `stanfit`) can now be set with the `axis.lim`-argument.
- The `grid.breaks`-argument for
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  now also takes a vector of values to directly define the grid breaks
  for the plot.
- Better default calculation for grid breaks in
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  and
  [`plot_models()`](https://strengejacke.github.io/sjPlot/reference/plot_models.md)
  when the `grid.breaks`-argument is of length one.
- The `terms`-argument for
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now also allows the specification of a range of numeric values in
  square brackets for marginal effects plots,
  e.g. `terms = "age [30:50]"` or `terms = "age [pretty]"`.
- For coefficient-plots, the `terms`- and `rm.terms`-arguments for
  [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now also allows specification of factor levels for categorical terms.
  Coefficients for the indicted factor levels are kept resp. removed
  (see
  [`?plot_model`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  for details).
- [`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md)
  now supports `clmm`-objects (package *ordinal*).
- `plot_model(type = "diag")` now also shows random-effects QQ-plots for
  `glmmTMB`-models, and also plots random-effects QQ-plots for all
  random effects (if model has more than one random effect term).

### Bug fixes

- `plot_model(type = "re")` now supports standard errors and confidence
  intervals for `glmmTMB`-objects.
- Fixed typo for `glmmTMB`-tidier, which may have returned wrong data
  for zero-inflation part of model.
- Multiple random intercepts for multilevel models fitted with `brms`
  area now shown in each own facet per intercept.
- Remove unnecessary warning in `sjp.likert()` for uneven category count
  when neutral category is specified.
- `plot_model(type = "int")` could not automatically select
  `mdrt.values` properly for non-integer variables.
- `sjp.grpfrq()` now correctly uses the complete space in facets when
  `facet.grid = TRUE`.
- `sjp.grpfrq(type = "boxplot")` did not correctly label the x-axis when
  one category had no elements in a vector.
- Problems with German umlauts when printing HTML tables were fixed.
