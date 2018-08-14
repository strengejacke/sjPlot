# sjPlot 2.5.1

## General

* `sjp.scatter()` was revised and renamed to `scatterplot()`. `scatterplot()` is pipe-friendly, and also works on grouped data frames.
* Reduce package dependencies.

## Changes to functions

* Improved support for `brmsfit`-objects with categorical-family for `plot_model()` and `tab_model()`.
* `tab_model()` gets a `show.adj.icc`-argument, to also show the adjusted ICC for mixed models.
* `tab_model()` gets a `col.order`-argument, reorder the table columns.
* Argument `hide.progress` in `view_df()` is deprecated. Please use `verbose` now.
* The `statistics`-argument in `sjt.xtab()` gets a `"fisher"`-option, to force Fisher's Exact Test to be used.

## Removed / Defunct

Following functions are now defunct:

* `sjp.lm()`, `sjp.glm()`, `sjp.lmer()`, `sjp.glmer()` and `sjp.int()`. Please use `plot_model()` instead.
* `sjt.frq()`. Please use `sjmisc::frq(out = "v")` instead.

## Bug fixes

* Due to changes in the _broom_ and _lmerTest_ packages, tidiers did no longer work for `lmerModLmerTest` objects.
* Fix issue with standardized coefficient (argument `show.std`) in `tab_model()`.

# sjPlot 2.5.0

## New functions

* `tab_model()` as replacement for `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and `sjt.glmer()`. Furthermore, `tab_model()` is designed to work with the same model-objects as `plot_model()`.
* New colour scales for ggplot-objects: `scale_fill_sjplot()` and `scale_color_sjplot()`. These provide predifined colour palettes from this package.
* `show_sjplot_pals()` to show all predefined colour palettes provided by this package.
* `sjplot_pal()` to return colour values of a specific palette.

## Deprecated

Following functions are now deprecated:

* `sjp.lm()`, `sjp.glm()`, `sjp.lmer()`, `sjp.glmer()` and `sjp.int()`. Please use `plot_model()` instead.
* `sjt.frq()`. Please use `sjmisc::frq(out = "v")` instead.

## Removed / Defunct

Following functions are now defunct:

* `sjt.grpmean()`, `sjt.mwu()` and `sjt.df()`. The replacements are `sjstats::grpmean()`, `sjstats::mwu()` and `tab_df()` resp. `tab_dfs()`.

## Changes to functions

* `plot_model()` and `plot_models()` get a `prefix.labels`-argument, to prefix automatically retrieved term labels with either the related variable name or label.
* `plot_model()` gets a `show.zeroinf`-argument to show or hide the zero-inflation-part of models in the plot.
* `plot_model()` gets a `jitter`-argument to add some random variation to data points for those plot types that accept `show.data = TRUE`.
* `plot_model()` gets a `legend.title`-argument to define the legend title for plots that display a legend.
* `plot_model()` now passes more arguments in `...` down to `ggeffects::plot()` for marginal effects plots.
* `plot_model()` now plots the zero-inflated part of the model for `brmsfit`-objects.
* `plot_model()` now plots multivariate response models, i.e. models with multiple outcomes.
* Diagnostic plots in `plot_model()` (`type = "diag"`) can now also be used with `brmsfit`-objects.
* Axis limits of diagnostic plots in `plot_model()` (`type = "diag"`) for Stan-models (`brmsfit` or `stanreg` resp. `stanfit`) can now be set with the `axis.lim`-argument.
* The `grid.breaks`-argument for `plot_model()` and `plot_models()` now also takes a vector of values to directly define the grid breaks for the plot.
* Better default calculation for grid breaks in `plot_model()` and `plot_models()` when the `grid.breaks`-argument is of length one.
* The `terms`-argument for `plot_model()` now also allows the specification of a range of numeric values in square brackets for marginal effects plots, e.g. `terms = "age [30:50]"` or `terms = "age [pretty]"`.
* For coefficient-plots, the `terms`- and `rm.terms`-arguments for `plot_model()` now also allows specification of factor levels for categorical terms. Coefficients for the indicted factor levels are kept resp. removed (see `?plot_model` for details).
* `plot_model()` now supports `clmm`-objects (package *ordinal*).
* `plot_model(type = "diag")` now also shows random-effects QQ-plots for `glmmTMB`-models, and also plots random-effects QQ-plots for all random effects (if model has more than one random effect term).

## Bug fixes

* `plot_model(type = "re")` now supports standard errors and confidence intervals for `glmmTMB`-objects.
* Fixed typo for `glmmTMB`-tidier, which may have returned wrong data for zero-inflation part of model.
* Multiple random intercepts for multilevel models fitted with `brms` area now shown in each own facet per intercept.
* Remove unnecessary warning in `sjp.likert()` for uneven category count when neutral category is specified.
* `plot_model(type = "int")` could not automatically select `mdrt.values` properly for non-integer variables.
* `sjp.grpfrq()` now correctly uses the complete space in facets when `facet.grid = TRUE`.
* `sjp.grpfrq(type = "boxplot")` did not correctly label the x-axis when one category had no elements in a vector.
* Problems with German umlauts when printing HTML tables were fixed.

# sjPlot 2.4.1

## General

* Remove unnecessary imports.
* Add back `save_plot()` function.
* Revised and updated package vignettes.

## New functions

* `tab_df()` and `tab_dfs()` as (more generic) replacement for `sjt.df()`. `tab_df()` prints the content of a data frame as HTML table, while `tab_dfs()` prints a list of data frames into a HTML table. The HTML table is then displayed in the Viewer-pane of RStudio or in a web browser.

## Deprecated

* `sjt.grpmean()` is now deprecated. Please use `sjstats::grpmean()` with argument `out = "viewer"` instead.
* `sjt.mwu()` is now deprecated. Please use `sjstats::mwu()` with argument `out = "viewer"` instead.
* `sjt.df()` is now deprecated. Please use `sjmisc::descr()` with argument `out = "viewer"` or `tab_df()` instead.

## Changes to functions

* `plot_model()` now also supports `clm`-models from package *ordinal*, `polr`-models from package *MASS*, `multinom`-models from package *nnet* and `Zelig-relogit`-models from package *Zelig*.
* `plot_model()` gets a `show.legend`-argument to show or hide the legend for marginal effects plots.
* `plot_model()` gets a `se`-argument to plot (robust) standard errors instead of confidence intervals for coefficient-plots.
* Diagnostic plots in `plot_model()` (`type = "diag"`) now also plot diagnostics of random effects from (generalized) linear mixed models.
* The `...`-argument of `plot_model()` now also accepts the arguments `sep_in` and `sep_out`, which are passed down to `snakecase::to_any_case()` for case conversion of term labels (axis labels).
* The `title`-argument in `plot_model()` now also works for plotting random effects (`type = "re"`).
* `sjt.itemanalysis()` no longer returns a list of score items, but only a data frame of scores.
* `sjp.grpfrq()` gets a `show.ci`-argument to add notches to boxplots.

## Bug fixes

* `view_df()` did not work with double values (with decimal points) when `show.values = TRUE`.
* `view_df()` caused an error when a variable has completely missing values.
* `plot_models()` did not properly remove intercepts from output for survey models, when `show.intercept = FALSE`.
* `plot_models()` did not automatically transform axis for all applicable model types.
* `get_model_data()` did not work for marginal effects plots.
* Fixed a bug that was introduced during the past update in `sjt.grpmean()`, resulting in multiple table outputs and a wrong overall p-value in the summary line.
* Model weights are now correctly taken into account for marginal effect plots in `plot_model()`.
* `sjp.likert()` did not show correct order for factors with character levels, when a neutral category was specified and was not the last factor level.
* Fixed issue when plotting random effects (`type = "re"`) for specific `brms`-models.

# sjPlot 2.4.0

## General

* The old `set_theme()` was removed. Instead, there are some new predifined themes available (see `?"sjPlot-themes"`). The former `sjp.setThemes()` was renamed to `set_theme()` instead.

## New functions

* `plot_model()` as replacement for `sjp.lm()`, `sjp.glm()`, `sjp.lmer()`, `sjp.glmer()` and `sjp.int()` (which will become deprecated in the future, and will later be removed).
* `get_model_data()` to get the data from plot-objects created with `plot_model()`.
* `font_size()`, `label_angle()` and `legend_style()` as convenient ways to tweak common ggplot-theme-elements.

## Changes to functions

* `view_df()` now better handles string variables and gets a `show.string.values`-argument to omit the output of values from string variables.
* `view_df()` gets a `max.len`-argument to truncate output for variables with many values.
* `view_df()` displays more information on non-labelled, numeric variables.
* `sjp.pca()` and `sjt.pca()` now give more informative error messages when just one component is extracted.

## Bug fixes

* Fixed bug with `rm.terms`-argument in `plot_models()`.
* In rare situations, `view_df()` did not work for string variables with missing values.
* Fixed issue with wrong footnote text when saving the output of `sjt.pca()` as file.
* `sjp.xtab()` did not work when `show.n` and `show.prc` were set to `FALSE`, but `show.values` was `TRUE`.

# sjPlot 2.3.3

## General

* Fixed issue with latest tidyr-update on CRAN.
* HTML-tables (`sjt.*`-functions) displayed in the viewer pane now automatically add a CSS-style for white page background. This fixes an RStudio issue on OS X, where the new look'n'feel used dark backgrounds in the viewer pane, making output hardly readable.

# sjPlot 2.3.2

## General

* Revising package code from scratch. Part of the old functions will be replaced by new ones, making the code base easier to maintain and reduce redundant functions by merging them together. In the course of the next updates, functions will first become deprecated and later defunct.
* Changed imports to avoid using deprecated functions.
* Use more informative warning- and error-messages for certain functions.

## New functions

* `plot_models()` as replacement for `sjp.lmm()` and `sjp.glmm()` (which are now deprecated).
* `sjp.fa()` and `sjt.fa()` to plot or print as table the results of factor analyses.

## Bug fixes

* The mean and standard deviation in the summary of `sjt.frq()` were not correctly computed, when `auto.group`-argument was specified. This bug was introduced in the last update and is not fixed again.
* Values of character vectors were not always correctly sorted in `view_df()` (if `show.frq = TRUE`).

# sjPlot 2.3.1

## General

* All `sjt`-functions can now be directly integrated into knitr-code-chunks, because sjPlot exports a knitr-print-method (see `vignette("sjtbasic", "sjPlot")`).
* `sjtab()` now also works within knitr-documents (see `vignette("sjtbasic", "sjPlot")`).
* Updated Namespace for functions that moved from package **sjstats** to **sjmisc**.

## Changes to functions

* Changed defaults for `save_plot()`.
* `save_plot()` now also supports _svg_-format.
* For effect-plots (`type = "eff"`), the `axis.title`-argument can now be used to change the title of y-axes.
* For `sjp.lm()`, `sjp.glm()`, `sjp.lmer()` and `sjp.glmer()`, if color palette has more values than needed, it is silently shortend to the required length.
* When plotting mixed models, argument `geom.colors` now also applies to plot-type `type = "ri.slope"`.
* Default correlation-method for `sjt.corr()` and `sjp.corr()` is now `pearson`.
* Argument `emph.p` for printing tables of regression models now defaults to `FALSE`.

## Bug fixes

* Fixed bug in `sjt.frq()` for variables with many missing values and labelled values that did not occur on that variable.
* Argument `value.labels` had no effect for `sjt.frq()`.
* Automatic label detection in `sjt.grpmean()` sometimes not worked for factors without variable labels.
* `sjp.glm()` used _Odds Ratios_ as default title for y-axis when plotting marginal effects. Fixed, now y-axis is correctly labelled.
* `sjt.glm()` used "Odds Ratios" as default column heading for the estimates, even for poisson or other models. Now the string for column headers is selected based on the first model input of the function.
* Solved issue with warning in prediction-plots (`type = "pred"`) for categorical variables on the x-axis.

# sjPlot 2.3.0

## General

* Vignettes were added to this package.

## Changes to functions

* You can use `geom.colors = "bw"` for linetype-plots, to create black & white figures that use different linetypes instead of different colors.
* `sjp.kfold_cv()` now also supports poisson and negative binomial regression models.
* `sjp.pca()` and `sjt.pca()` get a `rotation`-argument, to use either varimax- or oblimin-transformation of factor loadings.
* Argument `show.value` now also applies to bar plots in `sjp.pca()`. 
* `sjt.glm()`, for generalized linar (mixed) models, now shows adjusted standard errors, using the Taylor series-based delta method. 
* More precise rounding of percentage values in `sjt.xtab()`, `sjp.xtab()` and `sjp.grpfrq()`.
* Cramer's V in `sjt.xtab()` is now dentoted as _V_.
* `sjt.xtab()` gets a `...`-argument, to pass down further arguments to the test statistics functions `chisq.test()` and `fisher.test()`.
* `sjt.xtab()` gets a `statistics`-argument, to select one of different measures of associations for the table summary.

## Bug fixes

* Plotting or table output of regression models did not work with null-models (i.e. with intercept only).
