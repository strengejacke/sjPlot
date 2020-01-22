# sjPlot 2.8.2

## Function renaming

* `sjt.itemanalysis()` is now named `tab_itemscale()`.
* `sjt.xtab()` is now named `tab_xtab()`.

## Changes to functions

* Improved handling for `tab_model()` of robust estimation in general and Kenward-Roger or Satterthwaite approximations in particular for linear mixed models.
* Revised code to cope with forthcoming *tidyselect*-update.

## Bug fixes

* Improved `tab_df()` now uses value labels for factors instead of numeric values.
* Fixed some issues related to the lates *brms*-update.

# sjPlot 2.8.1

## Changes to functions

* `tab_model()` gets arguments `bootstrap`, `iterations` and `seed` to return bootstrapped estimates.

## Bug fixes

* Fixed issue in `tab_model()` with detecting labels when `auto.label = TRUE`.
* Fixed issue in `tab_model()` for negative binomial hurdle mixed models (i.e. *glmmTMB* models with truncated negative-binomial family).
* Fixed bug in `tab_model()` with `show.reflvl = TRUE`.
* Fixed bug in `tab_model()` where labels for coefficients where not matching the correct coefficients.

# sjPlot 2.8.0

## Breaking changes

* Cluster functions have been removed, as these are now re-implemented in the **parameters** package.

## General

* Standardization of model parameters (in `plot_model()` or `tab_model()`) now uses standardization based on refitting the model (see [vignette](https://easystats.github.io/effectsize/articles/standardize_parameters.html) for details).

## Changes to functions

* `plot_model()` gets `type = "emm"` as marginal effects plot type, which is similar to `type = "eff"`. See [Plotting Marginal Effects of Regression Models](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html) for details.
* The `verbose`-argument in `view_df()` now defaults to `FALSE`.
* Updated and re-arranged internal color palette, especially to have a better behaviour when selecting colors from continuous palettes (see `show_pals()`).

## Bug fixes

* `sort.est = NULL` in `plot_model()` now preserves original order of coefficients.
* Fixed bug in automatic axis labelling for `plot_frq()` for non-labelled, numeric values.
* Fixed bug in `plot_frq()` when plotting factors.
* Arguments `string.std_ci` and `string.std_se` are no longer ignored in `tab_model()`.

# sjPlot 2.7.2

## General

* Replaced `performance::principal_component()` by `parameters::principal_component()`.
* Fixed CRAN check issues, due to the latest *bayestestR* update.

## Function renaming

* `sjp.grpfrq()` is now names `plot_grpfrq()`.
* `sjp.xtab()` is now names `plot_xtab()`.

## Changes to functions

* `plot_grid()` gets a `tags`-argument to add tags to plot-panels.

## Bug fixes

* Fixed bug in `plot_stackfrq()` for data frames with many missing values.
* Fixed bug with sorting frequencies in `plot_frq()` when vector had more labels than values. 
* Fixed bug in `tab_model()` where `show.reflvl = TRUE` did not insert the reference category in first place, but in alphabetical order.

# sjPlot 2.7.1

## General

* Minor revisions to meet the changes in the forthcoming update from tidyr.
* new color palettes were added (see `show_sjplot_pals()`).

## Changes to functions

* `tab_model()` now supports *gamlss* models.
* `tab_df()` gets a `digits` argument, to round numeric values in output.

## Bug fixes

* Fixed bug in `tab_model()` with `show.df = TRUE` for *lmerModLmerTest*.
* Fixed bug in `tab_stackfrq()` when items had different amount of valid values.

# sjPlot 2.7.0

## Renamed functions

* `sjp.stackfrq()` was renamed to `plot_stackfrq()`.
* `sjt.stackfrq()` was renamed to `tab_stackfrq()`.

## Changes to functions

### `plot_likert()`

* showed category labels in the top and bottom legends in two rows if there are more than six categories. Also, the categories are ordered column wise instead of row wise. This behaviour can now be controlled for grouped likert plots, using `group.legend.options`. The ordering now defaults to row wise and the user can force all categories onto a single row.
* now automatically adjusts labels to avoid overlapping.

### `tab_model()`

* now supports `wbm()`-models from the *panelr*-package.
* gets a `show.aicc`-argument to show the second order AIC.
* gets a `show.reflvl`-argument to show the reference level of factors.
* gets a `string.std_se` and `string.std_ci`-argument to change the column header for standard errors and confidence intervals of standardized coefficients.
* no longer prints a message that default p-values for mixed models are based on Wald approximation.
* `show.ci50` defaults to `FALSE` now.

### `sjt.itemanalysis()`

* `sjt.itemanalysis()` now works on ordered factors. A clearer error message was added when unordered factors are used. The old error message was not helpful.
* The `factor.groups` argument can now be `"auto"` to detect factor groups based on a pca with Varimax rotation.

### `sjp.stackrq()`

* `sjp.stackfrq()` was renamed to `plot_stackfrq()`.
* `sjp.stackfrq()` (now named: `plot_stackfrq()`) gets a `show.n`-argument to also show count values. This option can be combined with `show.prc`.
* `sjp.stackfrq()` (now named: `plot_stackfrq()`) now also works on grouped data frames.

### changes to other functions

* `plot_model()` now supports `wbm()`-models from the *panelr*-package.

## Bug fixes

* `plot_model(type = "int")` now also recognized interaction terms with `:` in formula.
* Argument `string.est` in `tab_model()` did not overwrite the default label for the estimate-column-header.
* Minor fix in `tab_model()` for mixed models that can't compute R2.
* Fix issue in `tab_model()` when printing robust standard errors and CI (i.e. when using arguments `vcov*`).
* The `plot_likert()` option `reverse.scale = TRUE` resulted in `values = "sum.inside"` being outside and the other way around. This is fixed now. 
* `view_df()` mixed up labels and frequency values when value labels were present, but no such values were in the data.
* Argument `wrap.labels` in `plot_frq()` did not properly work for factor levels.
* Fix issue in `plot_models()` that stopped for some models.
* Fix issue in `sjt.stackfrq()`, when `show.na = TRUE` and some items had zero-values.

# sjPlot 2.6.3

## General

* Export `dplyr::n()`, to meet changes in dplyr 0.8.0.
* `plot_model()` and `tab_model()` now support `MixMod`-objects from package **GLMMadpative**, `mlogit`- and `gmnl`-models.

## Renamed functions

* `sjp.kfold_cv()` was renamed to `plot_kfold_cv()`.
* `sjp.frq()` was renamed to `plot_frq()`.

## Changes to functions

### tab_model()

* `tab_model()` gets a `show.ngrps`-argument, which adds back the functionality to print the number of random effects groups for mixed models.
* `tab_model()` gets a `show.loglik`-argument, which adds back the functionality to print the model's log-Likelihood.
* `tab_model()` gets a `strings`-argument, as convenient shortcut for setting column-header strings.
* `tab_model()` gets additional arguments `vcov.fun`, `vcov.type` and `vcov.args` that are passed down to `sjstats::robust()`, to calculate different types of (clustered) robust standard errors.
* The `p.style`-argument now also allows printing both numeric p-values and asterisks, by using `p.style = "both"`.

### plot_likert()

* `plot_likert()` gets a `reverse.scale` argument to reverse the order of categories, so positive and negative values switch position.
* `plot_likert()` gets a `groups` argument, to group items in the plot (thanks to @ndevln).
* Argument `grid.range` in `plot_likert()` now may also be a vector of length 2, to define diffent length for the left and right x-axis scales.

### Other

* `plot_frq()` (former `sjp.frq()`) now has pipe-consistent syntax, enables plotting multiple variables in one function call and supports grouped data frames.
* `plot_model()` gets additional arguments `vcov.fun`, `vcov.type` and `vcov.args` that are passed down to `sjstats::robust()`, to calculate different types of (clustered) robust standard errors.
* `sjt.xtab()`, `sjp.xtab()`, `plot_frq()` and `sjp.grpfrq()` get a `drop.empty()`-argument, to drop values / factor levels with no observations from output.

## Bug fixes

* Legend labels were inverted for **brms**-models in `plot_model(..., type = "diag")`.
* Legend labels were duplicated for marginal effects plots when `color ="bw"` and `legend.title` was specified.
* Fixed encoding issues with help-files.
* `view_df()` did not truncate frequency- and percentage-values for variables where value labels were truncated to a certain maximum number.
* `tab_model()` did not print number of observations for `coxph`-models.

# sjPlot 2.6.2

## General

* Revised some help-files and vignettes.

## Removed / Defunct

Following functions are now defunct:

* `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and `sjt.glmer()`. Please use `tab_model()` instead.

## Changes to functions

* `tab_model()` supports printing simplex parameters of monotonic effects of **brms** models.
* `tab_model()` gets a `prefix.labels`-argument to add a prefix to the labels of categorical terms.
* The `rotation`-argument in `sjt.pca()` and `sjp.pca()` now supports all rotations from `psych::principal()`.

## Bug fixes

* `plot_model()` no longer automatically changes the plot-type to `"slope"` for models with only one predictor that is categorical and has more than two levels.
* `type = "eff"` and `type = "pred"` in `plot_model()` did not work when `terms` was not specified. 
* If robust standard errors are requested in `tab_model()`, the confidence intervals and p-values are now re-calculated and adjusted based on the robust standard errors.
* `colors = "bw"` was not recognized correctly for `plot_model(..., type = "int")`.
* Fix issue in `sjp.frq()` with correct axis labels for non-labelled character vectors.

# sjPlot 2.6.1

## General

* Removed defunct functions.

## Deprecated

* `sjt.lm()`, `sjt.glm()`, `sjt.lmer()` and `sjt.glmer()` are now deprecated. Please use `tab_model()` instead.

## Changes to functions

* Arguments `dot.size` and `line.size` in `plot_model()` now also apply to marginal effects and diagnostic plots.
* `plot_model()` now uses a free x-axis scale in facets for models with zero-inflated part.
* `plot_model()` now shows multiple plots for models with zero-inflated parts when `grids = FALSE`.
* `tab_model()` gets a `p.style` and `p.threshold` argument to indicate significance levels as asteriks, and to determine the threshold for which an estimate is considered as significant.
* `plot_model()` and `plot_models()` get a `p.threshold` argument to determine the threshold for which an estimate is considered as significant.

## Bug fixes

* Fixed bug from the last update that made value labels disappear for `plot_likert()`.
* `tab_model()` now also accepts multiple model-objects stored in a `list` as argument, as stated in the help-file.
* The `file`-argument now works again in `sjt.itemanalysis()`.
* Argument `show.ci` in `tab_model()` did not compute confidence intervals for different levels.

# sjPlot 2.6.0

## General

* `sjp.scatter()` was revised and renamed to `plot_scatter()`. `plot_scatter()` is pipe-friendly, and also works on grouped data frames.
* `sjp.gpt()` was revised and renamed to `plot_gpt()`. `plot_gpt()` is pipe-friendly, and also works on grouped data frames.
* Reduce package dependencies.

## Renamed functions

* `sjp.scatter()` was renamed to `plot_scatter()`.
* `sjp.likert()` was renamed to `plot_likert()`.
* `sjp.gpt()` was renamed to `plot_gpt()`.
* `sjp.resid()` was renamed to `plot_residuals()`.

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
