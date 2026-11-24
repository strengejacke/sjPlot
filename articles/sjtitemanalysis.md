# Item Analysis of a Scale or an Index

This document shows examples for using the
[`tab_itemscale()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md)
function of the sjPlot package.

## Performing an item analysis of a scale or index

This function performs an item analysis with certain statistics that are
useful for scale or index development. Following statistics are computed
for each variable (column) of a data frame:

- percentage of missing values
- mean value
- standard deviation
- skew
- item difficulty
- item discrimination
- Cronbach’s Alpha if item was removed from scale
- mean (or average) inter-item-correlation

Optional, following statistics can be computed as well:

- kurstosis
- Shapiro-Wilk Normality Test

If the argument `factor.groups` is *not* `NULL`, the data frame df will
be splitted into groups, assuming that `factor.groups` indicate those
columns (variables) of the data frame that belong to a certain factor
(see, for instance, return value of function
[`tab_pca()`](https://strengejacke.github.io/sjPlot/reference/tab_pca.md)
or
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
as example for retrieving factor groups for a scale). This is useful
when you have perfomed a principal component analysis or factor analysis
as first step, and now want to see whether the found factors /
components represent a scale or index score.

To demonstrate this function, we first need some data:

## Index score with one component

The simplest function call is just passing the data frame as argument.
In this case, the function assumes that all variables of the data frame
belong to one factor only.

``` r

tab_itemscale(mydf)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
```

| Row | Missings | Mean | SD | Skew | Item Difficulty | Item Discrimination | α if deleted |  |
|:---|:---|:---|:---|:---|:---|----|----|----|
| do you feel you cope well as caregiver? | 0.77 % | 3.12 | 0.58 | -0.12 | 0.78 | -0.24 | 0.54 |  |
| do you find caregiving too demanding? | 0.66 % | 2.02 | 0.72 | 0.65 | 0.51 | 0.33 | 0.38 |  |
| does caregiving cause difficulties in your relationship with your friends? | 0.66 % | 1.63 | 0.87 | 1.31 | 0.41 | 0.41 | 0.34 |  |
| does caregiving have negative effect on your physical health? | 1.10 % | 1.77 | 0.87 | 1.06 | 0.44 | 0.44 | 0.32 |  |
| does caregiving cause difficulties in your relationship with your family? | 0.66 % | 1.39 | 0.67 | 1.77 | 0.35 | 0.36 | 0.38 |  |
| does caregiving cause financial difficulties? | 0.88 % | 1.29 | 0.64 | 2.43 | 0.32 | 0.42 | 0.37 |  |
| do you feel trapped in your role as caregiver? | 0.88 % | 1.92 | 0.91 | 0.83 | 0.48 | 0.37 | 0.35 |  |
| do you feel supported by friends/neighbours? | 0.77 % | 2.16 | 1.04 | 0.32 | 0.54 | -0.03 | 0.53 |  |
| do you feel caregiving worthwhile? | 2.20 % | 2.93 | 0.96 | -0.45 | 0.73 | -0.11 | 0.56 |  |
| Mean inter-item-correlation=0.092 · Cronbach’s α=0.459 |  |  |  |  |  |  |  |  |

Component 1 {.table style="border-collapse:collapse; border:none;"}

To interprete the output, we may consider following values as
rule-of-thumbs for indicating a reliable scale:

- item difficulty should range between 0.2 and 0.8. Ideal value is
  p+(1-p)/2 (which mostly is between 0.5 and 0.8)
- for item discrimination, acceptable values are 0.2 or higher; the
  closer to 1 the better
- in case the total Cronbach’s Alpha value is below the acceptable
  cut-off of 0.7 (mostly if an index has few items), the mean
  inter-item-correlation is an alternative measure to indicate
  acceptability; satisfactory range lies between 0.2 and 0.4

## Index score with more than one component

The items of the COPE index used for our example do not represent a
single factor. We can check this, for instance, with a principle
component analysis. If you know, which variable belongs to which factor
(i.e. which variable is part of which component), you can pass a numeric
vector with these group indices to the argument `factor.groups`. In this
case, the data frame is divided into the components specified by
`factor.groups`, and each component (or factor) is analysed.

``` r

library(parameters)
# Compute PCA on Cope-Index, and retrieve
# factor indices for each COPE index variable
pca <- parameters::principal_components(mydf)
factor.groups <- parameters::closest_component(pca)
```

The PCA extracted two components. Now
[`tab_itemscale()`](https://strengejacke.github.io/sjPlot/reference/tab_itemscale.md)
…

1.  performs an item analysis on both components, showing whether each
    of them is a reliable and useful scale or index score
2.  builds an index of each component, by standardizing each scale
3.  and adds a component-correlation-matrix, to see whether the index
    scores (which are based on the components) are highly correlated or
    not.

``` r

tab_itemscale(mydf, factor.groups)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
#> Warning: Data frame needs at least three columns for reliability-test.
```

| Row | Missings | Mean | SD | Skew | Item Difficulty | Item Discrimination | α if deleted |  |
|:---|:---|:---|:---|:---|:---|----|----|----|
| do you feel you cope well as caregiver? | 0.77 % | 3.12 | 0.58 | -0.12 | 0.78 | -0.37 | 0.78 |  |
| do you find caregiving too demanding? | 0.66 % | 2.02 | 0.72 | 0.65 | 0.51 | 0.49 | 0.61 |  |
| does caregiving cause difficulties in your relationship with your friends? | 0.66 % | 1.63 | 0.87 | 1.31 | 0.41 | 0.55 | 0.59 |  |
| does caregiving have negative effect on your physical health? | 1.10 % | 1.77 | 0.87 | 1.06 | 0.44 | 0.54 | 0.59 |  |
| does caregiving cause difficulties in your relationship with your family? | 0.66 % | 1.39 | 0.67 | 1.77 | 0.35 | 0.44 | 0.63 |  |
| does caregiving cause financial difficulties? | 0.88 % | 1.29 | 0.64 | 2.43 | 0.32 | 0.47 | 0.62 |  |
| do you feel trapped in your role as caregiver? | 0.88 % | 1.92 | 0.91 | 0.83 | 0.48 | 0.57 | 0.58 |  |
| Mean inter-item-correlation=0.196 · Cronbach’s α=0.676 |  |  |  |  |  |  |  |  |

Component 1 {.table style="border-collapse:collapse; border:none;"}

 

| Row | Missings | Mean | SD | Skew | Item Difficulty | Item Discrimination | α if deleted |  |
|:---|:---|:---|:---|:---|:---|----|----|----|
| do you feel supported by friends/neighbours? | 0.77 % | 2.16 | 1.04 | 0.32 | 0.54 | NA | NA |  |
| do you feel caregiving worthwhile? | 2.20 % | 2.93 | 0.96 | -0.45 | 0.73 | NA | NA |  |
| Mean inter-item-correlation=0.260 · Cronbach’s α=0.412 |  |  |  |  |  |  |  |  |

Component 2 {.table style="border-collapse:collapse; border:none;"}

 

[TABLE]

## Adding further statistics

``` r

tab_itemscale(mydf, factor.groups, show.shapiro = TRUE, show.kurtosis = TRUE)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
#> Warning: Data frame needs at least three columns for reliability-test.
```

| Row | Missings | Mean | SD | Skew | Kurtosis | W(p) | Item Difficulty | Item Discrimination | α if deleted |  |
|:---|:---|:---|:---|:---|:---|----|----|----|----|----|
| do you feel you cope well as caregiver? | 0.77 % | 3.12 | 0.58 | -0.12 | 0.27 | 0.75 (0.000) | 0.78 | -0.37 | 0.78 |  |
| do you find caregiving too demanding? | 0.66 % | 2.02 | 0.72 | 0.65 | 0.73 | 0.80 (0.000) | 0.51 | 0.49 | 0.61 |  |
| does caregiving cause difficulties in your relationship with your friends? | 0.66 % | 1.63 | 0.87 | 1.31 | 0.86 | 0.72 (0.000) | 0.41 | 0.55 | 0.59 |  |
| does caregiving have negative effect on your physical health? | 1.10 % | 1.77 | 0.87 | 1.06 | 0.48 | 0.78 (0.000) | 0.44 | 0.54 | 0.59 |  |
| does caregiving cause difficulties in your relationship with your family? | 0.66 % | 1.39 | 0.67 | 1.77 | 2.87 | 0.62 (0.000) | 0.35 | 0.44 | 0.63 |  |
| does caregiving cause financial difficulties? | 0.88 % | 1.29 | 0.64 | 2.43 | 5.77 | 0.51 (0.000) | 0.32 | 0.47 | 0.62 |  |
| do you feel trapped in your role as caregiver? | 0.88 % | 1.92 | 0.91 | 0.83 | -0.08 | 0.81 (0.000) | 0.48 | 0.57 | 0.58 |  |
| Mean inter-item-correlation=0.196 · Cronbach’s α=0.676 |  |  |  |  |  |  |  |  |  |  |

Component 1 {.table style="border-collapse:collapse; border:none;"}

 

| Row | Missings | Mean | SD | Skew | Kurtosis | W(p) | Item Difficulty | Item Discrimination | α if deleted |  |
|:---|:---|:---|:---|:---|:---|----|----|----|----|----|
| do you feel supported by friends/neighbours? | 0.77 % | 2.16 | 1.04 | 0.32 | -1.14 | 0.85 (0.000) | 0.54 | NA | NA |  |
| do you feel caregiving worthwhile? | 2.20 % | 2.93 | 0.96 | -0.45 | -0.83 | 0.85 (0.000) | 0.73 | NA | NA |  |
| Mean inter-item-correlation=0.260 · Cronbach’s α=0.412 |  |  |  |  |  |  |  |  |  |  |

Component 2 {.table style="border-collapse:collapse; border:none;"}

 

[TABLE]
