# Summary of Bayesian Models as HTML Table

    ## Loading required package: Rcpp

    ## Loading 'brms' package (version 2.23.0). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

    ## 
    ## Attaching package: 'brms'

    ## The following object is masked from 'package:stats':
    ## 
    ##     ar

This vignette shows examples for using
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
to create HTML tables for mixed models. Basically,
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
behaves in a very similar way for mixed models as for other, simple
regression models, as shown [in this
vignette](https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.md).

``` r

# load required packages
library(sjPlot)
library(brms)

# sample models
zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
set.seed(123)
m1 <- brm(bf(
    count ~ persons + child + camper + (1 | persons),
    zi ~ child + camper + (1 | persons)
  ),
  data = zinb,
  family = zero_inflated_poisson()
)

data(epilepsy)
set.seed(123)
epilepsy$visit <- as.numeric(epilepsy$visit)
epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
f1 <- bf(Base ~ zAge + count + (1 |ID| patient))
f2 <- bf(Base2 ~ zAge + Trt + (1 |ID| patient))
m2 <- brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
```

## Bayesian models summaries as HTML table

For Bayesian regression models, some of the differences to the table
output from [simple
models](https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.md)
or [mixed
models](https://strengejacke.github.io/sjPlot/articles/tab_mixed.md) of
`tab_models()` are the use of *Highest Density Intervals* instead of
confidence intervals, the Bayes-R-squared values, and a different “point
estimate” (which is, by default, the median from the posterior draws).

``` r

tab_model(m1)
```

|                              |         count         |              |     |
|:----------------------------:|:---------------------:|:------------:|-----|
|          Predictors          | Incidence Rate Ratios |   CI (95%)   |     |
|         Count Model          |                       |              |     |
|          Intercept           |         0.42          | 0.22 – 0.88  |     |
|           persons            |         2.32          | 1.86 – 2.93  |     |
|            child             |         0.32          | 0.26 – 0.38  |     |
|            camper            |         2.08          | 1.73 – 2.53  |     |
|     Zero-Inflated Model      |                       |              |     |
|          Intercept           |         0.52          | 0.11 – 2.21  |     |
|            child             |         6.44          | 3.46 – 12.95 |     |
|            camper            |         0.43          | 0.21 – 0.87  |     |
|        Random Effects        |                       |              |     |
|              σ²              |         5.22          |              |     |
|             τ₀₀              |         33.72         |              |     |
|             ICC              |         0.13          |              |     |
|         N _(persons)         |           4           |              |     |
|         Observations         |          250          |              |     |
| Marginal R² / Conditional R² |     0.186 / 0.248     |              |     |

## Multivariate response models

For multivariate response models, like mediator-analysis-models, it is
recommended to print just one model in the table, as each regression is
displayed as own “model” in the output.

``` r

tab_model(m2)
```

|                |   Base    |               |   Base2   |               |
|:--------------:|:---------:|:-------------:|:---------:|:-------------:|
|   Predictors   | Estimates |   CI (95%)    | Estimates |   CI (95%)    |
|   Intercept    |   28.61   | 11.35 – 34.20 |   26.61   | 11.24 – 29.03 |
|      zAge      |   -4.85   | -5.42 – -1.76 |   1.21    | -0.31 – 2.15  |
|     count      |   0.00    | -0.00 – 0.00  |           |               |
|   Trt: Trt1    |           |               |   -0.32   | -4.36 – 1.43  |
| Random Effects |           |               |           |               |
|       σ²       |   54.29   |               |           |               |
|      τ₀₀       |   4.54    |               |           |               |
|      ICC       |   0.96    |               |           |               |
|  N _(patient)  |    59     |               |           |               |
|  Observations  |    236    |               |           |               |

## Show two Credible Interval-column

To show a second CI-column, use `show.ci50 = TRUE`.

``` r

tab_model(m2, show.ci50 = TRUE)
```

|   | Base |  |  | Base2 |  |  |
|:--:|:--:|:--:|:--:|:--:|:--:|----|
| Predictors | Estimates | CI (50%) | CI (95%) | Estimates | CI (50%) | CI (95%) |
| Intercept | 28.61 | 24.07 – 30.23 | 11.35 – 34.20 | 26.61 | 21.53 – 28.45 | 11.24 – 29.03 |
| zAge | -4.85 | -5.17 – -3.89 | -5.42 – -1.76 | 1.21 | 0.74 – 1.54 | -0.31 – 2.15 |
| count | 0.00 | -0.00 – 0.00 | -0.00 – 0.00 |  |  |  |
| Trt: Trt1 |  |  |  | -0.32 | -1.91 – 0.69 | -4.36 – 1.43 |
| Random Effects |  |  |  |  |  |  |
| σ² | 54.49 |  |  |  |  |  |
| τ₀₀ | 3.96 |  |  |  |  |  |
| ICC | 0.96 |  |  |  |  |  |
| N _(patient) | 59 |  |  |  |  |  |
| Observations | 236 |  |  |  |  |  |

## Mixing multivariate and univariate response models

When both multivariate and univariate response models are displayed in
one table, a column *Response* is added for the multivariate response
model, to indicate the different outcomes.

``` r

tab_model(m1, m2)
```

|   | count |  | Base, Base2 |  |  |
|:--:|:--:|:--:|:--:|:--:|:--:|
| Predictors | Incidence Rate Ratios | CI (95%) | Estimates | CI (95%) | Response |
| Intercept | 0.42 | 0.22 – 0.88 | 28.61 | 11.35 – 34.20 | Base |
| Intercept | 0.42 | 0.22 – 0.88 | 26.61 | 11.24 – 29.03 | Base2 |
| persons | 2.32 | 1.86 – 2.93 |  |  |  |
| child | 0.32 | 0.26 – 0.38 |  |  |  |
| camper | 2.08 | 1.73 – 2.53 |  |  |  |
| zAge |  |  | -4.85 | -5.42 – -1.76 | Base |
| count |  |  | 0.00 | -0.00 – 0.00 | Base |
| zAge |  |  | 1.21 | -0.31 – 2.15 | Base2 |
| Trt: Trt1 |  |  | -0.32 | -4.36 – 1.43 | Base2 |
| Zero-Inflated Model |  |  |  |  |  |
| Intercept | 0.52 | 0.11 – 2.21 |  |  |  |
| child | 6.44 | 3.46 – 12.95 |  |  |  |
| camper | 0.43 | 0.21 – 0.87 |  |  |  |
| Random Effects |  |  |  |  |  |
| σ² | 5.49 |  | 55.22 |  |  |
| τ₀₀ | 33.50 |  | 4.43 |  |  |
| ICC | 0.14 |  | 0.96 |  |  |
| N | 4 _(persons) |  | 59 _(patient) |  |  |
| Observations | 250 |  | 236 |  |  |
| Marginal R² / Conditional R² | 0.186 / 0.248 |  | NA |  |  |
