# Summary of Regression Models as HTML Table

[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
is the pendant to
[`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md),
however, instead of creating plots,
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
creates HTML-tables that will be displayed either in your IDE’s
viewer-pane, in a web browser or in a knitr-markdown-document (like this
vignette).

HTML is the only output-format, you can’t (directly) create a LaTex or
PDF output from
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
and related table-functions. However, it is possible to easily export
the tables into Microsoft Word or Libre Office Writer.

This vignette shows how to create table from regression models with
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).
There’s a dedicated vignette that demonstrate how to change the [table
layout and appearance with
CSS](https://strengejacke.github.io/sjPlot/articles/table_css.md).

**Note!** Due to the custom CSS, the layout of the table inside a
knitr-document differs from the output in the viewer-pane and web
browser!

``` r

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# sample data
data("efc")
efc <- as_factor(efc, c161sex, c172code)
```

## A simple HTML table from regression results

First, we fit two linear models to demonstrate the
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)-function.

``` r

m1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
m2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + e17age, data = efc)
```

The simplest way of producing the table output is by passing the fitted
model as parameter. By default, estimates, confidence intervals (*CI*)
and p-values (*p*) are reported. As summary, the numbers of observations
as well as the R-squared values are shown.

``` r

tab_model(m1)
```

[TABLE]

## Automatic labelling

As the **sjPlot**-packages features [labelled
data](https://strengejacke.github.io/sjlabelled/), the coefficients in
the table are already labelled in this example. The name of the
dependent variable(s) is used as main column header for each model. For
non-labelled data, the coefficient names are shown.

``` r

data(mtcars)
m.mtcars <- lm(mpg ~ cyl + hp + wt, data = mtcars)
tab_model(m.mtcars)
```

|                  |      mpg      |               |             |
|:----------------:|:-------------:|:-------------:|:-----------:|
|    Predictors    |   Estimates   |      CI       |      p      |
|   (Intercept)    |     38.75     | 35.09 – 42.41 | **\<0.001** |
|       cyl        |     -0.94     | -2.07 – 0.19  |    0.098    |
|        hp        |     -0.02     | -0.04 – 0.01  |    0.140    |
|        wt        |     -3.17     | -4.68 – -1.65 | **\<0.001** |
|   Observations   |      32       |               |             |
| R² / R² adjusted | 0.843 / 0.826 |               |             |

If factors are involved and `auto.label = TRUE`, “pretty” parameters
names are used (see
[`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.html).

``` r

set.seed(2)
dat <- data.frame(
  y = runif(100, 0, 100),
  drug = as.factor(sample(c("nonsense", "useful", "placebo"), 100, TRUE)),
  group = as.factor(sample(c("control", "treatment"), 100, TRUE))
)

pretty_names <- lm(y ~ drug * group, data = dat)
tab_model(pretty_names)
```

[TABLE]

### Turn off automatic labelling

To turn off automatic labelling, use `auto.label = FALSE`, or provide an
empty character vector for `pred.labels` and `dv.labels`.

``` r

tab_model(m1, auto.label = FALSE)
```

|                  |   barthtot    |               |             |
|:----------------:|:-------------:|:-------------:|:-----------:|
|    Predictors    |   Estimates   |      CI       |      p      |
|   (Intercept)    |     87.15     | 77.96 – 96.34 | **\<0.001** |
|     c160age      |     -0.21     | -0.35 – -0.07 |  **0.004**  |
|     c12hour      |     -0.28     | -0.32 – -0.24 | **\<0.001** |
|     c161sex2     |     -0.39     | -4.49 – 3.71  |    0.850    |
|    c172code2     |     1.37      | -3.12 – 5.85  |    0.550    |
|    c172code3     |     -1.64     | -7.22 – 3.93  |    0.564    |
|   Observations   |      821      |               |             |
| R² / R² adjusted | 0.271 / 0.266 |               |             |

Same for models with non-labelled data and factors.

``` r

tab_model(pretty_names, auto.label = FALSE)
```

|                            |       y       |                |             |
|:--------------------------:|:-------------:|:--------------:|:-----------:|
|         Predictors         |   Estimates   |       CI       |      p      |
|        (Intercept)         |     66.84     | 52.97 – 80.71  | **\<0.001** |
|        drugplacebo         |     -7.18     | -28.25 – 13.89 |    0.500    |
|         druguseful         |    -30.95     | -53.08 – -8.82 |  **0.007**  |
|       grouptreatment       |    -21.66     | -40.13 – -3.19 |  **0.022**  |
| drugplacebo:grouptreatment |     4.15      | -23.68 – 31.98 |    0.768    |
| druguseful:grouptreatment  |     30.85     |  2.38 – 59.33  |  **0.034**  |
|        Observations        |      100      |                |             |
|      R² / R² adjusted      | 0.116 / 0.069 |                |             |

## More than one model

[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
can print multiple models at once, which are then printed side-by-side.
Identical coefficients are matched in a row.

``` r

tab_model(m1, m2)
```

[TABLE]

## Generalized linear models

For generalized linear models, the ouput is slightly adapted. Instead of
*Estimates*, the column is named *Odds Ratios*, *Incidence Rate Ratios*
etc., depending on the model. The coefficients are in this case
automatically converted (exponentiated). Furthermore, pseudo R-squared
statistics are shown in the summary.

``` r

m3 <- glm(
  tot_sc_e ~ c160age + c12hour + c161sex + c172code, 
  data = efc,
  family = poisson(link = "log")
)

efc$neg_c_7d <- ifelse(efc$neg_c_7 < median(efc$neg_c_7, na.rm = TRUE), 0, 1)
m4 <- glm(
  neg_c_7d ~ c161sex + barthtot + c172code,
  data = efc,
  family = binomial(link = "logit")
)

tab_model(m3, m4)
```

[TABLE]

### Untransformed estimates on the linear scale

To plot the estimates on the linear scale, use `transform = NULL`.

``` r

tab_model(m3, m4, transform = NULL, auto.label = FALSE)
```

|   | tot_sc_e |  |  | neg_c_7d |  |  |
|:--:|:--:|:--:|:--:|:--:|:--:|----|
| Predictors | Log-Mean | CI | p | Log-Odds | CI | p |
| (Intercept) | -1.19 | -1.58 – -0.80 | **\<0.001** | 1.88 | 1.30 – 2.48 | **\<0.001** |
| c160age | 0.01 | 0.01 – 0.02 | **\<0.001** |  |  |  |
| c12hour | 0.00 | 0.00 – 0.00 | **\<0.001** |  |  |  |
| c161sex2 | 0.01 | -0.15 – 0.18 | 0.867 | 0.63 | 0.27 – 0.99 | **0.001** |
| c172code2 | 0.39 | 0.19 – 0.58 | **\<0.001** | 0.21 | -0.18 – 0.60 | 0.288 |
| c172code3 | 0.64 | 0.42 – 0.87 | **\<0.001** | 0.31 | -0.17 – 0.80 | 0.204 |
| barthtot |  |  |  | -0.03 | -0.04 – -0.03 | **\<0.001** |
| Observations | 840 |  |  | 815 |  |  |
| R² Nagelkerke | 0.106 |  |  | 0.191 |  |  |

## More complex models

Other models, like hurdle- or zero-inflated models, also work with
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).
In this case, the zero inflation model is indicated in the table. Use
`show.zeroinf = FALSE` to hide this part from the table.

``` r

library(pscl)
data("bioChemists")
m5 <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd + ment, data = bioChemists)

tab_model(m5)
```

|                     |          art          |             |             |     |
|:-------------------:|:---------------------:|:-----------:|:-----------:|-----|
|     Predictors      | Incidence Rate Ratios |     CI      |      p      |     |
|     Count Model     |                       |             |             |     |
|     (Intercept)     |         1.83          | 1.61 – 2.10 | **\<0.001** |     |
|    fem \[Women\]    |         0.80          | 0.72 – 0.90 | **\<0.001** |     |
|   mar \[Married\]   |         1.14          | 1.01 – 1.30 |  **0.041**  |     |
|        kid5         |         0.86          | 0.78 – 0.94 |  **0.001**  |     |
|        ment         |         1.02          | 1.01 – 1.02 | **\<0.001** |     |
| Zero-Inflated Model |                       |             |             |     |
|     (Intercept)     |         0.45          | 0.20 – 1.01 |    0.054    |     |
|        kid5         |         1.12          | 0.79 – 1.58 |    0.531    |     |
|         phd         |         1.02          | 0.78 – 1.33 |    0.881    |     |
|        ment         |         0.88          | 0.81 – 0.95 |  **0.002**  |     |
|    Observations     |          915          |             |             |     |
|  R² / R² adjusted   |     0.230 / 0.226     |             |             |     |

You can combine any model in one table.

``` r

tab_model(m1, m3, m5, auto.label = FALSE, show.ci = FALSE)
```

|   | barthtot |  | tot_sc_e |  | art |  |
|:--:|:--:|:--:|:--:|:--:|:--:|----|
| Predictors | Estimates | p | Incidence Rate Ratios | p | Incidence Rate Ratios | p |
| (Intercept) | 87.15 | **\<0.001** | 0.30 | **\<0.001** |  |  |
| c160age | -0.21 | **0.004** | 1.01 | **\<0.001** |  |  |
| c12hour | -0.28 | **\<0.001** | 1.00 | **\<0.001** |  |  |
| c161sex2 | -0.39 | 0.850 | 1.01 | 0.867 |  |  |
| c172code2 | 1.37 | 0.550 | 1.47 | **\<0.001** |  |  |
| c172code3 | -1.64 | 0.564 | 1.90 | **\<0.001** |  |  |
| count\_(Intercept) |  |  |  |  | 1.83 | **\<0.001** |
| count_femWomen |  |  |  |  | 0.80 | **\<0.001** |
| count_marMarried |  |  |  |  | 1.14 | **0.041** |
| count_kid5 |  |  |  |  | 0.86 | **0.001** |
| count_ment |  |  |  |  | 1.02 | **\<0.001** |
| Zero-Inflated Model |  |  |  |  |  |  |
| zero\_(Intercept) |  |  |  |  | 0.45 | 0.054 |
| zero_kid5 |  |  |  |  | 1.12 | 0.531 |
| zero_phd |  |  |  |  | 1.02 | 0.881 |
| zero_ment |  |  |  |  | 0.88 | **0.002** |
| Observations | 821 |  | 840 |  | 915 |  |
| R² / R² adjusted | 0.271 / 0.266 |  | 0.106 |  | 0.230 / 0.226 |  |

## Show or hide further columns

[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
has some argument that allow to show or hide specific columns from the
output:

- `show.est` to show/hide the column with model estimates.
- `show.ci` to show/hide the column with confidence intervals.
- `show.se` to show/hide the column with standard errors.
- `show.std` to show/hide the column with standardized estimates (and
  their standard errors).
- `show.p` to show/hide the column with p-values.
- `show.stat` to show/hide the column with the coefficients’ test
  statistics.
- `show.df` for linear mixed models, when p-values are based on degrees
  of freedom with Kenward-Rogers approximation, these degrees of freedom
  are shown.

### Adding columns

In the following example, standard errors, standardized coefficients and
test statistics are also shown.

``` r

tab_model(m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE)
```

[TABLE]

### Removing columns

In the following example, default columns are removed.

``` r

tab_model(m3, m4, show.ci = FALSE, show.p = FALSE, auto.label = FALSE)
```

|               |       tot_sc_e        |  neg_c_7d   |
|:-------------:|:---------------------:|:-----------:|
|  Predictors   | Incidence Rate Ratios | Odds Ratios |
|  (Intercept)  |         0.30          |    6.54     |
|    c160age    |         1.01          |             |
|    c12hour    |         1.00          |             |
|   c161sex2    |         1.01          |    1.87     |
|   c172code2   |         1.47          |    1.23     |
|   c172code3   |         1.90          |    1.37     |
|   barthtot    |                       |    0.97     |
| Observations  |          840          |     815     |
| R² Nagelkerke |         0.106         |    0.191    |

### Removing and sorting columns

Another way to remove columns, which also allows to reorder the columns,
is the `col.order`-argument. This is a character vector, where each
element indicates a column in the output. The value `"est"`, for
instance, indicates the estimates, while `"std.est"` is the column for
standardized estimates and so on.

By default, `col.order` contains all possible columns. All columns that
should shown (see previous tables, for example using `show.se = TRUE` to
show standard errors, or `show.st = TRUE` to show standardized
estimates) are then printed by default. Colums that are *excluded* from
`col.order` are *not shown*, no matter if the `show*`-arguments are
`TRUE` or `FALSE`. So if `show.se = TRUE`, but`col.order` does not
contain the element `"se"`, standard errors are not shown. On the other
hand, if `show.est = FALSE`, but `col.order` *does include* the element
`"est"`, the columns with estimates are not shown.

In summary, `col.order` can be used to *exclude* columns from the table
and to change the order of colums.

``` r

tab_model(
  m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE,
  col.order = c("p", "stat", "est", "std.se", "se", "std.est")
)
```

[TABLE]

### Collapsing columns

With `collapse.ci` and `collapse.se`, the columns for confidence
intervals and standard errors can be collapsed into one column together
with the estimates. Sometimes this table layout is required.

``` r

tab_model(m1, collapse.ci = TRUE)
```

[TABLE]

## Defining own labels

There are different options to change the labels of the column headers
or coefficients, e.g. with:

- `pred.labels` to change the names of the coefficients in the
  *Predictors* column. Note that the length of `pred.labels` must
  exactly match the amount of predictors in the *Predictor* column.
- `dv.labels` to change the names of the model columns, which are
  labelled with the variable labels / names from the dependent
  variables.
- Further more, there are various `string.*`-arguments, to change the
  name of column headings.

``` r

tab_model(
  m1, m2, 
  pred.labels = c("Intercept", "Age (Carer)", "Hours per Week", "Gender (Carer)",
                  "Education: middle (Carer)", "Education: high (Carer)", 
                  "Age (Older Person)"),
  dv.labels = c("First Model", "M2"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)
```

|   | First Model |  |  | M2 |  |  |
|:--:|:--:|:--:|:--:|:--:|:--:|----|
| Coeffcient | Estimates | Conf. Int (95%) | P-Value | Estimates | Conf. Int (95%) | P-Value |
| Intercept | 87.15 | 77.96 – 96.34 | **\<0.001** | 9.83 | 7.33 – 12.33 | **\<0.001** |
| Age (Carer) | -0.21 | -0.35 – -0.07 | **0.004** | 0.01 | -0.01 – 0.03 | 0.359 |
| Hours per Week | -0.28 | -0.32 – -0.24 | **\<0.001** | 0.02 | 0.01 – 0.02 | **\<0.001** |
| Gender (Carer) | -0.39 | -4.49 – 3.71 | 0.850 | 0.43 | -0.15 – 1.01 | 0.147 |
| Education: middle (Carer) | 1.37 | -3.12 – 5.85 | 0.550 |  |  |  |
| Education: high (Carer) | -1.64 | -7.22 – 3.93 | 0.564 |  |  |  |
| Age (Older Person) |  |  |  | 0.01 | -0.03 – 0.04 | 0.741 |
| Observations | 821 |  |  | 879 |  |  |
| R² / R² adjusted | 0.271 / 0.266 |  |  | 0.067 / 0.063 |  |  |

## Including reference level of categorical predictors

By default, for categorical predictors, the variable names and the
categories for regression coefficients are shown in the table output.

``` r

library(glmmTMB)
data("Salamanders")
model <- glm(
  count ~ spp + Wtemp + mined + cover,
  family = poisson(),
  data = Salamanders
)

tab_model(model)
```

|               |         count         |              |             |
|:-------------:|:---------------------:|:------------:|:-----------:|
|  Predictors   | Incidence Rate Ratios |      CI      |      p      |
|  (Intercept)  |         0.22          | 0.17 – 0.29  | **\<0.001** |
|  spp \[PR\]   |         0.25          | 0.16 – 0.38  | **\<0.001** |
|  spp \[DM\]   |         1.26          | 0.98 – 1.62  |    0.074    |
| spp \[EC-A\]  |         0.46          | 0.33 – 0.64  | **\<0.001** |
| spp \[EC-L\]  |         1.86          | 1.48 – 2.36  | **\<0.001** |
| spp \[DES-L\] |         1.97          | 1.57 – 2.49  | **\<0.001** |
|  spp \[DF\]   |         1.08          | 0.83 – 1.41  |    0.549    |
|     Wtemp     |         1.00          | 0.93 – 1.08  |    0.977    |
| mined \[no\]  |         9.97          | 7.91 – 12.69 | **\<0.001** |
|     cover     |         0.79          | 0.73 – 0.86  | **\<0.001** |
| Observations  |          644          |              |             |
| R² Nagelkerke |         0.758         |              |             |

You can include the reference level for categorical predictors by
setting `show.reflvl = TRUE`.

``` r

tab_model(model, show.reflvl = TRUE)
```

|               |         count         |              |             |
|:-------------:|:---------------------:|:------------:|:-----------:|
|  Predictors   | Incidence Rate Ratios |      CI      |      p      |
|  (Intercept)  |         0.22          | 0.17 – 0.29  | **\<0.001** |
|     cover     |         0.79          | 0.73 – 0.86  | **\<0.001** |
|      GP       |      *Reference*      |              |             |
|      PR       |         0.25          | 0.16 – 0.38  | **\<0.001** |
|      DM       |         1.26          | 0.98 – 1.62  |    0.074    |
|     EC-A      |         0.46          | 0.33 – 0.64  | **\<0.001** |
|     EC-L      |         1.86          | 1.48 – 2.36  | **\<0.001** |
|     DES-L     |         1.97          | 1.57 – 2.49  | **\<0.001** |
|      DF       |         1.08          | 0.83 – 1.41  |    0.549    |
|      yes      |      *Reference*      |              |             |
|      no       |         9.97          | 7.91 – 12.69 | **\<0.001** |
|     Wtemp     |         1.00          | 0.93 – 1.08  |    0.977    |
| Observations  |          644          |              |             |
| R² Nagelkerke |         0.758         |              |             |

To show variable names, categories and include the reference level, also
set `prefix.labels = "varname"`.

``` r

tab_model(model, show.reflvl = TRUE, prefix.labels = "varname")
```

|               |         count         |              |             |
|:-------------:|:---------------------:|:------------:|:-----------:|
|  Predictors   | Incidence Rate Ratios |      CI      |      p      |
|  (Intercept)  |         0.22          | 0.17 – 0.29  | **\<0.001** |
|     cover     |         0.79          | 0.73 – 0.86  | **\<0.001** |
|    spp: GP    |      *Reference*      |              |             |
|    spp: PR    |         0.25          | 0.16 – 0.38  | **\<0.001** |
|    spp: DM    |         1.26          | 0.98 – 1.62  |    0.074    |
|   spp: EC-A   |         0.46          | 0.33 – 0.64  | **\<0.001** |
|   spp: EC-L   |         1.86          | 1.48 – 2.36  | **\<0.001** |
|  spp: DES-L   |         1.97          | 1.57 – 2.49  | **\<0.001** |
|    spp: DF    |         1.08          | 0.83 – 1.41  |    0.549    |
|  mined: yes   |      *Reference*      |              |             |
|   mined: no   |         9.97          | 7.91 – 12.69 | **\<0.001** |
|     Wtemp     |         1.00          | 0.93 – 1.08  |    0.977    |
| Observations  |          644          |              |             |
| R² Nagelkerke |         0.758         |              |             |

## Style of p-values

You can change the style of how p-values are displayed with the argument
`p.style`. With `p.style = "stars"`, the p-values are indicated as `*`
in the table.

``` r

tab_model(m1, m2, p.style = "stars")
```

[TABLE]

Another option would be scientific notation, using
`p.style = "scientific"`, which also can be combined with `digits.p`.

``` r

tab_model(m1, m2, p.style = "scientific", digits.p = 2)
```

[TABLE]

### Automatic matching for named vectors

Another way to easily assign labels are *named vectors*. In this case,
it doesn’t matter if `pred.labels` has more labels than coefficients in
the model(s), or in which order the labels are passed to
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).
The only requirement is that the labels’ names equal the coefficients
names as they appear in the
[`summary()`](https://rdrr.io/r/base/summary.html)-output.

``` r

# example, coefficients are "c161sex2" or "c172code3"
summary(m1)
#> 
#> Call:
#> lm(formula = barthtot ~ c160age + c12hour + c161sex + c172code, 
#>     data = efc)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -75.144 -14.944   4.401  18.661  72.393 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 87.14994    4.68009  18.621  < 2e-16 ***
#> c160age     -0.20716    0.07211  -2.873  0.00418 ** 
#> c12hour     -0.27883    0.01865 -14.950  < 2e-16 ***
#> c161sex2    -0.39402    2.08893  -0.189  0.85044    
#> c172code2    1.36596    2.28440   0.598  0.55004    
#> c172code3   -1.64045    2.84037  -0.578  0.56373    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 25.35 on 815 degrees of freedom
#>   (87 observations deleted due to missingness)
#> Multiple R-squared:  0.2708, Adjusted R-squared:  0.2664 
#> F-statistic: 60.54 on 5 and 815 DF,  p-value: < 2.2e-16

pl <- c(
  `(Intercept)` = "Intercept",
  e17age = "Age (Older Person)",
  c160age = "Age (Carer)", 
  c12hour = "Hours per Week", 
  barthtot = "Barthel-Index",
  c161sex2 = "Gender (Carer)",
  c172code2 = "Education: middle (Carer)", 
  c172code3 = "Education: high (Carer)",
  a_non_used_label = "We don't care"
)
 
tab_model(
  m1, m2, m3, m4, 
  pred.labels = pl, 
  dv.labels = c("Model1", "Model2", "Model3", "Model4"),
  show.ci = FALSE, 
  show.p = FALSE, 
  transform = NULL
)
```

|                           |    Model1     |    Model2     |  Model3  |  Model4  |
|:-------------------------:|:-------------:|:-------------:|:--------:|:--------:|
|        Predictors         |   Estimates   |   Estimates   | Log-Mean | Log-Odds |
|         Intercept         |     87.15     |     9.83      |  -1.19   |   1.88   |
|        Age (Carer)        |     -0.21     |     0.01      |   0.01   |          |
|      Hours per Week       |     -0.28     |     0.02      |   0.00   |          |
|      Gender (Carer)       |     -0.39     |     0.43      |   0.01   |   0.63   |
| Education: middle (Carer) |     1.37      |               |   0.39   |   0.21   |
|  Education: high (Carer)  |     -1.64     |               |   0.64   |   0.31   |
|    Age (Older Person)     |               |     0.01      |          |          |
|       Barthel-Index       |               |               |          |  -0.03   |
|       Observations        |      821      |      879      |   840    |   815    |
|     R² / R² adjusted      | 0.271 / 0.266 | 0.067 / 0.063 |  0.106   |  0.191   |

## Keep or remove coefficients from the table

Using the `terms`- or `rm.terms`-argument allows us to explicitly show
or remove specific coefficients from the table output.

``` r

tab_model(m1, terms = c("c160age", "c12hour"))
```

[TABLE]

Note that the names of terms to keep or remove should match the
coefficients names. For categorical predictors, one example would be:

``` r

tab_model(m1, rm.terms = c("c172code2", "c161sex2"))
```

[TABLE]
