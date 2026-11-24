# Robust Estimation of Standard Errors, Confidence Intervals and p-values

The
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
function also allows the computation of standard errors, confidence
intervals and p-values based on robust covariance matrix estimation from
model parameters. Robust estimation is based on the packages
**sandwich** and **clubSandwich**, so all models supported by either of
these packages work with
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md).

## Classical Regression Models

### Robust Covariance Matrix Estimation from Model Parameters

There are two arguments that allow for choosing different methods and
options of robust estimation: `vcov.fun` and `vcov.args`. Let us start
with a simple example, which uses a heteroskedasticity-consistent
covariance matrix estimation with estimation-type “HC3”
(i.e. `sandwich::vcovHC(type = "HC3")` is called):

``` r

data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species + Sepal.Width, data = iris)

# model parameters, where SE, CI and p-values are based on robust estimation
tab_model(model, vcov.fun = "HC3", show.se = TRUE)
```

[TABLE]

``` r


# compare standard errors to result from sandwich-package
unname(sqrt(diag(sandwich::vcovHC(model))))
#> [1] 0.45382603 0.11884474 0.69296611 0.63031982 0.08318559 0.13045539 0.11841325
```

### Cluster-Robust Covariance Matrix Estimation (sandwich)

If another covariance matrix estimation is required, use the
`vcov.fun`-argument. This argument needs the suffix for the related
`vcov*()`-functions as value, i.e. `vcov.fun = "CL"` would call
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html),
or `vcov.fun = "HAC"` would call
[`sandwich::vcovHAC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHAC.html).

The specific estimation type can be changed with `vcov.args`. E.g.,
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
accepts estimation types HC0 to HC3. In the next example, we use a
clustered covariance matrix estimation with HC1-estimation type.

``` r

# change estimation-type
tab_model(model, vcov.fun = "CL", vcov.args = list(type = "HC1"), show.se = TRUE)
```

[TABLE]

``` r


# compare standard errors to result from sandwich-package
unname(sqrt(diag(sandwich::vcovCL(model))))
#> [1] 0.42197635 0.11148130 0.65274212 0.58720711 0.07934029 0.12251570 0.11058144
```

Usually, clustered covariance matrix estimation is used when there is a
cluster-structure in the data. The variable indicating the
cluster-structure can be defined in
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
with the `cluster`-argument. In
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md),
additional arguments that should be passed down to functions from the
**sandwich** package can be specified in `vcov.args`:

``` r

iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))
# change estimation-type, defining additional arguments
tab_model(
  model, 
  vcov.fun = "CL", 
  vcov.args = list(type = "HC1", cluster = iris$cluster),
  show.se = TRUE
)
```

[TABLE]

``` r


# compare standard errors to result from sandwich-package
unname(sqrt(diag(sandwich::vcovCL(model, cluster = iris$cluster))))
#> [1] 0.33714287 0.07192334 0.51893777 0.26415406 0.07201145 0.09661348 0.05123446
```

### Cluster-Robust Covariance Matrix Estimation (clubSandwich)

Cluster-robust estimation of the variance-covariance matrix can also be
achieved using
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).
Thus, when `vcov.fun = "CR"`, the related function from the
**clubSandwich** package is called. Note that this function *requires*
the specification of the `cluster`-argument.

``` r

# create fake-cluster-variable, to demonstrate cluster robust standard errors
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

# cluster-robust estimation
tab_model(
  model, 
  vcov.fun = "CR1", 
  vcov.args = list(cluster = iris$cluster),
  show.se = TRUE
)
```

[TABLE]

``` r


# compare standard errors to result from clubSsandwich-package
unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))
#> [1] 0.33028501 0.07046034 0.50838200 0.25878087 0.07054666 0.09464825 0.05019229
```

### Robust Covariance Matrix Estimation on Standardized Model Parameters

Finally, robust estimation can be combined with standardization.
However, robust covariance matrix estimation only works for
`show.std = "std"`.

``` r

# model parameters, robust estimation on standardized model
tab_model(
  model, 
  show.std = "std",
  vcov.fun = "HC"
)
```

[TABLE]

## Mixed Models

### Robust Covariance Matrix Estimation for Mixed Models

For linear mixed models, that by definition have a clustered
(“hierarchical” or multilevel) structure in the data, it is also
possible to estimate a cluster-robust covariance matrix. This is
possible due to the **clubSandwich** package, thus we need to define the
same arguments as in the above example.

``` r

library(lme4)
data(iris)
set.seed(1234)
iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

# fit example model
model <- lme4::lmer(
  Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
  data = iris
)

# normal model parameters, like from 'summary()'
tab_model(model)
```

[TABLE]

``` r


# model parameters, cluster robust estimation for mixed models
tab_model(
  model, 
  vcov.fun = "CR1", 
  vcov.args = list(cluster = iris$grp)
)
```

[TABLE]

### Robust Covariance Matrix Estimation on Standardized Mixed Model Parameters

Again, robust estimation can be combined with standardization for linear
mixed models as well, which in such cases also only works for
`show.std = "std"`.

``` r

# model parameters, cluster robust estimation on standardized mixed model
tab_model(
  model, 
  show.std = "std",
  vcov.fun = "CR1", 
  vcov.args = list(cluster = iris$grp)
)
```

[TABLE]
