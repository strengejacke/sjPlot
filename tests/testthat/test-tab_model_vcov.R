test_that("tab_model and plot_model use Wald CIs by default when robust vcov is supplied", {
  skip_if_not_installed("sandwich")

  set.seed(42)
  n <- 200
  cluster <- rep(1:20, each = 10)
  u <- stats::rnorm(20, 0, 1.5)[cluster]
  x <- stats::rnorm(n) + 0.5 * u
  y <- stats::rbinom(n, 1, stats::plogis(-0.5 + 0.8 * x + u))
  d <- data.frame(y = y, x = x, cluster = cluster)

  m <- stats::glm(y ~ x, data = d, family = stats::binomial())
  vcov_args <- list(type = "HC0", cluster = d$cluster)

  mp_wald <- parameters::model_parameters(
    m,
    vcov = "CL",
    vcov_args = vcov_args,
    ci_method = "wald",
    verbose = FALSE
  )
  mp_prof <- parameters::model_parameters(m, ci_method = "profile", verbose = FALSE)

  # plot_model with robust vcov matches robust Wald CIs, not profile CIs
  p_robust <- suppressWarnings(
    plot_model(m, vcov.fun = "CL", vcov.args = vcov_args, transform = NULL),
    classes = "lifecycle_warning_deprecated"
  )
  expect_equal(p_robust$data$conf.low, mp_wald$CI_low[2], tolerance = 1e-6)
  expect_equal(p_robust$data$conf.high, mp_wald$CI_high[2], tolerance = 1e-6)
  expect_false(isTRUE(all.equal(p_robust$data$conf.low, mp_prof$CI_low[2], tolerance = 1e-3)))

  # tab_model with robust vcov renders exponentiated robust Wald CIs
  tab <- suppressWarnings(
    tab_model(m, vcov.fun = "CL", vcov.args = vcov_args, show.se = TRUE),
    classes = "lifecycle_warning_deprecated"
  )
  expected_ci <- sprintf("%.2f&nbsp;&ndash;&nbsp;%.2f", exp(mp_wald$CI_low[2]), exp(mp_wald$CI_high[2]))
  profile_ci <- sprintf("%.2f&nbsp;&ndash;&nbsp;%.2f", exp(mp_prof$CI_low[2]), exp(mp_prof$CI_high[2]))
  expect_true(grepl(expected_ci, tab$page.content, fixed = TRUE))
  expect_false(grepl(profile_ci, tab$page.content, fixed = TRUE))

  # Default non-robust GLM still uses profile CIs
  p_default <- suppressWarnings(
    plot_model(m, transform = NULL),
    classes = "lifecycle_warning_deprecated"
  )
  expect_equal(p_default$data$conf.low, mp_prof$CI_low[2], tolerance = 1e-6)
  expect_equal(p_default$data$conf.high, mp_prof$CI_high[2], tolerance = 1e-6)

  # Explicit p.val = "profile" is still respected
  p_explicit <- suppressWarnings(
    plot_model(m, vcov.fun = "CL", vcov.args = vcov_args, p.val = "profile", transform = NULL),
    classes = "lifecycle_warning_deprecated"
  )
  expect_equal(p_explicit$data$conf.low, mp_prof$CI_low[2], tolerance = 1e-6)
  expect_equal(p_explicit$data$conf.high, mp_prof$CI_high[2], tolerance = 1e-6)
})

test_that("tab_model uses Wald CIs by default for Poisson GLM with HC3 robust vcov", {
  skip_if_not_installed("sandwich")

  set.seed(123)
  x <- stats::rnorm(150)
  y <- stats::rpois(150, exp(0.2 + 0.5 * x + stats::rnorm(150, 0, 0.7)))
  m <- stats::glm(y ~ x, family = stats::poisson())

  mp_wald <- parameters::model_parameters(m, vcov = "HC3", ci_method = "wald", verbose = FALSE)
  mp_prof <- parameters::model_parameters(m, ci_method = "profile", verbose = FALSE)

  tab <- suppressWarnings(
    tab_model(m, vcov.fun = "HC3", show.se = TRUE),
    classes = "lifecycle_warning_deprecated"
  )
  expected_ci <- sprintf("%.2f&nbsp;&ndash;&nbsp;%.2f", exp(mp_wald$CI_low[2]), exp(mp_wald$CI_high[2]))
  profile_ci <- sprintf("%.2f&nbsp;&ndash;&nbsp;%.2f", exp(mp_prof$CI_low[2]), exp(mp_prof$CI_high[2]))
  expect_true(grepl(expected_ci, tab$page.content, fixed = TRUE))
  expect_false(grepl(profile_ci, tab$page.content, fixed = TRUE))
})
