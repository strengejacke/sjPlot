test_that("glmmTMB random effects use the conditional standard deviations", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")

  model <- glmmTMB::glmmTMB(
    diameter ~ (1 | plate) + (1 | sample),
    data = lme4::Penicillin
  )
  expected <- as.data.frame(glmmTMB::ranef(model, condVar = TRUE))
  plots <- suppressWarnings(
    plot_model(model, type = "re", transform = NULL),
    classes = "lifecycle_warning_deprecated"
  )
  groups <- unique(expected$grpvar)
  expect_length(plots, length(groups))

  for (j in seq_along(plots)) {
    dat <- plots[[j]]$data
    group_sd <- expected[expected$grpvar == groups[j], ]
    sd <- group_sd$condsd[match(as.character(dat$term), group_sd$grp)]
    margin <- stats::qnorm(0.975) * sd
    expect_equal(dat$estimate - dat$conf.low, margin)
    expect_equal(dat$conf.high - dat$estimate, margin)
  }
})
