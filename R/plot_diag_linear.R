plot_diag_linear <- function(model,
                             geom.colors,
                             dot.size,
                             line.size,
                             ...) {
  plot.list <- list()
  geom.colors <- col_check2(geom.colors, 2)

  p <- diag_vif(model)
  if (!is.null(p)) plot.list[[length(plot.list) + 1]] <- p

  p <- diag_qq(model, geom.colors, dot.size, line.size)
  if (!is.null(p)) plot.list[[length(plot.list) + 1]] <- p

  p <- diag_reqq(model, dot.size)
  if (!is.null(p)) plot.list[[length(plot.list) + 1]] <- p

  p <- diag_norm(model, geom.colors)
  if (!is.null(p)) plot.list[[length(plot.list) + 1]] <- p

  p <- diag_ncv(model, dot.size, line.size)
  if (!is.null(p)) plot.list[[length(plot.list) + 1]] <- p

  plot.list
}


plot_diag_glm <- function(model, geom.colors, dot.size, line.size, ...) {
  geom.colors <- col_check2(geom.colors, 2)
  diag_reqq(model, dot.size)
}


diag_ncv <- function(model, dot.size, line.size) {

  if (is.null(dot.size)) dot.size <- 1
  if (is.null(line.size)) line.size <- 1

  dat <- data.frame(
    res = stats::residuals(model),
    fitted = stats::fitted(model)
  )

  ggplot2::ggplot(dat, ggplot2::aes_string(x = "fitted", y = "res")) +
    geom_intercept_line2(0, NULL) +
    ggplot2::geom_point(size = dot.size) +
    geom_smooth(method = "loess", se = FALSE, size = line.size) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Homoscedasticity (constant variance of residuals)",
      subtitle = "Amount and distance of points scattered above/below line is equal or randomly spread"
    )
}


#' @importFrom rlang .data
diag_norm <- function(model, geom.colors) {
  res_ <- data.frame(res = stats::residuals(model))

  ggplot2::ggplot(res_, ggplot2::aes_string(x = "res")) +
    ggplot2::geom_density(fill = geom.colors[1], alpha = 0.2) +
    ggplot2::stat_function(
      fun = dnorm,
      args = list(
        mean = mean(unname(stats::residuals(model)), na.rm = TRUE),
        sd = stats::sd(unname(stats::residuals(model)), na.rm = TRUE)
      ),
      colour = geom.colors[2],
      size = 0.8
    ) +
    ggplot2::labs(
      x = "Residuals",
      y = "Density",
      title = "Non-normality of residuals",
      subtitle = "Distribution should look like normal curve"
    )
}


diag_qq <- function(model, geom.colors, dot.size, line.size, ...) {

  if (is.null(dot.size)) dot.size <- 1
  if (is.null(line.size)) line.size <- 1

  # qq-plot of studentized residuals
  if (inherits(model, c("lme", "lmerMod", "glmmTMB"))) {
    res_ <- sort(stats::residuals(model), na.last = NA)
    y_lab <- "Residuals"
  } else {
    # else, normal model
    res_ <- sort(stats::rstudent(model), na.last = NA)
    y_lab <- "Studentized Residuals"
  }

  fitted_ <- sort(stats::fitted(model), na.last = NA)

  # create data frame
  mydf <- stats::na.omit(data.frame(x = fitted_, y = res_))

  # plot it
  ggplot2::ggplot(mydf, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_point(size = dot.size) +
    ggplot2::scale_colour_manual(values = geom.colors) +
    ggplot2::stat_smooth(method = "lm", se = FALSE, size = line.size) +
    ggplot2::labs(
      title = "Non-normality of residuals and outliers",
      subtitle = "Dots should be plotted along the line",
      y = y_lab,
      x = "Theoretical quantiles (predicted values)"
    )
}


diag_reqq <- function(model, dot.size) {

  if (!is_merMod(model) && !inherits(model, "glmmTMB")) return(NULL)

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work, please install it.")
  }
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("Package 'glmmTMB' required for this function to work, please install it.")
  }

  if (inherits(model, "glmmTMB")) {
    re <- glmmTMB::ranef(model)[[1]]
    s1 <- TMB::sdreport(model$obj, getJointPrecision = TRUE)
    s2 <- sqrt(s1$diag.cov.random)
    se <- purrr::map(re, function(.x) {
      cnt <- nrow(.x) * ncol(.x)
      s3 <- s2[1:cnt]
      s2 <- s2[-(1:cnt)]
      s3
    })
  } else {
    re   <- lme4::ranef(model, condVar = TRUE)
    se <- purrr::map(re, function(.x) {
      pv   <- attr(.x, "postVar")
      cols <- seq_len(dim(pv)[1])
      unlist(lapply(cols, function(.y) sqrt(pv[.y, .y, ])))
    })
  }


  alpha <- .3
  if (is.null(dot.size)) dot.size <- 2

  # get ...-arguments
  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- eval(add.args[["alpha"]])


  purrr::map2(re, se, function(.re, .se) {
    ord  <- unlist(lapply(.re, order)) + rep((0:(ncol(.re) - 1)) * nrow(.re), each = nrow(.re))

    df.y <- unlist(.re)[ord]
    df.ci <- stats::qnorm(.975) * .se[ord]

    pDf  <- data_frame(
      y = df.y,
      ci = df.ci,
      nQQ = rep(stats::qnorm(stats::ppoints(nrow(.re))), ncol(.re)),
      ID = factor(rep(rownames(.re), ncol(.re))[ord], levels = rownames(.re)[ord]),
      ind = gl(ncol(.re), nrow(.re), labels = names(.re)),
      conf.low = df.y - df.ci,
      conf.high = df.y + df.ci
    )

    ggplot2::ggplot(pDf, ggplot2::aes_string(
      x = "nQQ",
      y = "y"
    )) +
      ggplot2::facet_wrap(~ ind, scales = "free") +
      ggplot2::labs(x = "Standard normal quantiles", y = "Random effect quantiles") +
      geom_intercept_line2(0, NULL) +
      ggplot2::stat_smooth(method = "lm", alpha = alpha) +
      ggplot2::geom_errorbar(
        ggplot2::aes_string(ymin = "conf.low", ymax = "conf.high"),
        width = 0,
        colour = "black"
      ) +
      ggplot2::geom_point(size = dot.size, colour = "darkblue")
  })
}


diag_vif <- function(fit) {

  if (is_merMod(fit) || inherits(fit, "lme"))
    return(NULL)

  if (!requireNamespace("car", quietly = TRUE))
    stop("Package `car` needed for this function to work. Please install it.", call. = FALSE)

  vifplot <- NULL

  # check if we have more than 1 term

  if (length(stats::coef(fit)) > 2) {

    # variance inflation factor
    # claculate VIF

    vifval <- car::vif(fit)

    if (is.matrix(vifval)) {
      val <- vifval[, 1]
    } else {
      val <- vifval
    }

    # retrieve highest VIF-value to determine y-axis range
    maxval <- val[which.max(val)]

    # determine upper limit of y-axis
    upperLimit <- 10

    # check whether maxval exceeds the critical VIF-Limit
    # of 10. If so, set upper limit to max. value

    if (maxval >= upperLimit) upperLimit <- ceiling(maxval)

    mydat <- data.frame(vif = round(val, 2)) |>
      rownames_as_column(var = "vars")


    vifplot <- ggplot2::ggplot(mydat, ggplot2::aes_string(x = "vars", y = "vif")) +
      ggplot2::geom_bar(stat = "identity", width = 0.7, fill = "#80acc8") +
      ggplot2::geom_hline(yintercept = 5, linetype = 2, colour = "darkgreen", alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 10, linetype = 2, colour = "darkred", alpha = 0.7) +
      ggplot2::annotate("text", x = 1, y = 4.7, label = "good", size = 4, colour = "darkgreen") +
      ggplot2::annotate("text", x = 1, y = 9.7, label = "tolerable", size = 4, colour = "darkred") +
      ggplot2::labs(title = "Variance Inflation Factors (multicollinearity)", x = NULL, y = NULL) +
      ggplot2::scale_y_continuous(limits = c(0, upperLimit), expand = c(0, 0))
  }

  vifplot
}
