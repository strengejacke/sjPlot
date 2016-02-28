# bind global variables
utils::globalVariables(c("p.level"))


#' @title Plot normal distributions
#' @name dist_norm
#' 
#' @description This function plots a simple normal distribution or a normal distribution
#'                with shaded areas that indicate at which value a significant p-level 
#'                is reached.
#' 
#' @param norm numeric, optional. If specified, a normal distribution with \code{mean} and \code{sd}
#'          is plotted and a shaded area at \code{norm} value position is plotted that
#'          indicates whether or not the specified value is significant or not.
#'          If both \code{norm} and \code{p} are not specified, a distribution without shaded
#'          area is plotted.
#' @param mean numeric. Mean value for normal distribution. By default 0.
#' @param sd numeric. Standard deviation for normal distribution. By default 1.
#' @param p numeric, optional. If specified, a normal distribution with \code{mean} and \code{sd}
#'          is plotted and a shaded area at the position where the specified p-level
#'          starts is plotted. If both \code{norm} and \code{p} are not specified, a distribution 
#'          without shaded area is plotted.
#' @param xmax numeric, optional. Specifies the maximum x-axis-value. If not specified, the x-axis
#'          ranges to a value where a p-level of 0.00001 is reached.
#' @param geom.colors User defined color palette for shaded areas.
#'          \itemize{
#'            \item If not specified, the qualitative \code{"Paired"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{\link[RColorBrewer]{display.brewer.all}} from the \pkg{RColorBrewer}-package to view all available palette names.
#'            \item Else specify your own color values as vector (e.g. \code{geom.colors = c("#f00000", "#00ff00")}).
#'          }
#' @param geom.alpha specified the alpha-level of the shaded area. Default is 0.7, range between 0 to 1.
#' 
#' @examples
#' # a simple normal distribution
#' dist_norm()
#' 
#' # a simple normal distribution with different mean and sd. 
#' # note that curve looks similar to above plot, but axis range
#' # has changed.
#' dist_norm(mean = 2, sd = 4)
#' 
#' # a simple normal distribution
#' dist_norm(norm = 1)
#' 
#' # a simple normal distribution
#' dist_norm(p = 0.2)
#' 
#' @import ggplot2
#' @importFrom stats qchisq pchisq dchisq qf pf df qnorm pnorm dnorm qt pt dt
#' @export
dist_norm <- function(norm = NULL,
                     mean = 0,
                     sd = 1,
                     p = NULL,
                     xmax = NULL,
                     geom.colors = NULL,
                     geom.alpha = 0.7) {
  # --------------------------------------
  # determine maximum range of x-axis.
  # --------------------------------------
  if (is.null(xmax)) {
    if (is.null(norm)) {
      n.max <- stats::qnorm(0.00001, mean, sd, lower.tail = F)
    }
    # --------------------------------------
    # else, if we have a x-value, take into
    # account all possible x-valuess that would lead
    # to a theoretical p-value of 0.00001.
    # --------------------------------------
    else {
      n.max <- norm
      while (stats::pnorm(n.max, mean, sd, lower.tail = F) > 0.00001) {
        n.max <- n.max + 1
      }
    }
  }
  else {
    n.max <- xmax
  }
  # --------------------------------------
  # create data frame
  # --------------------------------------
  mydat <- data.frame(x = seq(-n.max, n.max, length.out = 20 * n.max))
  # density normal distribution
  mydat$y <- stats::dnorm(mydat$x, mean, sd)
  # base plot with normal-distribution
  gp <- ggplot(mydat, aes(x = x, y = y)) + geom_line()
  sub.df <- NULL
  if (!is.null(p)) {
    # plot area for indicated x-value...
    sub.df <- mydat[mydat$x > stats::qnorm(p, mean, sd, lower.tail = F), ]
  }
  else if (!is.null(norm)) {
    # resp. for p-value...
    sub.df <- mydat[mydat$x > norm, ]
  }
  if (!is.null(sub.df)) {
    sub.df$p.level  <- ifelse(sub.df$x > stats::qnorm(0.05, mean, sd, lower.tail = F), "sig", "non-sig")
    cs <- stats::qnorm(0.05, mean, sd, lower.tail = F)
    gp <- gp +
      geom_ribbon(data = sub.df,
                  aes(ymax = y, fill = p.level),
                  ymin = 0,
                  alpha = geom.alpha) +
      annotate("text", 
               label = sprintf("x = %.2f", cs), 
               x = cs, 
               y = 0,
               vjust = 1.3)
    # add limit of p-value
    if (!is.null(norm)) {
      pv <- stats::pnorm(norm, mean, sd, lower.tail = F)
      if (pv >= 0.05) {
        gp <- gp +
          annotate("text", 
                   label = sprintf("p = %.2f", pv), 
                   x = norm, 
                   y = 0,
                   hjust = -0.1,
                   vjust = -0.5,
                   angle = 90)
      }
    }
  }
  gp <- sj.setGeomColors(gp, geom.colors, pal.len = 2, labels = c("p > 5%", "p < 0.05"))
  gp <- gp + ylab(NULL) + xlab(NULL)
  print(gp)
}


#' @title Plot chi-squared distributions
#' @name dist_chisq
#' 
#' @description This function plots a simple chi-squared distribution or a chi-squared distribution
#'                with shaded areas that indicate at which chi-squared value a significant p-level 
#'                is reached.
#' 
#' @param chi2 numeric, optional. If specified, a chi-squared distribution with \code{deg.f} degrees
#'          of freedom is plotted and a shaded area at \code{chi2} value position is plotted that
#'          indicates whether or not the specified value is significant or not.
#'          If both \code{chi2} and \code{p} are not specified, a distribution without shaded
#'          area is plotted.
#' @param deg.f numeric. The degrees of freedom for the chi-squared distribution. Needs to
#'          be specified.
#' @param p numeric, optional. If specified, a chi-squared distribution with \code{deg.f} degrees
#'          of freedom is plotted and a shaded area at the position where the specified p-level
#'          starts is plotted. If both \code{chi2} and \code{p} are not specified, a distribution 
#'          without shaded area is plotted.
#' @param xmax numeric, optional. Specifies the maximum x-axis-value. If not specified, the x-axis
#'          ranges to a value where a p-level of 0.00001 is reached.
#' 
#' @inheritParams dist_norm
#' 
#' @examples
#' # a simple chi-squared distribution
#' # for 6 degrees of freedom
#' dist_chisq(deg.f = 6)
#' 
#' # a chi-squared distribution for 6 degrees of freedom,
#' # and a shaded area starting at chi-squared value of ten.
#' # With a df of 6, a chi-squared value of 12.59 would be "significant",
#' # thus the shaded area from 10 to 12.58 is filled as "non-significant",
#' # while the area starting from chi-squared value 12.59 is filled as
#' # "significant"
#' dist_chisq(chi2 = 10, deg.f = 6)
#' 
#' # a chi-squared distribution for 6 degrees of freedom,
#' # and a shaded area starting at that chi-squared value, which has
#' # a p-level of about 0.125 (which equals a chi-squared value of about 10).
#' # With a df of 6, a chi-squared value of 12.59 would be "significant",
#' # thus the shaded area from 10 to 12.58 (p-level 0.125 to p-level 0.05) 
#' # is filled as "non-significant", while the area starting from chi-squared 
#' # value 12.59 (p-level < 0.05) is filled as "significant".
#' dist_chisq(p = 0.125, deg.f = 6)
#' 
#' @import ggplot2
#' @export
dist_chisq <- function(chi2 = NULL,
                      deg.f = NULL,
                      p = NULL,
                      xmax = NULL,
                      geom.colors = NULL,
                      geom.alpha = 0.7) {
  # --------------------------------------
  # check parameters
  # --------------------------------------
  if (is.null(deg.f)) {
    warning("Degrees of freedom ('deg.f') needs to be specified.", call. = F)
    return(invisible(NULL))
  }
  # --------------------------------------
  # determine maximum range of x-axis. if we have
  # p-value but no chi2-value, distribution should range until
  # a theoretical p-value of 0.00001 is reached. this should
  # cover all possible (and visible) chi2-values
  # --------------------------------------
  if (is.null(xmax)) {
    if (is.null(chi2)) {
      chisq.max <- stats::qchisq(0.00001, deg.f, lower.tail = F)
    }
    # --------------------------------------
    # else, if we have a chi2-value, take into
    # account all possible chi2-values that would lead
    # to a theoretical p-value of 0.00001.
    # --------------------------------------
    else {
      chisq.max <- chi2
      while (stats::pchisq(chisq.max, deg.f, lower.tail = F) > 0.00001) {
        chisq.max <- chisq.max + 1
      }
    }
  }
  else {
    chisq.max <- xmax
  }
  # --------------------------------------
  # create data frame
  # --------------------------------------
  mydat <- data.frame(x = seq(0, chisq.max, length.out = 10 * chisq.max))
  # density distribution of chi2
  mydat$y <- stats::dchisq(mydat$x, deg.f)
  # base plot with chi2-distribution
  gp <- ggplot(mydat, aes(x = x, y = y)) + geom_line()
  sub.df <- NULL
  if (!is.null(p)) {
    # plot area for indicated chi2-value...
    sub.df <- mydat[mydat$x > stats::qchisq(p, deg.f, lower.tail = F), ]
  }
  else if (!is.null(chi2)) {
    # resp. for p-value...
    sub.df <- mydat[mydat$x > chi2, ]
  }
  if (!is.null(sub.df)) {
    sub.df$p.level  <- ifelse(sub.df$x > stats::qchisq(0.05, deg.f, lower.tail = F), "sig", "non-sig")
    cs <- stats::qchisq(0.05, deg.f, lower.tail = F)
    gp <- gp +
      geom_ribbon(data = sub.df,
                  aes(ymax = y, fill = p.level),
                  ymin = 0,
                  alpha = geom.alpha) +
      annotate("text", 
               label = as.character(as.expression(substitute(chi^2 == c2, list(c2 = sprintf("%.2f", cs))))), 
               parse = TRUE, 
               x = cs, 
               y = 0,
               vjust = 1.2)
    # add limit of p-value
    if (!is.null(chi2)) {
      pv <- stats::pchisq(chi2, deg.f, lower.tail = F)
      if (pv >= 0.05) {
        gp <- gp +
          annotate("text", 
                   label = sprintf("p = %.2f", pv), 
                   x = chi2, 
                   y = 0,
                   hjust = -0.1,
                   vjust = -0.5,
                   angle = 90)
      }
    }
  }
  gp <- sj.setGeomColors(gp, geom.colors, pal.len = 2, labels = c("p > 5%", "p < 0.05"))
  gp <- gp + ylab(NULL) + xlab("chi-squared value")
  print(gp)
}


#' @title Plot F distributions
#' @name dist_f
#' 
#' @description This function plots a simple F distribution or an F distribution
#'                with shaded areas that indicate at which F value a significant p-level 
#'                is reached.
#' 
#' @param f numeric, optional. If specified, an F distribution with \code{deg.f1} and \code{deg.f2} degrees
#'          of freedom is plotted and a shaded area at \code{f} value position is plotted that
#'          indicates whether or not the specified value is significant or not.
#'          If both \code{f} and \code{p} are not specified, a distribution without shaded
#'          area is plotted.
#' @param deg.f1 numeric. The first degrees of freedom for the F distribution. Needs to
#'          be specified.
#' @param deg.f2 numeric. The second degrees of freedom for the F distribution. Needs to
#'          be specified.
#' @param p numeric, optional. If specified, a F distribution with \code{deg.f1} and \code{deg.f2} degrees
#'          of freedom is plotted and a shaded area at the position where the specified p-level
#'          starts is plotted. If both \code{f} and \code{p} are not specified, a distribution 
#'          without shaded area is plotted.
#' @param xmax numeric, optional. Specifies the maximum x-axis-value. If not specified, the x-axis
#'          ranges to a value where a p-level of 0.00001 is reached.
#' 
#' @inheritParams dist_norm
#' 
#' @examples
#' # a simple F distribution for 6 and 45 degrees of freedom
#' dist_f(deg.f1 = 6, deg.f2 = 45)
#' 
#' # F distribution for 6 and 45 degrees of freedom,
#' # and a shaded area starting at F value of two.
#' # F-values equal or greater than 2.31 are "significant"
#' dist_f(f = 2, deg.f1 = 6, deg.f2 = 45)
#' 
#' # F distribution for 6 and 45 degrees of freedom,
#' # and a shaded area starting at a p-level of 0.2
#' # (F-Value about 1.5).
#' dist_f(p = 0.2, deg.f1 = 6, deg.f2 = 45)
#' 
#' @import ggplot2
#' @export
dist_f <- function(f = NULL,
                  deg.f1 = NULL,
                  deg.f2 = NULL,
                  p = NULL,
                  xmax = NULL,
                  geom.colors = NULL,
                  geom.alpha = 0.7) {
  # --------------------------------------
  # check parameters
  # --------------------------------------
  if (is.null(deg.f1) || is.null(deg.f2)) {
    warning("Both degrees of freedom ('deg.f1' and 'deg.f2') needs to be specified.", call. = F)
    return(invisible(NULL))
  }
  # --------------------------------------
  # determine maximum range of x-axis. if we have
  # p-value but no f-value, distribution should range until
  # a theoretical p-value of 0.00001 is reached. this should
  # cover all possible (and visible) f-values
  # --------------------------------------
  if (is.null(xmax)) {
    if (is.null(f)) {
      f.max <- stats::qf(0.00001, deg.f1, deg.f2, lower.tail = F)
    # --------------------------------------
    # else, if we have a f-value, take into
    # account all possible f-values that would lead
    # to a theoretical p-value of 0.00001.
    # --------------------------------------
    } else {
      f.max <- f
      while (stats::pf(f.max, deg.f1, deg.f2, lower.tail = F) > 0.00001) f.max <- f.max + 1
    }
  } else {
    f.max <- xmax
  }
  # --------------------------------------
  # create data frame
  # --------------------------------------
  mydat <- data.frame(x = seq(0, f.max, length.out = 30 * f.max))
  # density distribution of f
  mydat$y <- stats::df(mydat$x, deg.f1, deg.f2)
  # base plot with f-distribution
  gp <- ggplot(mydat, aes(x = x, y = y)) + geom_line()
  sub.df <- NULL
  if (!is.null(p)) {
    # plot area for indicated f-value...
    sub.df <- mydat[mydat$x > stats::qf(p, deg.f1, deg.f2, lower.tail = F), ]
  } else if (!is.null(f)) {
    # resp. for p-value...
    sub.df <- mydat[mydat$x > f, ]
  }
  if (!is.null(sub.df)) {
    sub.df$p.level  <- ifelse(sub.df$x > stats::qf(0.05, deg.f1, deg.f2, lower.tail = F), "sig", "non-sig")
    fv <- stats::qf(0.05, deg.f1, deg.f2, lower.tail = F)
    gp <- gp +
      geom_ribbon(data = sub.df,
                  aes(ymax = y, fill = p.level),
                  ymin = 0,
                  alpha = geom.alpha) +
      annotate("text", 
               label = sprintf("F = %.2f", fv), 
               x = fv, 
               y = 0,
               vjust = 1.3)
    # add limit of p-value
    if (!is.null(f)) {
      pv <- stats::pf(f, deg.f1, deg.f2, lower.tail = F)
      if (pv >= 0.05) {
        gp <- gp +
          annotate("text", 
                   label = sprintf("p = %.2f", pv), 
                   x = f, 
                   y = 0,
                   hjust = -0.1,
                   vjust = -0.5,
                   angle = 90)
      }
    }
  }
  gp <- sj.setGeomColors(gp, geom.colors, pal.len = 2, labels = c("p > 5%", "p < 0.05"))
  gp <- gp + ylab(NULL) + xlab("F-value")
  print(gp)
}



#' @title Plot t-distributions
#' @name dist_t
#' 
#' @description This function plots a simple t-distribution or a t-distribution
#'                with shaded areas that indicate at which t-value a significant p-level 
#'                is reached.
#' 
#' @param t numeric, optional. If specified, a t-distribution with \code{deg.f} degrees
#'          of freedom is plotted and a shaded area at \code{t} value position is plotted that
#'          indicates whether or not the specified value is significant or not.
#'          If both \code{t} and \code{p} are not specified, a distribution without shaded
#'          area is plotted.
#' @param deg.f numeric. The degrees of freedom for the t-distribution. Needs to
#'          be specified.
#' @param p numeric, optional. If specified, a t-distribution with \code{deg.f} degrees
#'          of freedom is plotted and a shaded area at the position where the specified p-level
#'          starts is plotted. If both \code{t} and \code{p} are not specified, a distribution 
#'          without shaded area is plotted.
#' @param xmax numeric, optional. Specifies the maximum x-axis-value. If not specified, the x-axis
#'          ranges to a value where a p-level of 0.00001 is reached.
#' 
#' @inheritParams dist_norm
#' 
#' @examples
#' # a simple t-distribution
#' # for 6 degrees of freedom
#' dist_t(deg.f = 6)
#' 
#' # a t-distribution for 6 degrees of freedom,
#' # and a shaded area starting at t-value of one.
#' # With a df of 6, a t-value of 1.94 would be "significant".
#' dist_t(t = 1, deg.f = 6)
#' 
#' # a t-distribution for 6 degrees of freedom,
#' # and a shaded area starting at p-level of 0.4
#' # (t-value of about 0.26).
#' dist_t(p = 0.4, deg.f = 6)
#' 
#' @import ggplot2
#' @export
dist_t <- function(t = NULL,
                  deg.f = NULL,
                  p = NULL,
                  xmax = NULL,
                  geom.colors = NULL,
                  geom.alpha = 0.7) {
  # --------------------------------------
  # check parameters
  # --------------------------------------
  if (is.null(deg.f)) {
    warning("Degrees of freedom ('deg.f') needs to be specified.", call. = F)
    return(invisible(NULL))
  }
  # --------------------------------------
  # determine maximum range of x-axis. if we have
  # p-value but no t-value, distribution should range until
  # a theoretical p-value of 0.00001 is reached. this should
  # cover all possible (and visible) t-values
  # --------------------------------------
  if (is.null(xmax)) {
    if (is.null(t)) {
      t.max <- stats::qt(0.00001, deg.f, lower.tail = F)
    }
    # --------------------------------------
    # else, if we have a t-value, take into
    # account all possible t-values that would lead
    # to a theoretical p-value of 0.00001.
    # --------------------------------------
    else {
      t.max <- t
      while (stats::pt(t.max, deg.f, lower.tail = F) > 0.00001) {
        t.max <- t.max + 1
      }
    }
  }
  else {
    t.max <- xmax
  }
  # --------------------------------------
  # create data frame
  # --------------------------------------
  mydat <- data.frame(x = seq(-t.max, t.max, length.out = 20 * t.max))
  # density distribution of t
  mydat$y <- stats::dt(mydat$x, deg.f)
  # base plot with t-distribution
  gp <- ggplot(mydat, aes(x = x, y = y)) + geom_line()
  sub.df <- NULL
  if (!is.null(p)) {
    # plot area for indicated t-value...
    sub.df <- mydat[mydat$x > stats::qt(p, deg.f, lower.tail = F), ]
  }
  else if (!is.null(t)) {
    # resp. for p-value...
    sub.df <- mydat[mydat$x > t, ]
  }
  if (!is.null(sub.df)) {
    sub.df$p.level  <- ifelse(sub.df$x > stats::qt(0.05, deg.f, lower.tail = F), "sig", "non-sig")
    tv <- stats::qt(0.05, deg.f, lower.tail = F)
    gp <- gp +
      geom_ribbon(data = sub.df,
                  aes(ymax = y, fill = p.level),
                  ymin = 0,
                  alpha = geom.alpha) +
      annotate("text", 
               label = sprintf("t = %.2f", tv), 
               x = tv, 
               y = 0,
               vjust = 1.3)
    # add limit of p-value
    if (!is.null(t)) {
      pv <- stats::pt(t, deg.f, lower.tail = F)
      if (pv >= 0.05) {
        gp <- gp +
          annotate("text", 
                   label = sprintf("p = %.2f", pv), 
                   x = t, 
                   y = 0,
                   hjust = -0.1,
                   vjust = -0.5,
                   angle = 90)
      }
    }
  }
  gp <- sj.setGeomColors(gp, geom.colors, pal.len = 2, labels = c("p > 5%", "p < 0.05"))
  gp <- gp + ylab(NULL) + xlab("t-value")
  print(gp)
}
