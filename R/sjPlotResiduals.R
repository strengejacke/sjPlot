# bind global variables
utils::globalVariables(c("predicted", "residuals"))

#' @title Plot predicted values and their residuals
#' @name sjp.resid
#'
#' @description to follow...
#'
#' @param fit fitted linear regression model (of class \code{\link{lm}}, 
#'        \code{\link[nlme]{gls}} or \code{plm}).
#' @param show.lines logical, if \code{TRUE}, a line connecting predicted and
#'        residual values is plotted. Set this argument to \code{FALSE}, if
#'        plot-building is too time consuming.
#' @param show.resid logical, if \code{TRUE}, residual values are plotted.
#' @param show.pred logical, if \code{TRUE}, predicted values are plotted.
#' @inheritParams sjp.lm
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) 
#'           as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#' # fit model
#' fit <- lm(neg_c_7 ~ c12hour + e17age + e42dep, data = efc)
#' 
#' # plot residuals for all independent variables
#' sjp.resid(fit)
#' 
#' # remove some independent variables from output
#' sjp.resid(fit, remove.estimates = c("e17age", "e42dep"))
#'
#' @export
sjp.resid <- function(fit, remove.estimates = NULL, show.lines = TRUE, 
                      show.resid = TRUE, show.pred = TRUE, prnt.plot = TRUE) {
  # show lines only when both residual and predicted
  # values are plotted - else, lines make no sense
  if (!show.pred || !show.resid) show.lines <- FALSE
  
  # Obtain predicted and residual values
  mydat <- stats::model.frame(fit)
  
  # check whether estimates should be removed from plot
  if (!is.null(remove.estimates)) {
    keep <- which(!colnames(mydat) %in% remove.estimates)
  } else {
    keep <- seq_len(ncol(mydat))
  }
  mydat$predicted <- stats::predict(fit)
  mydat$residuals <- stats::residuals(fit)
  
  # get name of response, used in ggplot-aes
  rv <- sjstats::resp_var(fit)
  
  # remove estimates, if required
  dummy <- mydat %>% dplyr::select(keep, predicted, residuals)
  
  # set default variable labels, used as column names, so labelled
  # data variable labels appear in facet grid header.
  sel <- 2:length(keep)
  colnames(dummy)[sel] <- sjmisc::get_label(dummy, def.value = colnames(dummy)[sel])[sel]
  
  # melt data, build basic plot
  suppressWarnings(res.plot <- dummy %>% 
    tidyr::gather(key = "grp", value = "x", -1, -predicted, -residuals) %>% 
    ggplot(aes_string(x = "x", y = rv)) +
    stat_smooth(method = "lm", se = F, colour = "grey70"))
  
  if (show.lines) res.plot <- res.plot + 
    geom_segment(aes(xend = x, yend = predicted), alpha = .3)
  
  if (show.resid) res.plot <- res.plot + 
    geom_point(aes(color = residuals))
  
  if (show.pred) res.plot <- res.plot + 
    geom_point(aes(y = predicted), shape = 1)
  
  res.plot <- res.plot +
    facet_grid(~grp, scales = "free") +
    scale_color_gradient2(low = "#003399", mid = "white", high = "#993300") +
    guides(color = FALSE) +
    labs(x = NULL, y = sjmisc::get_label(mydat[[1]], def.value = rv))
  
  # Check whether ggplot object should be returned or plotted
  if (prnt.plot) graphics::plot(res.plot)
  
  # return results
  invisible(structure(class = "sjpresid",
                      list(plot = res.plot,
                           mydf = mydat)))
}
