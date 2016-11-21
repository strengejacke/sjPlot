# bind global variables
utils::globalVariables(c("predicted", "residuals"))

#' @title Plot predicted values and their residuals
#' @name sjp.resid
#'
#' @description This function plots observed and predicted values of the response
#'              of linear (mixed) models for each coefficient and highlights the
#'              observed values according to their distance (residuals) to the 
#'              predicted values. This allows to investigate how well actual and 
#'              predicted values of the outcome fit across the predictor variables.
#'
#' @param fit fitted linear (mixed) regression model (including objects of class
#'        \code{\link[nlme]{gls}} or \code{plm}).
#' @param show.lines logical, if \code{TRUE}, a line connecting predicted and
#'        residual values is plotted. Set this argument to \code{FALSE}, if
#'        plot-building is too time consuming.
#' @param show.resid logical, if \code{TRUE}, residual values are plotted.
#' @param show.pred logical, if \code{TRUE}, predicted values are plotted.
#' @inheritParams sjp.lm
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}),
#'           the residual pattern (\code{pattern}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#'
#' @note The actual (observed) values have a coloured fill, while the predicted
#'       values have a solid outline without filling.
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
#' # show pattern
#' sjp.resid(fit, remove.estimates = c("e17age", "e42dep"))$pattern
#'
#' @export
sjp.resid <- function(fit, geom.size = 2, remove.estimates = NULL, show.lines = TRUE, 
                      show.resid = TRUE, show.pred = TRUE, show.ci = F, prnt.plot = TRUE) {
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
  var.labels <- sjmisc::get_label(dummy, def.value = colnames(dummy)[sel])[sel]
  if (is.null(var.labels) || all(var.labels == "")) var.labels <- colnames(dummy)[sel]
  colnames(dummy)[sel] <- var.labels
  
  # melt data
  mydat <- suppressWarnings(dummy %>% 
    tidyr::gather(key = "grp", value = "x", -1, -predicted, -residuals))
  
  # melt data, build basic plot
  res.plot <- ggplot(mydat, aes_string(x = "x", y = rv)) +
    stat_smooth(method = "lm", se = show.ci, colour = "grey70")
  
  if (show.lines) res.plot <- res.plot + 
    geom_segment(aes_string(xend = "x", yend = "predicted"), alpha = .3)
  
  if (show.resid) res.plot <- res.plot + 
    geom_point(aes_string(fill = "residuals"), size = geom.size, shape = 21, colour = "grey50")
  
  if (show.pred) res.plot <- res.plot + 
    geom_point(aes_string(y = "predicted"), shape = 1, size = geom.size)
  
  # residual plot
  res.plot <- res.plot +
    facet_grid(~grp, scales = "free") +
    scale_fill_gradient2(low = "#003399", mid = "white", high = "#993300") +
    guides(color = FALSE, fill = FALSE) +
    labs(x = NULL, y = sjmisc::get_label(mydat[[1]], def.value = rv))
  
  # residual plot, showing pattern
  pattern.plot <- ggplot(mydat, aes_string(x = "x", y = "residuals")) + 
    geom_hline(yintercept = 0, colour = "white", size = 2) + 
    geom_line() +
    facet_grid(~grp, scales = "free") +
    labs(x = NULL, y = sjmisc::get_label(mydat[[1]], def.value = rv))
  
  # Check whether ggplot object should be returned or plotted
  if (prnt.plot) graphics::plot(res.plot)
  
  # return results
  invisible(structure(class = "sjpresid",
                      list(plot = res.plot,
                           pattern = pattern.plot,
                           mydf = mydat)))
}
