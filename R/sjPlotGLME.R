#' @importFrom stats model.frame family na.omit
sjp.glmer.ri.slope <- function(fit, show.ci, facet.grid, ri.nr, vars, emph.grp,
                               geom.colors, ylim, prnt.plot, ...) {
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model; and
  # get model family, for link-inverse function
  # ----------------------------
  fit.df <- stats::model.frame(fit, fixed.only = TRUE)
  fitfam <- stats::family(fit)
  faminfo <- sjstats::model_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin
  # ---------------------------------------
  # get ...-argument
  # ---------------------------------------
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.prob <- list()
  mydf.prob <- list()
  fit.term.names <- colnames(fit.df)[-1]
  response.name <- colnames(fit.df)[1]
  fi <- unname(lme4::fixef(fit))[1]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      fit.term.names <- fit.term.names[!is.na(match(fit.term.names, vars))]
    } else {
      fit.term.names <- fit.term.names[vars]
    }
  }
  # ---------------------------------------------------------
  # axis title, depending on model family
  # ---------------------------------------------------------
  if (binom_fam)
    y.title <- paste("Predicted probabilities")
  else if (poisson_fam)
    y.title <- paste("Predicted incidents")
  # ---------------------------------------
  # iterate all random effects
  # ---------------------------------------
  for (ri.count in ri.nr) {
    # retrieve random effects
    rand.ef <- lme4::ranef(fit)[[ri.count]]
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    # check if we have bw-figure
    bw.figure <- !is.null(geom.colors) && geom.colors[1] == "bw"

    if (!bw.figure) {
      if (!is.null(emph.grp)) {
        # create color palette
        grp.col <- col_check2(geom.colors, length(emph.grp))
        # now set only colors for highlighted groups
        geom.colors <- rep("#999999", length(row.names(rand.ef)))
        geom.colors[emph.grp] <- grp.col
      } else if (!is.null(geom.colors)) {
        # get color platte
        geom.colors <- suppressWarnings(col_check2(geom.colors, length(row.names(rand.ef))))
        # check if we have enough colors - if not, set to NULL
        if (length(stats::na.omit(geom.colors)) < length(row.names(rand.ef))) geom.colors <- NULL
      }
    }
    # ----------------------------
    # loop through all coefficients
    # ----------------------------
    for (i in seq_len(length(fit.term.names))) {
      # init lists with all additional data frames and plots
      final.df <- data.frame()
      # get values from coefficient
      coef.column <- which(colnames(fit.df) == fit.term.names[i])
      # check if we have found the coefficient
      if (length(coef.column) > 0) {
        # get values from each coefficient
        vals <- fit.df[, coef.column]
        # sort values, for x axis
        vals.unique <- sort(vals)
        # melt variable
        mydf.vals <- data.frame(value = vals.unique)
        # convert factor to numeric
        if (is.factor(mydf.vals$value)) mydf.vals$value <- sjmisc::to_value(mydf.vals$value, start.at = 0, keep.labels = F)
        # retrieve names of coefficients
        coef.names <- names(lme4::fixef(fit))
        # check if we have a factor, then we may have reference levels
        if (is.factor(vals)) {
          # add reference level to coefficient name
          ll <- levels(vals)
          fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
        } else {
          fit.fac.name <- fit.term.names[i]
        }
        # find coef-position
        coef.pos <- which(coef.names == fit.fac.name)
        # check if we have found the coefficient
        if (length(coef.pos) > 0) {
          # calculate x-beta by multiplying original values with estimate of that term
          mydf.vals$xbeta <- mydf.vals$value * lme4::fixef(fit)[coef.pos]
          # save predictor name
          pred.name <- fit.term.names[i]
          # do this for each random intercept group
          for (j in seq_len(nrow(rand.ef))) {
            # calculate probability for each random effect group
            mydf.vals$y <- fitfam$linkinv(eta = (fi + rand.ef[j, 1] + mydf.vals$xbeta))
            # add to final data frame
            final.df <- rbind(final.df,
                              cbind(pred = mydf.vals$value,
                                    prob = mydf.vals$y,
                                    grp = j))
          }
          # convert grouping level to factor
          final.df$grp <- as.factor(final.df$grp)
          # retrieve group level label
          levels(final.df$grp) <- row.names(rand.ef)
          # ---------------------------------------------------------
          # prepare base plot
          # ---------------------------------------------------------
          if (bw.figure)
            mp <- ggplot(final.df, aes_string(x = "pred", y = "prob", colour = "grp", fill = "grp", linetype = "grp"))
          else
            mp <- ggplot(final.df, aes_string(x = "pred", y = "prob", colour = "grp", fill = "grp"))
          # special handling for negativ binomial
          if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
            mp <- mp +
              stat_smooth(method = "glm.nb", se = show.ci, alpha = dot.args[["ci.alpha"]])
          } else {
            mp <- mp +
              stat_smooth(method = "glm",
                          method.args = list(family = fitfam$family),
                          se = show.ci,
                          alpha = dot.args[["ci.alpha"]])
          }
          # continue with plot setup
          mp <- mp +
            labs(x = NULL, y = y.title,
                 title = sprintf("%s of %s on %s", y.title, pred.name, response.name))
          # ------------------------------
          # prepare default y-axis limits
          # ------------------------------
          y.limits <- c(as.integer(floor(10 * min(final.df$prob, na.rm = T) * .9)) / 10,
                        as.integer(ceiling(10 * max(final.df$prob, na.rm = T) * 1.1)) / 10)
          # ------------------------------
          # check axis limits, if we have user defined values
          # ------------------------------
          if (!is.null(ylim)) {
            # if we have one axis limits range for all plots, use this here
            if (!is.list(ylim) && length(ylim) == 2) {
              y.limits <- ylim
            } else if (is.list(ylim) && length(ylim) >= i) {
              # we may have multiple axis-limits-values here, one pair for
              # each plot. so check for correct length here
              y.limits <- ylim[[i]]
            }
          }
          # ---------------------------------------------------------
          # cartesian coord still plots range of se, even
          # when se exceeds plot range.
          # ---------------------------------------------------------
          mp <- mp + coord_cartesian(ylim = y.limits)
          # ---------------------------------------------------------
          # wrap to facets
          # ---------------------------------------------------------
          if (facet.grid) {
            mp <- mp + facet_wrap(~grp,
                                  ncol = round(sqrt(nrow(rand.ef))),
                                  scales = "free_x") +
              # no legend
              guides(colour = FALSE, fill = FALSE, linetype = FALSE)
          } else if (!is.null(geom.colors)) {
            # ------------------------------
            # highlight specific groups?
            # ------------------------------
            # set grouping levels as legend labels
            legendLabels <- row.names(rand.ef)
            # set new color scale
            mp <- sj.setGeomColors(mp,
                                   geom.colors,
                                   length(legendLabels),
                                   T,
                                   legendLabels)
          }
          # -------------------------------------
          # add to plot and df list
          # -------------------------------------
          plot.prob[[length(plot.prob) + 1]] <- mp
          mydf.prob[[length(mydf.prob) + 1]] <- final.df
          # -------------------------------------
          # check if metric plots should be plotted
          # -------------------------------------
          if (prnt.plot) suppressWarnings(graphics::plot(mp))
        }
      }
    }
  }
  invisible(structure(class = "sjpglmer.ripc",
                      list(data = mydf.prob,
                           plot = plot.prob)))
}


#' @importFrom stats formula
#' @importFrom tibble as_tibble rownames_to_column
sjp.lmer.ri.slope <- function(fit, ri.nr, vars, emph.grp, ylim, geom.size, prnt.plot, type) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # -----------------------------------------------------------
  # get model frame and model matrix
  # -----------------------------------------------------------
  m_m <- as.data.frame(stats::model.matrix(fit))
  m_f <- stats::model.frame(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.fe <- list()
  mydf.fe <- list()
  all.term.names <- colnames(m_f)
  response.name <- all.term.names[1]
  fit.term.names <- names(lme4::fixef(fit))[-1]
  estimates <- unname(lme4::fixef(fit))[-1]
  fi <- unname(lme4::fixef(fit))[1]
  # ---------------------------------------
  # iterate all random intercept
  # ---------------------------------------
  for (ri.count in ri.nr) {
    ri.tmp <- lme4::ranef(fit)
    # retrieve random effects
    rand.ef <- tibble::as_tibble(tibble::rownames_to_column(ri.tmp[[ri.count]]))
    ri.name <- names(ri.tmp[ri.count])
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    geom.colors <- NULL
    if (!is.null(emph.grp)) {
      # create color palette
      grp.col <- scales::brewer_pal(palette = "Set1")(length(emph.grp))
      # now set only colors for highlighted groups
      geom.colors <- rep("#999999", length(row.names(rand.ef)))
      geom.colors[emph.grp] <- grp.col
    }
    # ----------------------------
    # filter vars?
    # ----------------------------
    if (!is.null(vars)) {
      if (is.character(vars)) {
        removers <- !is.na(match(fit.term.names, vars))
      } else {
        removers <- vars
      }
      fit.term.names <- fit.term.names[removers]
      estimates <- estimates[removers]
    }
    # ----------------------------
    # loop through all coefficients
    # ----------------------------
    # slopes for all fixed effects
    # slopes for all fixed effects
    for (j in seq_len(length(estimates))) {
      # reset data frame
      final.df <- data.frame()
      # slopes for each random intercept
      for (i in seq_len(nrow(rand.ef))) {
        # retrieve intercept
        ri <- rand.ef[[2]][i]
        #
        if (type == "eff.ri") {
          # retrieve unique values of estimates
          est.values <- as.vector(stats::na.omit(unique(m_m[[fit.term.names[j]]])))
          # retrieve all other estimates, which should be set to mean
          # we need the switch-argument for interaction terms, to find the correct
          # data in the model matrix
          other.est <-
            m_m[, c(FALSE, !sjmisc::str_contains(fit.term.names[j],
                                                 colnames(m_m),
                                                 switch = !sjmisc::is_empty(grep(":", fit.term.names[j], fixed = T)))[-1])]
          # compute mean effects
          est.effect <- as.vector(estimates[-j] * colMeans(sjmisc::to_value(other.est), na.rm = T))
          final.df <- rbind(final.df,
                            cbind(x = sjmisc::to_value(est.values, keep.labels = F),
                                  y = fi + ri + sum(est.effect) + sjmisc::to_value(est.values, keep.labels = F) * estimates[j],
                                  grp = i))
        } else {
          xpos <- NULL
          # find original values for estimates
          for (k in 1:length(all.term.names)) {
            # check if estimate's name matches any column
            # in the data frame of the fitted model
            pos <- grep(all.term.names[k], fit.term.names[j], fixed = T)
            # found?
            if (length(pos) > 0) {
              xpos <- sort(unique(m_f[, k]))
              break
            }
          }
          # check if we found any values...
          if (!is.null(xpos)) {
            final.df <- rbind(final.df,
                              cbind(x = sjmisc::to_value(xpos, keep.labels = F),
                                    y = fi + ri + sjmisc::to_value(xpos, keep.labels = F) * estimates[j],
                                    grp = i))
          }
        }
      }
      # comvert grouping level to factor
      final.df$grp <- as.factor(final.df$grp)
      # retrieve group level label
      levels(final.df$grp) <- rand.ef[[1]]
      # ------------------------------
      # prepare plot title
      # ------------------------------
      if (type == "eff.ri")
        plot.title <- sprintf("Marginal effect of %s, conditioned on %s", fit.term.names[j], ri.name)
      else
        plot.title <- sprintf("Random effect \"%s\"", ri.name)
      # ------------------------------
      # prepare base plot
      # ------------------------------
      gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) +
        geom_line(size = geom.size) +
        labs(title = plot.title, x = fit.term.names[j], y = response.name)
      # ------------------------------
      # check axis limits, if we have user defined values
      # ------------------------------
      if (!is.null(ylim)) {
        # if we have one axis limits range for all plots, use this here
        if (!is.list(ylim) && length(ylim) == 2) {
          gp <- gp + ylim(ylim)
        } else if (is.list(ylim) && length(ylim) >= j) {
          # we may have multiple axis-limits-values here, one pair for
          # each plot. so check for correct length here
          gp <- gp + ylim(ylim[[j]])
        }
      }
      # ------------------------------
      # highlight specific groups?
      # ------------------------------
      if (!is.null(geom.colors)) {
        # set grouping levels as legend labels
        legendLabels <- rand.ef[[1]]
        # set new color scale
        gp <- sj.setGeomColors(gp,
                               geom.colors,
                               length(geom.colors),
                               T,
                               legendLabels)
      }
      # -------------------------------------
      # add to plot and df list
      # -------------------------------------
      plot.fe[[length(plot.fe) + 1]] <- gp
      mydf.fe[[length(mydf.fe) + 1]] <- final.df
      # -------------------------------------
      # check if metric plots should be plotted
      # -------------------------------------
      if (prnt.plot) graphics::plot(gp)
    }
  }
  invisible(structure(class = "sjplmer.feri",
                      list(data = mydf.fe,
                           plot = plot.fe)))
}


sjp.lme.rsri <- function(fit,
                         title,
                         axis.title,
                         ri.nr,
                         emph.grp,
                         geom.colors,
                         geom.size,
                         sample.n,
                         show.legend,
                         ylim,
                         prnt.plot,
                         fun) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # -----------------------------------------------------------
  # get model frame
  # -----------------------------------------------------------
  m_f <- stats::model.frame(fit)
  # get predictor names
  pred.values <- colnames(m_f)
  # ----------------------------
  # get model family, for link-inverse function
  # ----------------------------
  fitfam <- stats::family(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.fe <- list()
  mydf.fe <- list()
  # ---------------------------------------
  # global intercept and values from random slope predictor
  # ---------------------------------------
  global.intercept <- as.vector(lme4::fixef(fit))[1]
  # we need to check bounds
  remove_ri <- c()
  for (h in seq_len(length(ri.nr))) {
    # get each random part
    re_tmp <- lme4::ranef(fit)[[ri.nr[h]]]
    # has slopes?
    if (ncol(re_tmp) < 2)
      remove_ri <- c(remove_ri, h)
  }
  # found any random effects without slopes? if yes, remove them from index
  if (!sjmisc::is_empty(remove_ri)) {
    ri.nr <- ri.nr[-remove_ri]
  }
  # nothing found?
  if (sjmisc::is_empty(ri.nr)) {
    warning("No random effects with random-slope-intercept parameters found.", call. = F)
    return(NULL)
  }
  # ---------------------------------------
  # iterate all random intercept
  # ---------------------------------------
  for (ri.count in ri.nr) {
    # ------------------------------
    # find random slopes
    # ------------------------------
    rnd.part <- lme4::ranef(fit)[[ri.count]]
    rnd.slope.name <- colnames(rnd.part[2])
    # do predictor name and rnd. slope name equal?
    # if not, might be a factor, so no exact matching possible
    if (!any(pred.values == rnd.slope.name)) {
      # try to find predictor name in random slope name
      for (ef in pred.values) {
        pos <- grep(ef, rnd.slope.name, fixed = T)
        if (length(pos) > 0 && 1 == pos) {
          rnd.slope.name <- ef
          break
        }
      }
    }
    # get all values of predictor that was used as random slope
    eff.range <- unique(sort(m_f[[rnd.slope.name]], na.last = NA))
    # is it a factor?
    if (is.factor(eff.range)) eff.range <- sjmisc::to_value(eff.range)
    # ------------------------------
    # retrieve random effects
    # ------------------------------
    rand.ef <- tibble::rownames_to_column(rnd.part)
    # ------------------------------
    # sample random rows?
    # good to have when we have many random intercepts
    # ------------------------------
    if (!is.null(sample.n) && is.numeric(sample.n)) {
      if (length(sample.n) == 1)
        rand.ef <- dplyr::sample_n(rand.ef, sample.n)
      else
        rand.ef <- dplyr::slice(rand.ef, sample.n)
    }
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    if (!is.null(emph.grp)) {
      # create color palette
      grp.col <- col_check2(geom.colors, length(emph.grp))
      # now set only colors for highlighted groups
      geom.colors <- rep("#999999", length(row.names(rand.ef)))
      geom.colors[emph.grp] <- grp.col
    } else if (!is.null(geom.colors) && (geom.colors[1] == "gs" || nrow(rand.ef) < 10)) {
      geom.colors <- col_check2(geom.colors, nrow(rand.ef))
    } else {
      geom.colors <- NULL
    }
    # we may have multiple random slope values, e.g.
    # if random slope is a factor
    for (j in 3:ncol(rand.ef)) {
      # reset data frame
      final.df <- data.frame()
      # slopes for each random intercept
      for (i in seq_len(nrow(rand.ef))) {
        # retrieve intercept
        ri <- rand.ef[[2]][i]
        # retrieve random slope
        rs <- rand.ef[[j]][i]
        # compute x and y posistion, i.e. the coordinate for the regression line
        # of random slope / intercept
        final.df <- rbind(final.df,
                          cbind(x = eff.range,
                                y = global.intercept + ri + rs * eff.range,
                                grp = rand.ef[[1]][i]))
      }
      # convert grouping level to factor
      final.df$grp <- as.factor(final.df$grp)
      final.df$x <- sjmisc::to_value(final.df$x, keep.labels = F)
      final.df$y <- sjmisc::to_value(final.df$y, keep.labels = F)
      # logistic regression?
      if (fun == "glm") final.df$y <- fitfam$linkinv(eta = final.df$y)
      # ------------------------------
      # check axis limits
      # ------------------------------
      if (is.null(ylim)) {
        ylim <- c(as.integer(floor(10 * min(final.df$y, na.rm = T) * .9)) / 10,
                          as.integer(ceiling(10 * max(final.df$y, na.rm = T) * 1.1)) / 10)
      }
      # get random intercept name
      ri.name <- names(lme4::ranef(fit)[ri.count])
      # ------------------------------
      # plot title
      # ------------------------------
      if (is.null(title))
        p_title <- sprintf("Random slopes within \"%s\"", ri.name)
      else
        p_title <- title
      # ------------------------------
      # axis-x title
      # ------------------------------
      if (is.null(axis.title))
        p_axisTitle.x <- sjlabelled::get_label(m_f[[rnd.slope.name]], def.value = rnd.slope.name)
      else
        p_axisTitle.x <- axis.title
      # ------------------------------
      # prepare base response title
      # ------------------------------
      p_axisTitle.y <- sjlabelled::get_label(m_f[[1]], def.value = colnames(m_f)[1])
      # ------------------------------
      # prepare base plot
      # ------------------------------
      gp <- ggplot(final.df, aes_string(x = "x", y = "y", colour = "grp"))
      if (fun == "lm") {
        gp <- gp + geom_line(size = geom.size)
      } else {
        # special handling for negativ binomial
        if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
          gp <- gp +
            stat_smooth(method = "glm.nb", se = F)
        } else {
          gp <- gp +
            stat_smooth(method = "glm", se = F,
                        method.args = list(family = fitfam$family))
        }
      }
      gp <- gp +
        scale_y_continuous(limits = ylim) +
        labs(title = p_title, y = p_axisTitle.y, x = p_axisTitle.x)
      # ------------------------------
      # highlight specific groups?
      # ------------------------------
      if (!is.null(geom.colors)) {
        # set grouping levels as legend labels
        legendLabels <- rand.ef[, 1]
        # set new color scale
        gp <- sj.setGeomColors(gp,
                               geom.colors,
                               length(geom.colors),
                               show.legend,
                               legendLabels)
      } else if (!show.legend) {
        gp <- gp + guides(colour = FALSE)
      }
      # -------------------------------------
      # add to plot and df list
      # -------------------------------------
      plot.fe[[length(plot.fe) + 1]] <- gp
      mydf.fe[[length(mydf.fe) + 1]] <- final.df
      # -------------------------------------
      # check if metric plots should be plotted
      # -------------------------------------
      if (prnt.plot) graphics::plot(gp)
    }
  }
  invisible(structure(class = "sjplmer.reri",
                      list(data = mydf.fe,
                           plot = plot.fe)))
}
