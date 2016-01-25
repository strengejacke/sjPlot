# bind global variables
utils::globalVariables(c("xn", "vld", "l.ci", "u.ci"))

#' @importFrom dplyr filter
sjp.emm <- function(fit,
                    swapPredictors = FALSE,
                    plevel = 0.05,
                    title = NULL,
                    geom.colors = "Set1",
                    geom.size = 0.7,
                    axisTitle.x = NULL,
                    axisTitle.y = NULL,
                    axisLabels.x = NULL,
                    legendTitle = NULL,
                    legendLabels = NULL,
                    showValueLabels = FALSE,
                    valueLabel.digits = 2,
                    showCI = FALSE,
                    breakTitleAt = 50,
                    breakLegendTitleAt = 20,
                    breakLegendLabelsAt = 20,
                    axisLimits.y = NULL,
                    gridBreaksAt = NULL,
                    facet.grid = FALSE,
                    printPlot = TRUE) {
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) geom.size = .7
  # ------------------------
  # check if suggested packages are available
  # ------------------------
  if (!requireNamespace("lsmeans", quietly = TRUE)) {
    stop("Package 'lsmeans' needed for this function to work. Please install it.", call. = FALSE)
  }
  if ((any(class(fit) == "lmerMod" || any(class(fit) == "merModLmerTest"))) && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -----------------------------------------------------------
  # go to sub-function if class = lmerMod
  # -----------------------------------------------------------
  if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    return(sjp.emm.lmer(fit, swapPredictors, plevel, title, geom.colors, geom.size,
                        axisTitle.x, axisTitle.y, axisLabels.x, legendLabels,
                        showValueLabels, valueLabel.digits, showCI, breakTitleAt,
                        breakLegendLabelsAt, axisLimits.y, gridBreaksAt, 
                        facet.grid, printPlot))
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) gridbreaks.y <- ggplot2::waiver()
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) legendLabels <- unlistlabels(legendLabels)
  if (!is.null(legendTitle) && is.list(legendTitle)) legendTitle <- unlist(legendTitle)
  # -----------------------------------------------------------
  # retrieve p-values, without intercept
  # -----------------------------------------------------------
  pval <- summary(fit)$coefficients[-1, 4]
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # save all term labels
  it <- attr(fit$terms, "term.labels")
  # save coefficients
  cf <- names(fit$coefficients[-1])
  # init counter
  it.nr <- 0
  it.pos <- c()
  it.names <- c()
  # check whether current term name contains a ":",
  # thus if it is an interaction term
  pos <- grep(":", it)
  # if yes...
  if (length(pos) > 0) it.names <- it[pos]
  # check whether current coefficient contains a ":",
  # thus if it is an interaction term
  pos <- grep(":", cf)
  # if yes...
  if (length(pos) > 0) {
    # ... set count of interactions
    it.nr <- length(pos)
    # ... and save position of coefficient in model
    it.pos <- pos
  }
  # check whether we have any interaction terms included at all
  if (it.nr == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return(invisible(NULL))
  }
  # save names of interaction predictor variables into this object
  # but only those with a specific p-level
  intnames <- c()
  for (i in 1:length(it.pos)) {
    if (is.na(pval[it.pos[i]])) pval[it.pos[i]] <- 1
    if (pval[it.pos[i]] < plevel) {
      intnames <- c(intnames, cf[it.pos[i]])
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0 == length(intnames)) {
    warning("No significant interactions found...", call. = FALSE)
    return(invisible(NULL))
  }
  # -----------------------------------------------------------
  # Now iterate all interaction terms from model
  # -----------------------------------------------------------
  interactionterms <- c()
  for (i in 1:length(it.names)) {
    # -----------------------------------------------------------
    # retrieve interaction terms
    # -----------------------------------------------------------
    terms <- unlist(strsplit(it.names[i], ":"))
    # -----------------------------------------------------------
    # check if both interaction terms are factors
    # -----------------------------------------------------------
    if (is.factor(fit$model[[terms[1]]]) && is.factor(fit$model[[terms[2]]])) {
      # -----------------------------------------------------------
      # Iterate all interactions on factor-level-basis from model
      # -----------------------------------------------------------
      for (cnt in 1:length(intnames)) {
        # -----------------------------------------------------------
        # first, retrieve and split interaction term so we know
        # the two predictor variables, or factor levels of the
        # interaction term
        # -----------------------------------------------------------
        lvls <- unlist(strsplit(intnames[cnt], ":"))
        # -----------------------------------------------------------
        # since we may have factors with more levels, the original
        # term labels differ from what we have as coefficient-
        # e.g., "ChickWeight$Diet", becomes "Diet1", "Diet2", etc.
        # to calculate marginal means, we only need "Diet". So here
        # we have to find, which terms match the significant coefficients
        # found, and use the term labels for ls means...
        # -----------------------------------------------------------
        if (grepl(terms[1], lvls[1], fixed = T) && grepl(terms[2], lvls[2], fixed = T)) {
          # we found a match
          interactionterms <- rbind(interactionterms, terms)
          # leave loop
          break
        }
      }
    } else {
      warning(sprintf("Both %s and %s need to be factors! Skipping these interaction terms...", terms[1], terms[2]), call. = F)
    }
  }
  # -----------------------------------------------------------
  # check if we have any valid interaction terms
  # for lsmeans function
  # -----------------------------------------------------------
  if (nrow(interactionterms) > 0) {
    for (cnt in 1:nrow(interactionterms)) {
      # -----------------------------------------------------------
      # retrieve each pair of interaction terms
      # -----------------------------------------------------------
      term.pairs <- interactionterms[cnt, ]
      if (swapPredictors) term.pairs <- rev(term.pairs)
      # -----------------------------------------------------------
      # retrieve estiamted marginal means
      # -----------------------------------------------------------
      emm <- summary(lsmeans::lsmeans.character(fit, term.pairs))
      # create data frame from lsmeans
      intdf <- data.frame(emm[2],
                          emm[3],
                          emm[1],
                          emm[6],
                          emm[7],
                          rep(valueLabel.digits, times = nrow(emm[1])))
      colnames(intdf) <- c("x", "y", "grp", "l.ci", "u.ci", "vld")
      # -----------------------------------------------------------
      # remove missings
      # -----------------------------------------------------------
      if (anyNA(intdf$y)) {
        # warn user
        warning("fitted model had estimates with missing values. Output may be incomplete.", call. = F)
        # remove missings
        intdf <- dplyr::filter(intdf, !is.na(y))
      }
      # -----------------------------------------------------------
      # convert df-values to numeric
      # -----------------------------------------------------------
      intdf$y <- as.numeric(as.character(intdf$y))
      # add numeric x for geom_line
      intdf$xn <- as.numeric(intdf$x)
      # ci to numeric, y-scale is continuous
      intdf$l.ci <- as.numeric(intdf$l.ci)
      intdf$u.ci <- as.numeric(intdf$u.ci)
      # order data frame
      intdf <- intdf[order(intdf$grp), ]
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        lowerLim.y <- ifelse(showCI == TRUE, floor(min(intdf$l.ci)), floor(min(intdf$y)))
        upperLim.y <- ifelse(showCI == TRUE, ceiling(max(intdf$u.ci)), ceiling(max(intdf$y)))
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
      # -----------------------------------------------------------
      # check whether user defined grid breaks / tick marks are used
      # -----------------------------------------------------------
      if (!is.null(gridBreaksAt)) {
        gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by = gridBreaksAt))
      }
      # -----------------------------------------------------------
      # prepare label and name from dependend variable
      # -----------------------------------------------------------
      # get response name, which is variable name
      response.name <- colnames(fit$model)[1]
      # get variable label attribute
      response.label <- sjmisc::get_label(fit$model[[1]], 
                                          def.value = response.name)
      # check if we have any
      if (is.null(response.label)) response.label <- response.name
      # -----------------------------------------------------------
      # prepare label for x-axix
      # -----------------------------------------------------------
      alx <- sjmisc::get_labels(fit$model[[term.pairs[2]]], 
                                attr.only = F, 
                                include.values = NULL, 
                                include.non.labelled = T)
      # check if we have any
      if (is.null(alx)) alx <- term.pairs[2]
      # -----------------------------------------------------------
      # prepare plot title and axis titles
      # -----------------------------------------------------------
      if (is.null(title)) {
        labtitle <- paste0("Estimated marginal means of ", response.name,
                           " between ", term.pairs[2],
                           " and ", term.pairs[1])
      } else {
        labtitle <- title
      }
      # -----------------------------------------------------------
      # legend labels
      # -----------------------------------------------------------
      if (is.null(legendLabels)) {
        lLabels <- levels(fit$model[term.pairs[1]][, 1])
      } else {
        lLabels <- legendLabels
      }
      # -----------------------------------------------------------
      # legend title
      # -----------------------------------------------------------
      if (is.null(legendTitle)) {
        lTitle <- term.pairs[1]
      } else {
        # set legend title for plot
        lTitle <- legendTitle
      }
      if (is.null(axisLabels.x)) axisLabels.x <- alx
      if (!is.null(axisTitle.x)) {
        labx <- axisTitle.x
      } else {
        labx <- term.pairs[2]
      }
      if (!is.null(axisTitle.y)) {
        laby <- axisTitle.y
      } else {
        laby <- response.label
      }
      # -----------------------------------------------------------
      # prepare annotation labels
      # -----------------------------------------------------------
      # wrap title
      labtitle <- sjmisc::word_wrap(labtitle, breakTitleAt)
      # wrap legend labels
      lLabels <- sjmisc::word_wrap(lLabels, breakLegendLabelsAt)
      # wrap legend title
      lTitle <- sjmisc::word_wrap(lTitle, breakLegendTitleAt)
      # -----------------------------------------------------------
      # prepare base plot of interactions
      # -----------------------------------------------------------
      baseplot <- ggplot(intdf)
      # -----------------------------------------------------------
      # Confidence intervals?
      # -----------------------------------------------------------
      if (showCI) baseplot <- baseplot +
          geom_ribbon(aes(x = xn, ymin = l.ci, ymax = u.ci, fill = grp), alpha = .3)
      # -----------------------------------------------------------
      # continue with plot. point and line layers above ribbon
      # -----------------------------------------------------------
      baseplot <- baseplot +
        geom_point(aes(x = x, y = y, colour = grp)) +
        geom_line(aes(x = xn, y = y, colour = grp), size = geom.size) +
        scale_x_discrete(labels = axisLabels.x)
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_text(aes(label = round(y, vld), x = x, y = y),
                    vjust = 1.5,
                    show.legend = FALSE)
      }
      # ------------------------------------------------------------------------------------
      # build plot object with theme and labels
      # ------------------------------------------------------------------------------------
      baseplot <- baseplot +
        # set plot and axis titles
        labs(title = labtitle,
             x = labx,
             y = laby,
             colour = lTitle) +
        # set axis scale breaks
        scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
      # ---------------------------------------------------------
      # facet grid?
      # ---------------------------------------------------------
      if (facet.grid) baseplot <- baseplot + facet_grid( ~grp)    
      # ---------------------------------------------------------
      # set geom colors
      # ---------------------------------------------------------
      baseplot <- sj.setGeomColors(baseplot, geom.colors, length(lLabels), TRUE, lLabels) + guides(fill = FALSE)
      # ---------------------------------------------------------
      # Check whether ggplot object should be returned or plotted
      # ---------------------------------------------------------
      if (printPlot) print(baseplot)
      # concatenate plot object
      plotlist[[length(plotlist) + 1]] <- baseplot
      dflist[[length(dflist) + 1]] <- intdf
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpemmint",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}


sjp.emm.lmer <- function(fit, swapPredictors, plevel, title, geom.colors, geom.size, axisTitle.x,
                         axisTitle.y, axisLabels.x, legendLabels, showValueLabels,
                         valueLabel.digits, showCI, breakTitleAt, breakLegendLabelsAt,
                         axisLimits.y, gridBreaksAt, facet.grid, printPlot) {
  if ((any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' needed for this function to work. Please install it.", call. = FALSE)
  }
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) geom.size = .7
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) gridbreaks.y <- ggplot2::waiver()
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) legendLabels <- unlistlabels(legendLabels)
  # -----------------------------------------------------------
  # get terms of fitted model
  # -----------------------------------------------------------
  preds <- attr(terms(fit), "term.labels")
  # interaction terms contain colons
  it.names <- c()
  # any predictors with colon?
  pos <- grep(":", preds)
  # if yes, we have our interaction terms
  if (length(pos) > 0) {
    it.names <- preds[pos]
  } else {
    warning("No interaction term found in fitted model...", call. = F)
    return(invisible(NULL))
  }
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # get model summary
  fit.coef <- summary(fit)$coefficients
  # save coefficients
  cf <- rownames(fit.coef)
  # find first interaction terms
  pos <- grep(":", cf)
  # get all p-values
  pval <- get_lmerMod_pvalues(fit)[pos]
  # get significant interactions
  intnames <- cf[pos[which(pval < plevel)]]
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0 == length(intnames)) {
    warning("No significant interactions found...", call. = FALSE)
    return(invisible(NULL))
  }
  # -----------------------------------------------------------
  # Now iterate all interaction terms from model
  # -----------------------------------------------------------
  interactionterms <- c()
  for (i in 1:length(it.names)) {
    # -----------------------------------------------------------
    # retrieve interaction terms
    # -----------------------------------------------------------
    terms <- unlist(strsplit(it.names[i], ":"))
    # -----------------------------------------------------------
    # check if both interaction terms are factors
    # -----------------------------------------------------------
    if (is.factor(fit@frame[[terms[1]]]) && is.factor(fit@frame[[terms[2]]])) {
      # -----------------------------------------------------------
      # Iterate all interactions on factor-level-basis from model
      # -----------------------------------------------------------
      for (cnt in 1:length(intnames)) {
        # -----------------------------------------------------------
        # first, retrieve and split interaction term so we know
        # the two predictor variables, or factor levels of the
        # interaction term
        # -----------------------------------------------------------
        lvls <- unlist(strsplit(intnames[cnt], ":"))
        # -----------------------------------------------------------
        # since we may have factors with more levels, the original
        # term labels differ from what we have as coefficient-
        # e.g., "ChickWeight$Diet", becomes "Diet1", "Diet2", etc.
        # to calculate marginal means, we only need "Diet". So here
        # we have to find, which terms match the significant coefficients
        # found, and use the term labels for ls means...
        # -----------------------------------------------------------
        if (grepl(terms[1], lvls[1], fixed = T) && grepl(terms[2], lvls[2], fixed = T)) {
          # we found a match
          interactionterms <- rbind(interactionterms, terms)
          # leave loop
          break
        }
      }
    } else {
      warning(sprintf("Both %s and %s need to be factors! Skipping these interaction terms...", terms[1], terms[2]), call. = F)
    }
  }
  # -----------------------------------------------------------
  # check if we have any valid interaction terms
  # for lsmeans function
  # -----------------------------------------------------------
  if (nrow(interactionterms) > 0) {
    for (cnt in 1:nrow(interactionterms)) {
      # -----------------------------------------------------------
      # retrieve each pair of interaction terms
      # -----------------------------------------------------------
      term.pairs <- interactionterms[cnt, ]
      # -----------------------------------------------------------
      # retrieve estimated marginal means for all predictors of
      # the model, with various statistics in a data frame format
      # -----------------------------------------------------------
      emm.df <- lmerTest::lsmeans(fit, paste(term.pairs, collapse = ":"))[[1]]
      # swap predictors?
      if (swapPredictors) term.pairs <- rev(term.pairs)
      # -----------------------------------------------------------
      # get column indices of interaction terms, estimate and
      # confidence intervals. latter term in interaction is considered
      # as "within subject" (x-pos), first term is considered as
      # "between subjects" (group)
      # -----------------------------------------------------------
      emm.col <- c(which(colnames(emm.df) == term.pairs[2]),
                   which(colnames(emm.df) == "Estimate"),
                   which(colnames(emm.df) == term.pairs[1]),
                   which(colnames(emm.df) == "Lower CI"),
                   which(colnames(emm.df) == "Upper CI"))
      # -----------------------------------------------------------
      # create data frame from lsmeans
      # -----------------------------------------------------------
      intdf <- data.frame(emm.df[, emm.col],
                          rep(valueLabel.digits, times = nrow(emm.df)))
      colnames(intdf) <- c("x", "y", "grp", "l.ci", "u.ci", "vld")
      # -----------------------------------------------------------
      # convert df-values to numeric
      # -----------------------------------------------------------
      intdf$y <- as.numeric(as.character(intdf$y))
      # add numeric x for geom_line
      intdf$xn <- as.numeric(intdf$x)
      # ci to numeric, y-scale is continuous
      intdf$l.ci <- as.numeric(intdf$l.ci)
      intdf$u.ci <- as.numeric(intdf$u.ci)
      # order data frame
      intdf <- intdf[order(intdf$grp), ]
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        lowerLim.y <- ifelse(showCI == TRUE, floor(min(intdf$l.ci)), floor(min(intdf$y)))
        upperLim.y <- ifelse(showCI == TRUE, ceiling(max(intdf$u.ci)), ceiling(max(intdf$y)))
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
      # -----------------------------------------------------------
      # check whether user defined grid breaks / tick marks are used
      # -----------------------------------------------------------
      if (!is.null(gridBreaksAt)) {
        gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by = gridBreaksAt))
      }
      # -----------------------------------------------------------
      # prepare label and name from depend variable
      # -----------------------------------------------------------
      # get response name, which is variable name
      response.name <- colnames(fit@frame)[1]
      # get variable label attribute
      response.label <- sjmisc::get_label(fit@frame[[1]], 
                                          def.value = response.name)
      # check if we have any
      if (is.null(response.label)) response.label <- response.name
      # -----------------------------------------------------------
      # prepare label for x-axix
      # -----------------------------------------------------------
      # get value label attribute
      alx <- sjmisc::get_labels(fit@frame[[term.pairs[2]]], 
                                attr.only = F, 
                                include.values = NULL, 
                                include.non.labelled = T)
      # check if we have any
      if (is.null(alx)) alx <- term.pairs[2]
      # -----------------------------------------------------------
      # prepare plot title and axis titles
      # -----------------------------------------------------------
      if (is.null(title)) {
        labtitle <- paste0("Estimated marginal means of ", response.name,
                           " between ", term.pairs[2],
                           " and ", term.pairs[1])
      } else {
        labtitle <- title
      }
      if (is.null(legendLabels)) {
        lLabels <- levels(fit@frame[[term.pairs[1]]])
      } else {
        lLabels <- legendLabels
      }
      if (is.null(axisLabels.x)) axisLabels.x <- alx
      if (!is.null(axisTitle.x)) {
        labx <- axisTitle.x
      } else {
        labx <- term.pairs[2]
      }
      if (!is.null(axisTitle.y)) {
        laby <- axisTitle.y
      } else {
        laby <- response.label
      }
      # -----------------------------------------------------------
      # prepare annotation labels
      # -----------------------------------------------------------
      # wrap title(s)
      labtitle <- sjmisc::word_wrap(labtitle, breakTitleAt)
      labx <- sjmisc::word_wrap(labx, breakTitleAt)
      laby <- sjmisc::word_wrap(laby, breakTitleAt)
      # wrap legend labels
      lLabels <- sjmisc::word_wrap(lLabels, breakLegendLabelsAt)
      # -----------------------------------------------------------
      # prepare base plot of interactions
      # -----------------------------------------------------------
      baseplot <- ggplot(intdf)
      # -----------------------------------------------------------
      # Confidence intervals?
      # -----------------------------------------------------------
      if (showCI) baseplot <- baseplot +
        geom_ribbon(aes(x = xn, ymin = l.ci, ymax = u.ci, fill = grp), alpha = .3)
      # -----------------------------------------------------------
      # continue with plot. point and line layers above ribbon
      # -----------------------------------------------------------
      baseplot <- baseplot +
        geom_point(aes(x = x, y = y, colour = grp)) +
        geom_line(aes(x = xn, y = y, colour = grp), size = geom.size) +
        scale_x_discrete(labels = axisLabels.x)
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_text(aes(label = round(y, vld), x = x, y = y),
                    vjust = 1.5,
                    show.legend = FALSE)
      }
      # ------------------------------------------------------------------------------------
      # build plot object with theme and labels
      # ------------------------------------------------------------------------------------
      baseplot <- baseplot +
        # set plot and axis titles
        labs(title = labtitle,
             x = labx,
             y = laby,
             colour = term.pairs[1]) +
        # set axis scale breaks
        scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
      # ---------------------------------------------------------
      # facet grid?
      # ---------------------------------------------------------
      if (facet.grid) baseplot <- baseplot + facet_grid( ~grp)    
      # ---------------------------------------------------------
      # set geom colors
      # ---------------------------------------------------------
      baseplot <- sj.setGeomColors(baseplot, geom.colors, length(lLabels), TRUE, lLabels) + guides(fill = FALSE)
      # ---------------------------------------------------------
      # Check whether ggplot object should be returned or plotted
      # ---------------------------------------------------------
      if (printPlot) print(baseplot)
      # concatenate plot object
      plotlist[[length(plotlist) + 1]] <- baseplot
      dflist[[length(dflist) + 1]] <- intdf
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpemmint",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}