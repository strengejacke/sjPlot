# bind global variables
utils::globalVariables(c("xn", "vld", "conf.low", "conf.high"))

#' @importFrom dplyr filter
#' @importFrom sjstats get_model_pval
#' @importFrom stats terms
sjp.emm <- function(fit,
                    swap.pred = FALSE,
                    plevel = 0.05,
                    title = NULL,
                    geom.colors = "Set1",
                    geom.size = 0.7,
                    axis.title = NULL,
                    axis.labels = NULL,
                    legend.title = NULL,
                    legend.labels = NULL,
                    show.values = FALSE,
                    digits = 2,
                    show.ci = FALSE,
                    p.kr = TRUE,
                    breakTitleAt = 50,
                    wrap.legend.title = 20,
                    wrap.legend.labels = 20,
                    y.offset = 0.07,
                    ylim = NULL,
                    grid.breaks = NULL,
                    facet.grid = FALSE,
                    prnt.plot = TRUE,
                    ...) {
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) geom.size = .7
  # ---------------------------------------
  # get ...-arguments
  # ---------------------------------------
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)
  # ------------------------
  # check if suggested packages are available
  # ------------------------
  if (!requireNamespace("lsmeans", quietly = TRUE)) {
    stop("Package `lsmeans` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (inherits(fit, c("lmerMod", "merModLmerTest")) && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package `lmerTest` needed for this function to work. Please install it.", call. = FALSE)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legend.labels) && is.list(legend.labels)) legend.labels <- unlistlabels(legend.labels)
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(grid.breaks)) gridbreaks.y <- ggplot2::waiver()
  # -----------------------------------------------------------
  # is mermod?
  # -----------------------------------------------------------
  is_mer_mod <- is_merMod(fit)
  # -----------------------------------------------------------
  # find interaction terms
  # -----------------------------------------------------------
  # init variable
  it.names <- c()
  # get model term names
  preds <- attr(stats::terms(fit), "term.labels")
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
  # -----------------------------------------------------------
  # get terms of fitted model
  # -----------------------------------------------------------
  # get all p-values
  pval <- sjstats::get_model_pval(fit, p.kr)[["p.value"]][pos]
  # get significant interactions
  intnames <- cf[pos[which(pval < plevel)]]
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0 == length(intnames)) {
    warning("No significant interactions found...", call. = FALSE)
    return(invisible(NULL))
  }
  # -----------------------------------------------------------
  # get model frame
  # -----------------------------------------------------------
  m_f <- stats::model.frame(fit)
  # -----------------------------------------------------------
  # Now iterate all interaction terms from model
  # -----------------------------------------------------------
  interactionterms <- c()
  for (i in seq_len(length(it.names))) {
    # -----------------------------------------------------------
    # retrieve interaction terms
    # -----------------------------------------------------------
    terms <- unlist(strsplit(it.names[i], ":"))
    # -----------------------------------------------------------
    # check if both interaction terms are factors
    # -----------------------------------------------------------
    if (is.factor(m_f[[terms[1]]]) && is.factor(m_f[[terms[2]]])) {
      # -----------------------------------------------------------
      # Iterate all interactions on factor-level-basis from model
      # -----------------------------------------------------------
      for (cnt in seq_len(length(intnames))) {
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
  is.em <- sjmisc::is_empty(interactionterms)
  if (!is.em && nrow(interactionterms) > 0) {
    for (cnt in seq_len(nrow(interactionterms))) {
      # -----------------------------------------------------------
      # retrieve each pair of interaction terms
      # -----------------------------------------------------------
      term.pairs <- interactionterms[cnt, ]
      # go on here for mermod objects
      if (is_mer_mod) {
        # -----------------------------------------------------------
        # retrieve estimated marginal means for all predictors of
        # the model, with various statistics in a data frame format
        # -----------------------------------------------------------
        emm.df <- lmerTest::lsmeans(fit, paste(term.pairs, collapse = ":"))[[1]]
        # swap predictors?
        if (swap.pred) term.pairs <- rev(term.pairs)
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
                            rep(digits, times = nrow(emm.df)))
      } else {
        if (swap.pred) term.pairs <- rev(term.pairs)
        # -----------------------------------------------------------
        # retrieve estiamted marginal means
        # -----------------------------------------------------------
        emm <- summary(lsmeans::lsmeans.character(fit, term.pairs))
        # create data frame from lsmeans
        intdf <- data.frame(emm[2], emm[3], emm[1], emm[6], emm[7],
                            rep(digits, times = nrow(emm[1])))
      }
      colnames(intdf) <- c("x", "y", "grp", "conf.low", "conf.high", "vld")
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
      intdf$conf.low <- as.numeric(intdf$conf.low)
      intdf$conf.high <- as.numeric(intdf$conf.high)
      # order data frame
      intdf <- intdf[order(intdf$grp), ]
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(ylim)) {
        lowerLim.y <- ifelse(show.ci, floor(min(intdf$conf.low)), floor(min(intdf$y)))
        upperLim.y <- ifelse(show.ci, ceiling(max(intdf$conf.high)), ceiling(max(intdf$y)))
      } else {
        lowerLim.y <- ylim[1]
        upperLim.y <- ylim[2]
      }
      # -----------------------------------------------------------
      # check whether user defined grid breaks / tick marks are used
      # -----------------------------------------------------------
      if (!is.null(grid.breaks)) {
        gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by = grid.breaks))
      }
      # -----------------------------------------------------------
      # prepare label and name from dependend variable
      # -----------------------------------------------------------
      # get response name, which is variable name
      response.name <- sjstats::resp_var(fit)
      # get variable label attribute
      response.label <- sjmisc::get_label(m_f[[1]], def.value = response.name)
      # -----------------------------------------------------------
      # prepare label for x-axix
      # -----------------------------------------------------------
      alx <- sjmisc::get_labels(m_f[[term.pairs[2]]],
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
      if (is.null(legend.labels)) {
        # try to get labels
        lLabels <- sjmisc::get_labels(m_f[term.pairs[1]][, 1], attr.only = F)
        # if we still have no labels, get factor levels
        if (is.null(lLabels)) levels(m_f[term.pairs[1]][, 1])
      } else {
        lLabels <- legend.labels
      }
      # -----------------------------------------------------------
      # legend title
      # -----------------------------------------------------------
      if (is.null(legend.title)) {
        lTitle <- sjmisc::get_label(m_f[[term.pairs[1]]], def.value = term.pairs[1])
      } else {
        # set legend title for plot
        lTitle <- legend.title
      }
      # -----------------------------------------------------------
      # axis labels and titles
      # -----------------------------------------------------------
      if (is.null(axis.labels)) axis.labels <- alx
      if (is.null(axis.title)) {
        labx <- sjmisc::get_label(m_f[[term.pairs[2]]], def.value = term.pairs[2])
      } else {
        labx <- axis.title
      }
      # y-axis label
      laby <- response.label
      # -----------------------------------------------------------
      # prepare annotation labels
      # -----------------------------------------------------------
      # wrap title and axis titles
      labtitle <- sjmisc::word_wrap(labtitle, breakTitleAt)
      labx <- sjmisc::word_wrap(labx, breakTitleAt)
      laby <- sjmisc::word_wrap(laby, breakTitleAt)
      # wrap legend labels
      lLabels <- sjmisc::word_wrap(lLabels, wrap.legend.labels)
      # wrap legend title
      lTitle <- sjmisc::word_wrap(lTitle, wrap.legend.title)
      # -----------------------------------------------------------
      # prepare base plot of interactions
      # -----------------------------------------------------------
      baseplot <- ggplot(intdf)
      # -----------------------------------------------------------
      # Confidence intervals?
      # -----------------------------------------------------------
      if (show.ci) baseplot <- baseplot +
          geom_ribbon(aes_string(x = "xn", ymin = "conf.low", ymax = "conf.high", fill = "grp"), alpha = dot.args[["ci.alpha"]])
      # -----------------------------------------------------------
      # continue with plot. point and line layers above ribbon
      # -----------------------------------------------------------
      baseplot <- baseplot +
        geom_point(aes_string(x = "x", y = "y", colour = "grp")) +
        geom_line(aes_string(x = "xn", y = "y", colour = "grp"), size = geom.size) +
        scale_x_discrete(labels = axis.labels)
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (show.values) {
        baseplot <- baseplot +
          geom_text(aes(label = round(y, vld), x = x, y = y),
                    nudge_y = y.offset,
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
      if (prnt.plot) graphics::plot(baseplot)
      # concatenate plot object
      plotlist[[length(plotlist) + 1]] <- baseplot
      dflist[[length(dflist) + 1]] <- intdf
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjPlot", "sjpemmint"),
                      list(plot.list = plotlist,
                           data.list = dflist)))
}
