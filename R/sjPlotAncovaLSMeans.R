# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xn", "vld", "l.ci", "u.ci"))

#' @title Plot estimated marginal means of Two-Way Repeated Measures AN(C)OVA
#' @name sjp.emm.int
#' 
#' @references \href{http://www.theanalysisfactor.com/using-adjusted-means-to-interpret-moderators-in-analysis-of-covariance/}{Grace-Martin K: Using Adjusted Means to Interpret Moderators in Analysis of Covariance.}
#'             
#' @description Plot estimated marginal means (also called \emph{least square means} or 
#'                \emph{marginal means}) of (significant) interaction terms in two-way
#'                repeated measure ANOVA or ANCOVA. The fitted models may be linear (mixed 
#'                effects) models of class \code{\link{lm}} or \code{\link[lme4]{merMod}}.
#'                This function may be used to plot differences in interventions between 
#'                control and treatment groups over multiple time points.
#' 
#' @note Please note that all interaction terms have to be of type \code{\link{factor}}!
#'         Furthermore, predictors of interactions that are introduced first into the model
#'         are used as grouping variable, while the latter predictor is printed along the x-axis
#'         (i.e. lm(y~a+b+a:b) means that "a" is used as grouping variable and "b" is plotted along the x-axis).
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.emm.int/}{sjPlot manual: sjp.emm.int}
#'            \item \href{http://strengejacke.wordpress.com/2014/08/19/visualize-pre-post-comparison-of-intervention-rstats/}{Weblog example}
#'          }
#' 
#' @param fit the fitted linear (mixed effect) model (\code{\link{lm}} or \code{\link[lme4]{lmer}}),
#'          including interaction terms.
#' @param swapPredictors if \code{TRUE}, the grouping variable and predictor on
#'          the x-axis are swapped.
#' @param plevel Indicates at which p-value an interaction term is considered as significant. Default is
#'          0.05 (5 percent).
#' @param title a default title used for the plots. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param geom.colors A vector of color values.
#' @param axisTitle.x a default title used for the x-axis. Default value is \code{NULL}, 
#'          which means that each plot's x-axis uses the predictor's name as title.
#' @param axisTitle.y a default title used for the y-axis. Default value is \code{NULL}, 
#'          which means that each plot's y-axis uses the dependent variable's name as title.
#' @param axisLabels.x Character vector with value labels of the repeated measure variable 
#'          that are used for labelling the x-axis.
#' @param legendTitle Character vector with title of the diagram's legend. Default is 
#'          \code{NULL}, so the name of the grouping variable is used as legend title.
#' @param legendLabels Labels for the guide/legend. Default is \code{NULL}, so the levels of 
#'          the grouping variable are used as legend labels.
#' @param showValueLabels if \code{TRUE}, value labels are plotted along the lines. Default is \code{FALSE}.
#' @param valueLabel.digits the amount of digits of the displayed value labels. Defaults to 2.
#' @param showCI If \code{TRUE}, a confidence region for the estimated marginal means
#'          will be plotted.
#' @param breakTitleAt Wordwrap for diagram's title. Determines how many chars of the title are 
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted. Default is \code{20}.
#' @param axisLimits.y A vector with two values, defining the lower and upper limit from the y-axis.
#'          By default, this value is \code{NULL}, i.e. axis limits will be calculated upon the
#'          range of y-values.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list}) 
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#' 
#' @examples
#' \dontrun{
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#' 
#' # prepare data frame
#' df <- data.frame(mpg = mtcars$mpg,
#'                  vs = factor(mtcars$vs),
#'                  am = factor(mtcars$am))
#' # fit "dummy" model.
#' fit <- lm(mpg ~ vs + am + vs:am, data = df)
#' # show summary to see significant interactions
#' summary(fit)
#' 
#' # plot marginal means of interaction terms
#' # note we have to adjust plevel, because no interaction
#' # is significant
#' sjp.emm.int(fit, plevel = 1)
#' # plot marginal means of interaction terms, including value labels
#' sjp.emm.int(fit, plevel = 1, showValueLabels = TRUE)
#' 
#' 
#' # load sample data set
#' library(sjmisc)
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' mydf <- data.frame(burden = efc$neg_c_7,
#'                    sex = efc$c161sex, 
#'                    education = efc$c172code)
#' # convert gender predictor to factor                         
#' mydf$sex <- factor(mydf$sex)
#' mydf$education <- factor(mydf$education)
#' # name factor levels and dependent variable
#' levels(mydf$sex) <- c("female", "male")
#' levels(mydf$education) <- c("low", "mid", "high")
#' mydf$burden <- set_var_labels(mydf$burden, "care burden")
#' # fit "dummy" model
#' fit <- lm(burden ~ .*., data = mydf, na.action = na.omit)
#' summary(fit)
#' 
#' # plot marginal means of interactions, no interaction found
#' sjp.emm.int(fit)
#' # plot marginal means of interactions, including those with p-value up to 1
#' sjp.emm.int(fit, plevel = 1)
#' # swap predictors
#' sjp.emm.int(fit, plevel = 1, swapPredictors = TRUE)}
#' 
#' 
#' @import ggplot2
#' @import sjmisc
#' @export
sjp.emm.int <- function(fit,
                       swapPredictors=FALSE,
                       plevel=0.05,
                       title=NULL,
                       geom.colors="Set1",
                       axisTitle.x=NULL,
                       axisTitle.y=NULL,
                       axisLabels.x=NULL,
                       legendTitle=NULL,
                       legendLabels=NULL,
                       showValueLabels=FALSE,
                       valueLabel.digits=2,
                       showCI=FALSE,
                       breakTitleAt=50,
                       breakLegendTitleAt=20,
                       breakLegendLabelsAt=20,
                       axisLimits.y=NULL,
                       gridBreaksAt=NULL,
                       printPlot=TRUE) {
  # ------------------------
  # check if suggested packages are available
  # ------------------------
  if (!requireNamespace("lsmeans", quietly = TRUE)) {
    stop("Package 'lsmeans' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (any(class(fit) == "lmerMod") && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -----------------------------------------------------------
  # go to sub-function if class = lmerMod
  # -----------------------------------------------------------
  if (any(class(fit) == "lmerMod")) {
    return (sjp.emm.lmer(fit, swapPredictors, plevel, title, geom.colors,
                         axisTitle.x, axisTitle.y, axisLabels.x, legendLabels, 
                         showValueLabels, valueLabel.digits, showCI, breakTitleAt, 
                         breakLegendLabelsAt, axisLimits.y, gridBreaksAt, printPlot))
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) gridbreaks.x <- gridbreaks.y <- waiver()
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
  if(it.nr == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return (NULL)
  }
  # save names of interaction predictor variables into this object
  # but only those with a specific p-level
  intnames <- c()
  for (i in 1:length(it.pos)) {
    if (pval[it.pos[i]] < plevel) {
      intnames <- c(intnames, cf[it.pos[i]])
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0 == length(intnames)) {
    warning("No significant interactions found...", call. = FALSE)
    return (NULL)
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
  }
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
    response.label <- sjmisc:::autoSetVariableLabels(fit$model[[1]])
    # check if we have any
    if (is.null(response.label)) response.label <- response.name
    # -----------------------------------------------------------
    # prepare label for x-axix
    # -----------------------------------------------------------
    # get value label attribute
    alx <- sjmisc:::autoSetValueLabels(fit$model[[term.pairs[2]]])
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
      geom_line(aes(x = xn, y = y, colour = grp)) +
      scale_x_discrete(labels = axisLabels.x)
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (showValueLabels) {
      baseplot <- baseplot +
        geom_text(aes(label = round(y, vld), x = x, y = y), 
                  vjust = 1.5, 
                  show_guide = FALSE)
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
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpemmint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}


sjp.emm.lmer <- function(fit, swapPredictors, plevel, title, geom.colors, axisTitle.x,
                         axisTitle.y, axisLabels.x, legendLabels, showValueLabels, 
                         valueLabel.digits, showCI, breakTitleAt, breakLegendLabelsAt, 
                         axisLimits.y, gridBreaksAt, printPlot) {
  if (any(class(fit) == "lmerMod") && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' needed for this function to work. Please install it.", call. = FALSE)
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) gridbreaks.x <- gridbreaks.y <- waiver()
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
    return (NULL)
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
  if (ncol(fit.coef) > 4) {
    pval <- fit.coef[pos[1]:nrow(fit.coef), 5]
    # get significant interactions
    intnames <- cf[pos[which(pval < plevel)]]
  } else {
    pval <- NULL
    intnames <- cf[pos[1]:nrow(fit.coef)]
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0 == length(intnames)) {
    warning("No significant interactions found...", call. = FALSE)
    return (NULL)
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
  }
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
    # -----------------------------------Ï------------------------
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
    response.label <- sjmisc:::autoSetVariableLabels(fit@frame[[1]])
    # check if we have any
    if (is.null(response.label)) response.label <- response.name
    # -----------------------------------------------------------
    # prepare label for x-axix
    # -----------------------------------------------------------
    # get value label attribute
    alx <- sjmisc:::autoSetValueLabels(fit@frame[[term.pairs[2]]])
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
      geom_line(aes(x = xn, y = y, colour = grp)) +
      scale_x_discrete(labels = axisLabels.x)
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (showValueLabels) {
      baseplot <- baseplot +
        geom_text(aes(label = round(y, vld), x = x, y = y), 
                  vjust = 1.5, 
                  show_guide = FALSE)
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
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpemmint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}