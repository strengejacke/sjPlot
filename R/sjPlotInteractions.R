#' @title Plot interaction effects of (generalized) linear (mixed) models
#' @name sjp.int
#'
#' @references \itemize{
#'              \item \href{http://www.theanalysisfactor.com/interpreting-interactions-in-regression/}{Grace-Martin K: Interpreting Interactions in Regression}
#'              \item \href{http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/}{Grace-Martin K: Clarifications on Interpreting Interactions in Regression}
#'              \item \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin K: 3 Tips to Make Interpreting Moderation Effects Easier}
#'              \item Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'              }
#'
#' @description Plot regression (predicted values) or probability lines (predicted probabilities) of 
#'                significant interaction terms to better understand effects
#'                of moderations in regression models. This function accepts following fitted model classes:
#'                \itemize{
#'                  \item linear models (\code{lm})
#'                  \item generalized linear models (\code{glm})
#'                  \item linear mixed effects models (\code{lme4::lmer})
#'                  \item generalized linear mixed effects models (\code{lme4::glmer})
#'                }
#'                Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'                must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work, 
#'                but \code{lm(dep ~ pred1:pred2)} won't!
#'
#' @note Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'        must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work, 
#'        but \code{lm(dep ~ pred1:pred2)} won't!
#'
#' @seealso \itemize{
#'            \item \code{\link{sjp.emm.int}}
#'            }
#'
#' @param fit the fitted (generalized) linear (mixed) model object, including interaction terms. Accepted model
#'          classes are
#'          \itemize{
#'            \item linear models (\code{lm})
#'            \item generalized linear models (\code{glm})
#'            \item linear mixed effects models (\code{lme4::lmer})
#'            \item generalized linear mixed effects models (\code{lme4::glmer})
#'            }
#' @param diff if \code{FALSE} (default), the minimum and maximum interaction effects of predictor 2 on predictor 1
#'          are shown (one line each). if \code{TRUE}, only the difference between minimum and maximum interaction effect
#'          is shown (single line)
#' @param moderatorValues indicates which values of the moderator variable should be used when plotting the effects of the
#'          independent variable on the dependent variable. By default, \code{"minmax"} is used, i.e. the minimum and maximum values
#'          (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.
#'          Use \code{"meansd"} to use the mean value of the moderator as well as one standard deviation below and above mean value
#'          to plot the effect of the moderator on the independent variable (following
#'          the convention suggested by Cohen and Cohen and popularized by Aiken and West,
#'          i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean
#'          as values of the moderator, see \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin K: 3 Tips to Make Interpreting Moderation Effects Easier}).
#' @param swapPredictors if \code{TRUE}, the predictor with less unique values is printed along the x-axis. Default is
#'          \code{FALSE}, so the predictor with more unique values is printed along the x-axis.
#' @param plevel Indicates at which p-value an interaction term is considered as significant. Default is
#'          0.05 (5 percent).
#' @param title a default title used for the plots. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param fillColor fill color of the shaded area between the minimum and maximum lines. Default is \code{"grey"}.
#'          Either set \code{fillColor} to \code{NULL} or use 0 for \code{fillAlpha} if you want to hide the shaded area.
#' @param fillAlpha alpha value (transparancy) of the shaded area between the minimum and maximum lines. Default is 0.4.
#'          Use either 0 or set \code{fillColor} to \code{NULL} if you want to hide the shaded area.
#' @param geom.colors A vector of color values. First value is the color of the line indicating the lower bound of
#'          the interaction term (moderator value). Second value is the color of the line indicating the upper bound of
#'          the interaction term (moderator value). Third value, if applicable, is the color of the line indicating the
#'          mean value of the interaction term (moderator value). Third value is only used when \code{moderatorValues}
#'          is \code{"meansd"}. Or, if \code{diff} is \code{TRUE}, only one color value for the line indicating the
#'          upper difference between lower and upper bound of interaction terms.
#' @param axisTitle.x a default title used for the x-axis. Default value is \code{NULL},
#'          which means that each plot's x-axis uses the predictor's name as title.
#' @param axisTitle.y a default title used for the y-axis. Default value is \code{NULL},
#'          which means that each plot's y-axis uses the dependent variable's name as title.
#' @param legendLabels Labels for the guide/legend. Default is \code{NULL}, so the name of the predictor with
#'          min/max-effect is used as legend label.
#' @param showValueLabels if \code{TRUE}, value labels are plotted along the lines. Default is \code{FALSE}.
#' @param breakTitleAt Wordwrap for diagram's title. Determines how many chars of the title are
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are
#'          displayed in one line and when a line break is inserted. Default is \code{20}.
#' @param breakAnnotationLabelsAt Wordwrap for diagram annotation labels. Determines how many chars of the legend labels are
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#'          Only applies if \code{showInterceptLine} is \code{TRUE}.
#' @param axisLimits.y A vector with two values, defining the lower and upper limit from the y-axis.
#'          By default, this value is \code{NULL}, i.e. axis limits will be calculated upon the
#'          range of y-values.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}.
#' @param showInterceptLines If \code{TRUE}, the intercept and the estimate of the predictor
#'          (reference category of predictor in case interaction is not present) are plotted.
#' @param showInterceptLabels If \code{TRUE} (default), the intercept lines are labelled. Only
#'          applies if \code{showInterceptLines} is \code{TRUE}.
#' @param interceptLineColor The line color of the model's intercept line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param estLineColor The line color of the model's predictor's estimate line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param lineLabelSize The size of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is 3.7.
#' @param lineLabelColor The color of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is \code{"black"}.
#' @param lineLabelString Default string for the intercept lines that is appended to the predictor
#'          variable name. By default, this string is \code{"(no interaction)"}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list})
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#'
#' @examples
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#'
#' # fit "dummy" model.
#' fit <- lm(weight ~ Time * Diet,
#'           data = ChickWeight,
#'           x = TRUE)
#'
#' # show summary to see significant interactions
#' summary(fit)
#'
#' # plot regression line of interaction terms
#' sjp.int(fit)
#' # plot regression line of interaction terms, including value labels
#' sjp.int(fit, showValueLabels = TRUE)
#'
#'
#' # load sample data set
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' mydf <- data.frame(usage = efc$tot_sc_e,
#'                    sex = efc$c161sex,
#'                    education = efc$c172code,
#'                    burden = efc$neg_c_7,
#'                    dependency = efc$e42dep)
#' # convert gender predictor to factor
#' mydf$sex <- relevel(factor(mydf$sex), ref = "2")
#' # fit "dummy" model
#' fit <- lm(usage ~ .*., data = mydf, x = TRUE)
#' summary(fit)
#'
#' # plot interactions
#' sjp.int(fit)
#'
#' # plot interactions, using mean and sd as moderator
#' # values to calculate interaction effect
#' sjp.int(fit, moderatorValues = "meansd")
#'
#' # plot interactions, including those with p-value up to 0.1
#' sjp.int(fit,
#'         plevel = 0.1,
#'         showInterceptLines = TRUE)
#'
#' # -------------------------------
#' # Predictors for negative impact of care.
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
#' # create data frame for fitted model
#' mydf <- data.frame(y = as.factor(y),
#'                    sex = as.factor(efc$c161sex),
#'                    barthel = as.numeric(efc$barthtot))
#' # fit model
#' fit <- glm(y ~ sex * barthel,
#'            data = mydf,
#'            family = binomial(link = "logit"),
#'            x = TRUE)
#' # plot interaction, increase p-level sensivity
#' sjp.int(fit,
#'         legendLabels = get_val_labels(efc$c161sex),
#'         plevel = 0.1)
#'
#' # compare results to boxplots
#' sjp.grpfrq(mydf$barthel,
#'            mydf$y,
#'            interactionVar = mydf$sex,
#'            interactionVarLabels = get_val_labels(efc$c161sex),
#'            legendLabels = c("low burden", "high burden"),
#'            type = "box")
#'
#' @import ggplot2
#' @export
sjp.int <- function(fit,
                    diff=FALSE,
                    moderatorValues="minmax",
                    swapPredictors=FALSE,
                    plevel=0.05,
                    title=NULL,
                    fillColor="grey",
                    fillAlpha=0.4,
                    geom.colors="Set1",
                    axisTitle.x=NULL,
                    axisTitle.y=NULL,
                    legendLabels=NULL,
                    showValueLabels=FALSE,
                    breakTitleAt=50,
                    breakLegendLabelsAt=20,
                    breakAnnotationLabelsAt=50,
                    axisLimits.y=NULL,
                    gridBreaksAt=NULL,
                    showInterceptLines=FALSE,
                    showInterceptLabels=TRUE,
                    interceptLineColor="darkseagreen4",
                    estLineColor="darkslategray4",
                    lineLabelSize=3.7,
                    lineLabelColor="black",
                    lineLabelString="(no interaction)",
                    printPlot=TRUE) {
  # -----------------------------------------------------------
  # check class of fitted model
  # -----------------------------------------------------------
  c.f <- class(fit)
  if (length(c.f) == 1 && c.f == "lm") {
    fun <- "lm"
  } else if (length(c.f) > 1 && any(c.f == "glm")) {
    fun <- "glm"
  } else if (c.f == "lmerMod") {
    fun <- "lmer"
  } else if (c.f == "glmerMod") {
    fun <- "glmer"
  }
  if ((fun == "glm" || fun == "glmer") && is.null(axisTitle.y)) axisTitle.y <- "Predicted Probability"
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(fillColor)) {
    fillColor <- "white"
    fillAlpha <- 0
  }
  if (is.null(gridBreaksAt)) gridbreaks.x <- gridbreaks.y <- waiver()
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # -----------------------------------------------------------
  # retrieve coefficients
  # -----------------------------------------------------------
  coef.tab <- summary(fit)$coefficients
  pval <- rep(0, times = nrow(coef.tab) - 1)
  # -----------------------------------------------------------
  # prepare values for (generalized) linear models
  # -----------------------------------------------------------
  if (fun == "lm" || fun == "glm") {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    predvars <- attr(attr(fit$terms, "dataClasses"), "names")[-1]
    depvar.label <- attr(attr(fit$terms, "dataClasses"), "names")[1]
    # remember length of predictor variables
    predvars.length <- length(predvars)
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    if (ncol(coef.tab) > 3) pval <- coef.tab[-1, 4]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- coef.tab[-1, 1]
    estimates.names <- it <- names(estimates)
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- estimates.intercept <- coef.tab[1, 1]
  # -----------------------------------------------------------
  # prepare values for (generalized) linear mixed effecrs models
  # -----------------------------------------------------------
  } else {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    predvars <- colnames(fit@frame)[-1]
    depvar.label <- colnames(fit@frame)[1]
    # remember length of predictor variables
    predvars.length <- length(predvars)
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    if (ncol(coef.tab) > 3) pval <- coef.tab[-1, 4]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- unname(lme4::fixef(fit)[-1])
    estimates.names <- names(lme4::fixef(fit)[-1])
    # -----------------------------------------------------------
    # copy variable values to data frame
    # -----------------------------------------------------------
    fitdat <- fit@frame
    # -----------------------------------------------------------
    # extract factors, to check whether factor levels are included
    # as interaction terms. interaction terms with factors are labelled
    # with additional factor levels (e.g. "sex" will be "sex2:age").
    # However, since merMod objects don't return the model matrix,
    # the data frame's variable names do not equal the term names.
    # in order to find the original values of interaction terms in
    # the data frame, we need to "rename" the terms into the related
    # variable names in the data frame
    # -----------------------------------------------------------
    fac.names <- c()
    # find factor variables
    for (i in 1:ncol(fitdat)) {
      if (is.factor(fitdat[, i])) fac.names <- c(fac.names, colnames(fitdat)[i])
    }
    # if we found any, check if factor variable
    # was used as interaction term
    if (!is.null(fac.names)) {
      for (i in 1:length(fac.names)) {
        # retrieve all factor levels except reference category
        fac.lvl <- levels(fitdat[, fac.names[i]])[-1]
        # iterate interaction term for all factor levels,
        # and replace with "original variable name in data frame
        for (j in 1:length(fac.lvl)) {
          # create replacement-strings
          rep1 <- paste0(fac.names[i], fac.lvl[j])
          rep2 <- paste0(fac.names[i])
          # replace in all
          estimates.names <- gsub(rep1, rep2, estimates.names, fixed = TRUE)
        }
      }
    }
    it <- estimates.names
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- estimates.intercept <- unname(lme4::fixef(fit)[1])
  }
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # init indicator for first term
  firstit <- 0
  # iterate all rownames. interaction terms contain a colon...
  for (i in 1:length(it)) {
    # check whether current interactio term name contains a ":",
    # and firstit is not already set
    pos <- grep(":", it[i], fixed = FALSE)
    if (length(pos) > 0) {
      # set position to first interaction term in model
      firstit <- i
      break;
    }
  }
  # check whether we have any interaction terms included at all
  if(firstit == 0) stop("No interaction term found in fitted model...", call. = FALSE)
  # save names of interaction predictor variables into this object
  intnames <- c()
  for (i in firstit:length(pval)) {
    if (pval[i] < plevel) intnames <- c(intnames, it[i])
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) stop("No significant interactions found...", call. = FALSE)
  # -----------------------------------------------------------
  # check whether parameter X=TRUE was set when fitting the linear
  # model. if not, we cannot procede here. not needed for
  # merMod objects, see above
  # -----------------------------------------------------------
  if(fun == "lm" || fun == "glm") {
    if (class(fit$x) != "matrix") {
      stop("The model matrix is not available! Please use \"x=TRUE\" in your (g)lm-command...", call. = FALSE)
    }
    # -----------------------------------------------------------
    # copy variable values to data frame
    # -----------------------------------------------------------
    fitdat <- as.data.frame(fit$x)
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # Now iterate all significant interaction terms
  # and manually calculate the linear regression by inserting
  # the estimates of each term and the associated interaction term,
  # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
  # -----------------------------------------------------------
  for (cnt in 1:length(intnames)) {
    # -----------------------------------------------------------
    # first, retrieve and split interaction term so we know
    # the two predictor variables of the interaction term
    # -----------------------------------------------------------
    interactionterms <- strsplit(intnames[cnt], ":")
    labx <- c()
    # Label on y-axis is name of dependent variable
    laby <- depvar.label
    # -----------------------------------------------------------
    # find estimates (beta values) for each single predictor of
    # the interaction as well as of the interaction term
    # -----------------------------------------------------------
    b1 <- as.numeric(estimates[match(interactionterms[[1]][1], estimates.names)])
    b2 <- as.numeric(estimates[match(interactionterms[[1]][2], estimates.names)])
    b3 <- as.numeric(estimates[match(intnames[cnt], estimates.names)])
    # -----------------------------------------------------------
    # check whether each predictor was included in the model
    # as single term as well
    # -----------------------------------------------------------
    if(is.na(b1) || is.na(b2) || is.na(b3)) {
      stop("Predictors of interaction terms (main effects) must be included as single term as well. See Note in ?sjp.int", call. = FALSE)
    }
    # -----------------------------------------------------------
    # retrieve number of unique values in each predictor variable.
    # depending on the amount of values the variable for the x-axis
    # is chosen. In this case, we use the predictor with the higher
    # number of unique values on the x-axis.
    # -----------------------------------------------------------
    # retrieve values as data frame
    df_pred1uniquevals <- unique(fitdat[interactionterms[[1]][1]])
    df_pred2uniquevals <- unique(fitdat[interactionterms[[1]][2]])
    # convert data frame to numeric vector
    pred1uniquevals <- pred2uniquevals <- as.numeric(c())
    pred1uniquevals <- sort(as.numeric(c(apply(df_pred1uniquevals, c(1), as.numeric ))))
    pred2uniquevals <- sort(as.numeric(c(apply(df_pred2uniquevals, c(1), as.numeric ))))
    # init data frame
    intdf <- c()
    # -----------------------------------------------------------
    # choose x-value according to higher number of unique values
    # choose minimum and maximum value from predictor that has
    # a "smaller range" (i.e. less unique values)
    # or swap predictors on axes if requested
    # -----------------------------------------------------------
    if (swapPredictors) {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), F, T)
    } else {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), T, F)
    }
    # -----------------------------------------------------------
    # calculate regression line
    # -----------------------------------------------------------
    if (useFirstPredOnY) {
      labx <- c(interactionterms[[1]][1])
      predy <- c(interactionterms[[1]][2])
      # -----------------------------------------------------------
      # check which values of moderator should be plotted, i.e. if
      # lower/upper bound (min-max) or mean and standard-deviation
      # should be used as valus for the moderator.
      # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
      # -----------------------------------------------------------
      if (moderatorValues == "minmax") {
        mw <- NA
        ymin <- min(pred2uniquevals)
        ymax <- max(pred2uniquevals)
      } else {
        mw <- mean(pred2uniquevals, na.rm = T)
        ymin <- mw - sd(pred2uniquevals, na.rm = T)
        ymax <- mw + sd(pred2uniquevals, na.rm = T)
      }
      # intercept of predictor's reference category
      est_b <- b2 + b0
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      for (j in 1:length(pred1uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred1uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 1 under absence (or lowest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*pr) + (b2*ymin) + (b3*pr*ymin))
        miny = (b0 + (b1 * pr) + (b3 * pr * ymin))
        # ------------------------------
        # here we calculate the effect of predictor 1 under presence (or strongest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*pr) + (b2*ymax) + (b3*pr*ymax))
        maxy = (b0 + (b1 * pr) + (b3 * pr * ymax))
        # store in df
        tmp <- as.data.frame(cbind(x = pr, 
                                   y = miny, 
                                   ymin = miny, 
                                   ymax = maxy, 
                                   grp = "min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x = pr, 
                                   y = maxy, 
                                   ymin = miny, 
                                   ymax = maxy, 
                                   grp = "max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        if (moderatorValues != "minmax") {
          # ------------------------------
          # here we calculate the effect of predictor 1 under presence
          # of mean of predictor 2 on the dependent variable. Thus, the slope for
          # predictor 2 only is not needed. see references above
          # ------------------------------
          mittelwert <- (b0 + (b1 * pr) + (b3 * pr * mw))
          tmp <- as.data.frame(cbind(x = pr, 
                                     y = mittelwert, 
                                     ymin = miny, 
                                     ymax = maxy, 
                                     grp = "mean"))
          intdf <- as.data.frame(rbind(intdf, tmp))
        }
      }
    } else {
      labx <- c(interactionterms[[1]][2])
      predy <- c(interactionterms[[1]][1])
      # -----------------------------------------------------------
      # check which values of moderator should be plotted, i.e. if
      # lower/upper bound (min-max) or mean and standard-deviation
      # should be used as valus for the moderator.
      # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
      # -----------------------------------------------------------
      if (moderatorValues == "minmax") {
        mw <- NA
        ymin <- min(pred1uniquevals)
        ymax <- max(pred1uniquevals)
      } else {
        mw <- mean(pred1uniquevals, na.rm = T)
        ymin <- mw-sd(pred1uniquevals, na.rm = T)
        ymax <- mw+sd(pred1uniquevals, na.rm = T)
      }
      # intercept of predictor's reference category
      est_b <- b1 + b0
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      # compute for minimum value
      for (j in 1:length(pred2uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred2uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 2 under absence (or lowest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*ymin) + (b2*pr) + (b3*pr*ymin))
        miny = (b0 + (b2 * pr) + (b3 * pr * ymin))
        # ------------------------------
        # here we calculate the effect of predictor 2 under presence (or strongest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*ymax) + (b2*pr) + (b3*pr*ymax))
        maxy = (b0 + (b2 * pr) + (b3 * pr * ymax))
        # store in df
        tmp <- as.data.frame(cbind(x = pr, 
                                   y = miny, 
                                   ymin = miny, 
                                   ymax = maxy, 
                                   grp = "min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x = pr, 
                                   y = maxy, 
                                   ymin = miny, 
                                   ymax = maxy, 
                                   grp = "max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        if (moderatorValues != "minmax") {
          # ------------------------------
          # here we calculate the effect of predictor 2 under presence
          # of mean of predictor 1 on the dependent variable. Thus, the slope for
          # predictor 1 only is not needed. see references above
          # ------------------------------
          mittelwert <- (b0 + (b2 * pr) + (b3 * pr * mw))
          tmp <- as.data.frame(cbind(x = pr, y = mittelwert, ymin = miny, ymax = maxy, grp = "mean"))
          intdf <- as.data.frame(rbind(intdf, tmp))
        }
      }
    }
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    if (fun == "lm" || fun == "lmer") {
      intdf$x <- as.numeric(as.character(intdf$x))
      intdf$y <- as.numeric(as.character(intdf$y))
      intdf$ymin <- as.numeric(as.character(intdf$ymin))
      intdf$ymax <- as.numeric(as.character(intdf$ymax))
      intdf$ydiff <- intdf$ymax - intdf$ymin
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      lowerLim.x <- floor(min(intdf$x))
      upperLim.x <- ceiling(max(intdf$x))
      if (is.null(axisLimits.y)) {
        lowerLim.y <- floor(min(intdf$y))
        upperLim.y <- ceiling(max(intdf$y))
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    } else {
      intdf$x <- as.numeric(as.character(intdf$x))
      intdf$y <- odds.to.prob(as.numeric(as.character(intdf$y)))
      intdf$ymin <- odds.to.prob(as.numeric(as.character(intdf$ymin)))
      intdf$ymax <- odds.to.prob(as.numeric(as.character(intdf$ymax)))
      intdf$ydiff <- odds.to.prob(intdf$ymax - intdf$ymin)
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      lowerLim.x <- floor(min(intdf$x))
      upperLim.x <- ceiling(max(intdf$x))
      if (is.null(axisLimits.y)) {
        lowerLim.y <- 0
        upperLim.y <- 1
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    }
    # -----------------------------------------------------------
    # check whether we have to modify axis limits in case intercept
    # lines are also plotted
    # -----------------------------------------------------------
    if (showInterceptLines) {
      # retrieve intercept bounds
      ilmin <- min(b0, est_b)
      ilmax <- max(b0, est_b)
      # adjust lower lim if necessary
      if (ilmin < lowerLim.y) lowerLim.y <- floor(ilmin)
      # adjust upper lim if necessary
      if (ilmax > upperLim.y) upperLim.y <- ceiling(max(ilmax))
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.x <- c(seq(lowerLim.x, upperLim.x, by=gridBreaksAt))
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by=gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
#       labtitle <- paste0("Effect of ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,1,2)],
#                          " on ", depvar.label,
#                          " under minimum and maximum interaction with ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,2,1)])
      labtitle <- paste0("Interaction of ",
                         interactionterms[[1]][ifelse(useFirstPredOnY == TRUE, 1, 2)],
                         " and ",
                         interactionterms[[1]][ifelse(useFirstPredOnY == TRUE, 2, 1)],
                         " on ", depvar.label)
    } else {
      labtitle <- title
    }
    if (is.null(legendLabels)) {
      if (moderatorValues == "minmax") {
        lLabels <- c(paste0("lower bound of ", predy), paste0("upper bound of ", predy))
      } else {
        lLabels <- c(paste0("lower sd of ", predy), paste0("upper sd of ", predy), paste0("mean of ", predy))
      }
    } else {
      lLabels <- legendLabels
    }
    if (!is.null(axisTitle.x)) labx <- axisTitle.x
    if (!is.null(axisTitle.y)) laby <- axisTitle.y
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    annoLabels <- paste(lLabels[1], lineLabelString)
    annoLabels <- c(annoLabels, paste(lLabels[2], lineLabelString))
    # wrap title
    labtitle <- word_wrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- word_wrap(lLabels, breakLegendLabelsAt)
    # wrap annotation labels
    annoLabels <- word_wrap(annoLabels, breakAnnotationLabelsAt)
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    if (diff) {
      baseplot <- ggplot(intdf, aes(x = x, y = ydiff)) +
        # -----------------------------------------------------------
        # add a shaded region between minimun
        # and maximum curve of interactions
        # -----------------------------------------------------------
        geom_ribbon(aes(x = x, ymin = 0, ymax = ydiff),
                    fill = fillColor,
                    alpha = fillAlpha) +
        geom_line()
      # -----------------------------------------------------------
      # show value labels
      # -----------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_text(aes(label = round(ydiff, 1), x = x, y = ydiff),
                    vjust = 1.5,
                    show_guide = FALSE)
      }
    } else {
      baseplot <- ggplot(intdf) +
        # add a shaded region between minimun and maximum curve of interactions
        geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax),
                    fill = fillColor,
                    alpha = fillAlpha) +
        geom_line(aes(x = x, y = y, colour = grp))
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_point(aes(x = x, y = y, colour = grp)) +
          geom_text(aes(label = round(y, 1), x = x, y = y),
                    vjust = 1.5,
                    show_guide = FALSE)
      }
      # ------------------------------------------------------------
      # plot intercept line and estimate line (i.e. reference category
      # of predictor, in case interaction is not present)
      # ------------------------------------------------------------
      if (showInterceptLines) {
        baseplot <- baseplot +
          geom_abline(intercept = b0,
                      slope = 0,
                      colour = interceptLineColor) +
          geom_abline(intercept = est_b,
                      slope = 0,
                      colour = estLineColor)
        if (showInterceptLabels) {
          baseplot <- baseplot +
            annotate("text",
                     label = annoLabels[1],
                     x = -Inf,
                     hjust = -0.05,
                     vjust = -0.5,
                     colour = lineLabelColor,
                     size = lineLabelSize,
                     y = b0) +
            annotate("text",
                     label = annoLabels[2],
                     x = -Inf,
                     hjust = -0.05,
                     vjust = -0.5,
                     colour = lineLabelColor,
                     size = lineLabelSize,
                     y = est_b)
        }
      }
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot +
      # set plot and axis titles
      labs(title = labtitle, x = labx, y = laby) +
      # set axis scale breaks
      scale_x_continuous(limits = c(lowerLim.x, upperLim.x), breaks = gridbreaks.x) +
      scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (diff) {
      col.len <- 1
      lLabels <- NULL
    } else {
      if (moderatorValues == "minmax") {
        col.len <- 2
      } else {
        col.len <- 3
      }
    }
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot, geom.colors, col.len, !is.null(lLabels), lLabels)
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
  invisible (structure(class = "sjpint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}
