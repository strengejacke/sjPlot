# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xn", "vld"))

#' @title Plot adjusted (estimated marginal) means of interaction (moderation) in linear models
#' @name sjp.emm.int
#' 
#' @references \itemize{
#'              \item \href{http://www.theanalysisfactor.com/using-adjusted-means-to-interpret-moderators-in-analysis-of-covariance/}{Grace-Martin K: Using Adjusted Means to Interpret Moderators in Analysis of Covariance.}
#'              }
#'             
#' @description Plot estimated marginal means of (significant) interaction terms in linear models (lm). This function may
#'                be used to plot differences in interventions between control and treatment groups over multiple
#'                time points.
#' 
#' @note Please note that all interaction terms have to be of type \code{\link{factor}}!
#'         Furthermore, predictors of interactions that are introduced first into the model
#'         are used as grouping variable, while the latter predictor is printed along the x-axis
#'         (i.e. lm(y~a+b+a:b) means that "a" is used as grouping variable and "b" is plotted along the x-axis).
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.emm.int/}{sjPlot manual: sjp.emm.int}
#'            \item \href{http://strengejacke.wordpress.com/2014/08/19/visualize-pre-post-comparison-of-intervention-rstats/}{Weblog example}
#'            \item \code{\link{sjp.int}}
#'            \item \code{\link{sjp.reglin}}
#'            \item \code{\link{sjp.aov1}}
#'            \item \code{\link{sjp.lm.ma}}
#'          }
#' 
#' @param fit the fitted linear model (lm) object, including interaction terms
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
#' @param legendLabels Labels for the guide/legend. Default is \code{NULL}, so the name of the predictor with 
#'          min/max-effect is used as legend label.
#' @param showValueLabels if \code{TRUE}, value labels are plotted along the lines. Default is \code{FALSE}.
#' @param valueLabel.digits the amount of digits of the displayed value labels. Defaults to 2.
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
#' df <- data.frame(mpg=mtcars$mpg,vs=factor(mtcars$vs),am=factor(mtcars$am))
#' # fit "dummy" model.
#' fit <- lm(mpg~vs+am+vs:am, data=df)
#' # show summary to see significant interactions
#' summary(fit)
#' 
#' # plot marginal means of interaction terms
#' # note we have to adjust plevel, because no interaction
#' # is significant
#' sjp.emm.int(fit, plevel=1)
#' # plot marginal means of interaction terms, including value labels
#' sjp.emm.int(fit, plevel=1, showValueLabels=TRUE)
#' 
#' 
#' # load sample data set
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' df <- as.data.frame(cbind(burden=efc$neg_c_7,
#'                           sex=efc$c161sex, 
#'                           education=efc$c172code))
#' # convert gender predictor to factor                         
#' df$sex <- factor(df$sex)
#' df$education <- factor(df$education)
#' # name factor levels and dependent variable
#' levels(df$sex) <- c("female", "male")
#' levels(df$education) <- c("low", "mid", "high")
#' df$burden <- sji.setVariableLabels(df$burden, "care burden")
#' # fit "dummy" model
#' fit <- lm(burden ~ .*., data=df, na.action=na.omit)
#' summary(fit)
#' 
#' # plot marginal means of interactions, no interaction found
#' sjp.emm.int(fit)
#' # plot marginal means of interactions, including those with p-value up to 1
#' sjp.emm.int(fit, plevel=1)
#' # swap predictors
#' sjp.emm.int(fit, plevel=1, swapPredictors=TRUE)}
#' 
#' 
#' @import ggplot2
#' @export
sjp.emm.int <- function(fit,
                       swapPredictors=FALSE,
                       plevel=0.05,
                       title=NULL,
                       geom.colors="Set1",
                       axisTitle.x=NULL,
                       axisTitle.y=NULL,
                       legendLabels=NULL,
                       showValueLabels=FALSE,
                       valueLabel.digits=2,
                       breakTitleAt=50,
                       breakLegendLabelsAt=20,
                       breakAnnotationLabelsAt=50,
                       axisLimits.y=NULL,
                       gridBreaksAt=NULL,
                       printPlot=TRUE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lsmeans", quietly = TRUE)) {
    stop("Package 'lsmeans' needed for this function to work. Please install it.", call. = FALSE)
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks.x <- gridbreaks.y <- waiver()
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # -----------------------------------------------------------
  # retrieve p-values, without intercept
  # -----------------------------------------------------------
  pval <- summary(fit)$coefficients[-1,4]
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
  # loop all term labels
  for (i in 1:length(it)) {
    # check whether current term name contains a ":",
    # thus if it is an interaction term
    pos <- grep(":", it[i])
    # if yes...
    if (length(pos)>0) {
      it.names <- c(it.names, it[i])
    }
  }
  # loop all coefficients
  for (i in 1:length(cf)) {
    # check whether current coefficient contains a ":",
    # thus if it is an interaction term
    pos <- grep(":", cf[i])
    # if yes...
    if (length(pos)>0) {
      # ... increase counter of interactions
      it.nr <- it.nr+1
      # ... and save position of coefficient in model
      it.pos <- c(it.pos, i)
    }
  }
  # check whether we have any interaction terms included at all
  if(it.nr==0) {
    stop("No interaction term found in fitted model...", call.=FALSE)
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
  if (is.null(intnames) || 0==length(intnames)) {
    stop("No significant interactions found...", call.=FALSE)
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
      if (grepl(terms[1], lvls[1]) && grepl(terms[2], lvls[2])) {
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
    term.pairs <- interactionterms[cnt,]
    if (swapPredictors) term.pairs <- rev(term.pairs)
    # -----------------------------------------------------------
    # retrieve estiamted marginal means
    # -----------------------------------------------------------
    emm <- summary(lsmeans::lsmeans.character(fit, term.pairs))
    # create data frame from lsmeans
    intdf <- data.frame(emm[2], emm[3], emm[1], emm[6], emm[7], rep(valueLabel.digits, times=nrow(emm[1])))
    colnames(intdf) <- c("x", "y", "grp", "l.ci", "u.ci", "vld")
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    intdf$y <- as.numeric(as.character(intdf$y))
    # add numeric x for geom_line
    intdf$xn <- as.numeric(intdf$x)
    # order data frame
    intdf <- intdf[order(intdf$grp),]
    # -----------------------------------------------------------
    # retrieve lowest and highest x and y position to determine
    # the scale limits
    # -----------------------------------------------------------
    if (is.null(axisLimits.y)) {
      lowerLim.y <- floor(min(intdf$y))
      upperLim.y <- ceiling(max(intdf$y))
    }
    else {
      lowerLim.y <- axisLimits.y[1]
      upperLim.y <- axisLimits.y[2]
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by=gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare label and name from depend variable
    # -----------------------------------------------------------
    response.name <- attr(fit$model[[1]],"variable.label")
    response.label <- unname(attr(fit$model[[1]],"variable.label"))    
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Interaction of ", term.pairs[1], 
                         " and ", term.pairs[2],
                         " on ", response.label)
    }
    else {
      labtitle <- title
    }
    if (is.null(legendLabels)) {
      lLabels <- levels(fit$model[term.pairs[1]][,1])
    }
    else {
      lLabels <- legendLabels
    }
    if (!is.null(axisTitle.x)) {
      labx <- axisTitle.x
    }
    else {
      labx <- term.pairs[2]
    }
    if (!is.null(axisTitle.y)) {
      laby <- axisTitle.y
    }
    else {
      laby <- response.name
    }
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    # wrap title
    labtitle <- sju.wordwrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- sju.wordwrap(lLabels, breakLegendLabelsAt)
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    baseplot <- ggplot(intdf) + 
      geom_point(aes(x=x, y=y, colour=grp)) +
      geom_line(aes(x=xn, y=y, colour=grp))
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (showValueLabels) {
      baseplot <- baseplot +
        geom_text(aes(label=round(y,vld), x=x, y=y), vjust=1.5, show_guide=FALSE)
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot + 
      # set plot and axis titles
      labs(title=labtitle, x=labx, y=laby, colour = term.pairs[1]) +
      # set axis scale breaks
      scale_y_continuous(limits=c(lowerLim.y, upperLim.y), breaks=gridbreaks.y)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot, geom.colors, length(lLabels), TRUE, lLabels)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (printPlot) print(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist)+1]] <- baseplot
    dflist[[length(dflist)+1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpemmint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}