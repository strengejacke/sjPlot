#' @title Plot interaction effects of (generalized) linear (mixed) models
#' @name sjp.int
#'
#' @references \itemize{
#'              \item Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'              \item Brambor T, Clark WR and Golder M (2006) Understanding Interaction Models: Improving Empirical Analyses. Political Analysis 14: 63-82. \href{https://academic.oup.com/pan/article/14/1/63/1501303/Understanding-Interaction-Models-Improving}{download}
#'              \item Esarey J, Sumner JL (2015) Marginal Effects in Interaction Models: Determining and Controlling the False Positive Rate. \href{http://jee3.web.rice.edu/interaction-overconfidence.pdf}{download}
#'              \item Fox J (2003) Effect displays in R for generalised linear models. Journal of Statistical Software 8:15, 1–27, \href{http://www.jstatsoft.org/v08/i15/}{<http://www.jstatsoft.org/v08/i15/>}
#'              \item Hayes AF (2012) PROCESS: A versatile computational tool for observed variable mediation, moderation, and conditional process modeling [White paper] \href{http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/SobelTest?action=AttachFile&do=get&target=process.pdf}{download}
#'              \item \href{http://www.theanalysisfactor.com/interpreting-interactions-in-regression/}{Grace-Martin K: Interpreting Interactions in Regression}
#'              \item \href{http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/}{Grace-Martin K: Clarifications on Interpreting Interactions in Regression}
#'              }
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.int/}{sjPlot manual: sjp.int}
#'
#' @description Plot regression (predicted values) or probability lines (predicted probabilities) of
#'                significant interaction terms to better understand effects
#'                of moderations in regression models. This function accepts following fitted model classes:
#'                \itemize{
#'                  \item linear models (\code{\link{lm}})
#'                  \item generalized linear models (\code{\link{glm}})
#'                  \item linear mixed effects models (\code{\link[lme4]{lmer}})
#'                  \item generalized linear mixed effects models (\code{\link[lme4]{glmer}})
#'                  \item non-linear mixed effects models (\code{\link[lme4]{nlmer}})
#'                  \item linear mixed effects models (\code{\link[nlme]{lme}}, but only for \code{type = "eff"})
#'                  \item generalized least squares models (\code{\link[nlme]{gls}}, but only for \code{type = "eff"})
#'                  \item panel data estimators (\code{\link[plm]{plm}})
#'                }
#'                Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'                must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work,
#'                but \code{lm(dep ~ pred1:pred2)} won't!
#'
#' @param fit A fitted (generalized) linear (mixed) model object, including interaction terms. Accepted model
#'          classes are
#'          \itemize{
#'            \item linear models (\code{\link{lm}})
#'            \item generalized linear models (\code{\link{glm}})
#'            \item linear mixed effects models (\code{\link[lme4]{lmer}})
#'            \item generalized linear mixed effects models (\code{\link[lme4]{glmer}})
#'            \item non-linear mixed effects models (\code{\link[lme4]{nlmer}})
#'            \item linear mixed effects models (\code{\link[nlme]{lme}}, but only for \code{type = "eff"})
#'            \item generalized least squares models (\code{\link[nlme]{gls}}, but only for \code{type = "eff"})
#'            \item panel data estimators (\code{\link[plm]{plm}})
#'          }
#' @param type Interaction plot type. Use one of following values:
#'          \describe{
#'            \item{\code{type = "eff"}}{(default) plots the overall moderation effect on the response value. See 'Details'.}
#'            \item{\code{type = "cond"}}{plots the mere \emph{change} of the moderating effect on the response value (conditional effect). See 'Details'.}
#'          }
#' @param int.term Name of interaction term of \code{fit} (as character), which should be plotted
#'          when using \code{type = "eff"}. By default, this argument will be ignored
#'          (i.e. \code{int.term = NULL}). See 'Details'.
#' @param int.plot.index Numeric vector with index numbers that indicate which
#'          interaction terms should be plotted in case the \code{fit} has more than
#'          one interaction. By default, this value is \code{NULL}, hence all interactions
#'          are plotted.
#' @param diff Logical, if \code{FALSE} (default), the minimum and maximum interaction effects of the moderating variable
#'          is shown (one line each). if \code{TRUE}, only the difference between minimum and maximum interaction effect
#'          is shown (single line). Only applies to \code{type = "cond"}.
#' @param mdrt.values Indicates which values of the moderator variable should be
#'          used when plotting the interaction effects.
#'          \describe{
#'            \item{\code{"minmax"}}{(default) minimum and maximum values (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.}
#'            \item{\code{"meansd"}}{uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable (following the convention suggested by Cohen and Cohen and popularized by Aiken and West, i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean as values of the moderator, see \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin K: 3 Tips to Make Interpreting Moderation Effects Easier}).}
#'            \item{\code{"zeromax"}}{is similar to the \code{"minmax"} option, however, \code{0} is always used as minimum value for the moderator. This may be useful for predictors that don't have an empirical zero-value, but absence of moderation should be simulated by using 0 as minimum.}
#'            \item{\code{"quart"}}{calculates and uses the quartiles (lower, median and upper) of the moderator value.}
#'            \item{\code{"all"}}{uses all values of the moderator variable. Note that this option only applies to \code{type = "eff"}, for numeric moderator values.}
#'          }
#' @param swap.pred Logical, if \code{TRUE}, the predictor on the x-axis and the moderator value in an interaction are
#'          swapped. For \code{type = "eff"}, the first interaction term is used as moderator and the second term
#'          is plotted at the x-axis. For \code{type = "cond"}, the interaction's predictor with less unique values is
#'          printed along the x-axis. Default is \code{FALSE}, so the second predictor in an interaction, respectively
#'          the predictor with more unique values is printed along the x-axis.
#' @param plevel Numeric, indicates at which p-value an interaction term is considered as \emph{significant},
#'          i.e. at which p-level an interaction term will be considered for plotting. Default is
#'          0.1 (10 percent), hence, non-significant interactions are excluded by default. This
#'          argument does not apply to \code{type = "eff"}.
#' @param title Default title used for the plots. Should be a character vector
#'          of same length as interaction plots to be plotted. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param fill.color Fill color of the shaded area between the minimum and maximum lines. Default is \code{"grey"}.
#'          Either set \code{fill.color} to \code{NULL} or use 0 for \code{fill.alpha} if you want to hide the shaded area.
#' @param fill.alpha Alpha value (transparancy) of the shaded area between the minimum and maximum lines. Default is 0.4.
#'          Use either 0 or set \code{fill.color} to \code{NULL} if you want to hide the shaded area.
#' @param geom.colors Vector of color values or name of a valid color brewer palette.
#'          If not a color brewer palette name, \code{geom.colors} must be of same
#'          length as moderator values used in the plot (see \code{mdrt.values}).
#'          See also 'Details' in \code{\link{sjp.grpfrq}}.
#' @param axis.title Default title used for the x-axis. Should be a character vector
#'          of same length as interaction plots to be plotted. Default value is \code{NULL},
#'          which means that each plot's x-axis uses the predictor's name as title.
#' @param legend.title Title of the plot's legend. A character vector of same length as
#'          amount of interaction plots to be plotted (i.e. one vector element for each
#'          plot's legend title).
#' @param legend.labels Labels for the guide/legend. Either a character vector of same length as
#'          amount of legend labels of the plot, or a \code{list} of character vectors, if more than one
#'          interaction plot is plotted (i.e. one vector of legend labels for each interaction plot).
#'          Default is \code{NULL}, so the name of the predictor with min/max-effect is used
#'          as legend label.
#' @param jitter.ci Logical, if \code{TRUE} and \code{show.ci = TRUE} and confidence
#'          bands are displayed as error bars, adds jittering to lines and error bars
#'          to avoid overlapping.
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.frq
#' @inheritParams sjp.lmer
#' @inheritParams sjp.glmer
#'
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list})
#'           as well as the data frames that were used for setting up the ggplot-objects (\code{data.list}).
#'
#' @details \describe{
#'            \item{\code{type = "eff"}}{plots the overall effects (marginal effects) of the interaction, with all remaining
#'              covariates set to the mean. Effects are calculated using the \code{\link[effects]{effect}}-
#'              function from the \pkg{effects}-package.
#'              You can pass further arguments down to \code{allEffects} for flexible
#'              function call via the \code{...}-argument.
#'            }
#'            \item{\code{type = "cond"}}{plots the effective \emph{change} or \emph{impact}
#'              (conditional effect) on a dependent variable of a moderation effect, as
#'              described by Grace-Martin, i.e. the difference of the moderation effect on the
#'              dependent variable in \emph{presence} and \emph{absence} of the moderating effect
#'              (\emph{simple slope} plot or \emph{conditional effect}, see Hayes 2012). All
#'              remaining predictors are set to zero (i.e. ignored and not adjusted for).
#'              Hence, this plot type may be used especially for \emph{binary or dummy coded}
#'              moderator values (see also Esarey and Summer 2015).
#'              This type \emph{does not} show the overall effect (marginal mean, i.e. adjusted
#'              for all other predictors and covariates) of interactions on the result of Y. Use
#'              \code{type = "eff"} for effect displays similar to the \code{\link[effects]{effect}}-function
#'              from the \pkg{effects}-package.
#'            }
#'          }
#'          The argument \code{int.term} only applies to \code{type = "eff"} and can be used
#'          to select a specific interaction term of the model that should be plotted. The function
#'          then calls \code{effect(int.term, fit)} to compute effects for this specific interaction
#'          term only. This approach is recommended, when the fitted model contains many observations
#'          and/or variables, which may slow down the effect-computation dramatically. In such cases,
#'          consider computing effects for selected interaction terms only with \code{int.terms}.
#'          See 'Examples'.
#'
#' @note Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'        must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work,
#'        but \code{lm(dep ~ pred1:pred2)} won't! \cr \cr
#'        For \code{type = "eff"}, predictors of interactions that are introduced first into the model
#'        are used as grouping variable, while the latter predictor is printed along the x-axis
#'        (i.e. lm(y~a+b+a:b) means that "a" is used as grouping variable and "b" is plotted along the x-axis).
#'
#' @examples
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#'
#' # fit "dummy" model. Note that moderator should enter
#' # first the model, followed by predictor. Else, use
#' # argument "swap.pred" to change predictor on
#' # x-axis with moderator
#' fit <- lm(weight ~ Diet * Time, data = ChickWeight)
#'
#' # show summary to see significant interactions
#' summary(fit)
#'
#' # plot regression line of interaction terms, including value labels
#' sjp.int(fit, type = "eff", show.values = TRUE)
#'
#'
#' # load sample data set
#' library(sjmisc)
#' library(sjlabelled)
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
#' fit <- lm(usage ~ .*., data = mydf)
#' summary(fit)
#'
#' # plot interactions. note that type = "cond" only considers
#' # significant interactions by default. use "plevel" to
#' # adjust p-level sensivity
#' sjp.int(fit, type = "cond")
#'
#' # plot only selected interaction term for
#' # type = "eff"
#' sjp.int(fit, type = "eff", int.term = "sex*education")
#'
#' # plot interactions, using mean and sd as moderator
#' # values to calculate interaction effect
#' sjp.int(fit, type = "eff", mdrt.values = "meansd")
#' sjp.int(fit, type = "cond", mdrt.values = "meansd")
#'
#' # plot interactions, including those with p-value up to 0.1
#' sjp.int(fit, type = "cond", plevel = 0.1)
#'
#' # -------------------------------
#' # Predictors for negative impact of care.
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(stats::na.omit(efc$neg_c_7)), 0, 1)
#' # create data frame for fitted model
#' mydf <- data.frame(y = as.factor(y),
#'                    sex = as.factor(efc$c161sex),
#'                    barthel = as.numeric(efc$barthtot))
#' # fit model
#' fit <- glm(y ~ sex * barthel, data = mydf, family = binomial(link = "logit"))
#' # plot interaction, increase p-level sensivity
#' sjp.int(fit, type = "eff", legend.labels = get_labels(efc$c161sex), plevel = 0.1)
#' sjp.int(fit, type = "cond", legend.labels = get_labels(efc$c161sex), plevel = 0.1)
#'
#' \dontrun{
#' # load sample data set
#' library(sjmisc)
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' mydf <- data.frame(burden = efc$neg_c_7,
#'                    sex = efc$c161sex,
#'                    education = efc$c172code,
#'                    barthel = efc$barthtot)
#' # convert gender predictor to factor
#' mydf$sex <- factor(mydf$sex)
#' mydf$education <- factor(mydf$education)
#' # name factor levels and dependent variable
#' levels(mydf$sex) <- c("female", "male")
#' levels(mydf$education) <- c("low", "mid", "high")
#' mydf$burden <- set_label(mydf$burden, lab = "care burden")
#' # fit "dummy" model
#' fit <- lm(burden ~ .*., data = mydf)
#'
#' # plot effects
#' sjp.int(fit, type = "eff", show.ci = TRUE)
#'
#' # plot effects, faceted
#' sjp.int(fit, type = "eff", int.plot.index = 3, show.ci = TRUE, facet.grid = TRUE)}
#'
#' @import ggplot2
#' @importFrom stats family quantile sd
#' @importFrom sjmisc is_num_fac
#' @importFrom effects allEffects effect
#' @export
sjp.int <- function(fit,
                    type = c("eff", "cond"),
                    int.term = NULL,
                    int.plot.index = NULL,
                    mdrt.values = c("minmax", "meansd", "zeromax", "quart", "all"),
                    swap.pred = FALSE,
                    plevel = 0.1,
                    diff = FALSE,
                    title = NULL,
                    axis.title = NULL,
                    legend.title = NULL,
                    legend.labels = NULL,
                    wrap.title = 50,
                    wrap.legend.labels = 20,
                    wrap.legend.title = 20,
                    geom.colors = "Set1",
                    geom.size = NULL,
                    fill.color = "grey",
                    fill.alpha = 0.3,
                    show.values = FALSE,
                    show.ci = FALSE,
                    jitter.ci = FALSE,
                    p.kr = TRUE,
                    grid.breaks = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    y.offset = 0.07,
                    digits = 2,
                    facet.grid = FALSE,
                    prnt.plot = TRUE,
                    ...) {
  # -----------------------------------------------------------
  # match arguments
  # -----------------------------------------------------------
  type <- match.arg(type)
  mdrt.values <- match.arg(mdrt.values)
  # -----------------------------------------------------------
  # check class of fitted model
  # -----------------------------------------------------------
  c.f <- class(fit)
  fun <- "lm"
  if (any(c.f == "glm")) {
    fun <- "glm"
  } else if (any(c.f == "lm")) {
    fun <- "lm"
  } else if (any(c.f == "plm")) {
    fun <- "plm"
  } else if (any(c.f == "glmerMod")) {
    fun <- "glmer"
  } else if (any(c.f == "nlmerMod")) {
    fun <- "nlmer"
  } else if (any(c.f == "lmerMod") || any(c.f == "merModLmerTest")) {
    fun <- "lmer"
  } else if (any(c.f == "lme")) {
    fun <- "lme"
    if (type != "eff") {
      message("Only 'type = \"eff\"' supports objects of class 'nlme::lme'. Defaulting type to \"eff\".")
      type <- "eff"
    }
  } else if (any(c.f == "gls")) {
    fun <- "gls"
    if (type != "eff") {
      message("Only 'type = \"eff\"' supports objects of class 'nlme::gls'. Defaulting type to \"eff\".")
      type <- "eff"
    }
  }
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) geom.size = .7
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (fun == "plm" && !"package:plm" %in% search()) {
    stop("Package `plm` needs to be loaded for this function to work... Use `library(plm)` and call this function again.", call. = FALSE)
  }
  # -----------------------------------------------------------
  # argument check
  # -----------------------------------------------------------
  if (is.null(fill.color)) {
    fill.color <- "white"
    fill.alpha <- 0
  }
  # gridbreaks
  if (is.null(grid.breaks)) gridbreaks.x <- gridbreaks.y <- ggplot2::waiver()
  # check matching argument combinations
  if (type == "cond" && mdrt.values == "all") {
    message("`mdrt.values = \"all\"` only applies to `type = \"eff\"`. Defaulting `mdrt.values` to `minmax`.")
    mdrt.values <- "minmax"
  }
  # ------------------------
  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  # ------------------------
  fitfam <- get_glm_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  binom_fam <- fitfam$is_bin
  # --------------------------------------------------------
  # list labels
  # --------------------------------------------------------
  if (!is.null(legend.labels) && !is.list(legend.labels)) legend.labels <- list(legend.labels)
  if (!is.null(legend.title) && is.list(legend.title)) legend.title <- unlist(legend.title)
  # --------------------------------------------------------
  # plot moderation effeczs?
  # --------------------------------------------------------
  if (type == "eff") {
    return(sjp.eff.int(fit, int.term, int.plot.index, mdrt.values, swap.pred, plevel,
                       title, geom.colors, geom.size, axis.title,
                       legend.title, legend.labels, show.values, wrap.title, wrap.legend.labels,
                       wrap.legend.title, xlim, ylim, y.offset, grid.breaks,
                       show.ci, jitter.ci, p.kr, facet.grid, prnt.plot, fun, ...))
  }
  # -----------------------------------------------------------
  # set axis title
  # -----------------------------------------------------------
  y_title <- NULL
  if ((fun == "glm" || fun == "glmer")) {
    if (binom_fam)
      y_title <- "Change in Predicted Probability"
    else
      y_title <- "Change in Incidents Rates"
  }
  # -----------------------------------------------------------
  # get all (significant) interaction terms from model
  # the function "getInteractionTerms" checks if a fitted
  # model contains any interaction terms that are significant
  # at the level specified by "plevel". returns NULL, if model
  # contains no interaction terms or no significant interaction term.
  # else, information on model and interaction terms is returned
  # -----------------------------------------------------------
  git <- getInteractionTerms(fit, fun, plevel, p.kr)
  # check return value
  if (is.null(git)) return(invisible(NULL))
  # -----------------------------------------------------------
  # init variables from return values
  # -----------------------------------------------------------
  # b0 <- git[["b0"]]
  estimates.names <- git[["estimates.names"]]
  estimates <- git[["estimates"]]
  fitdat <- git[["fitdat"]]
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # when we have linear mixed effects models and both interaction
  # terms are factors, we may have the same interaction term names
  # multiples times - thus, remove redundant duplicates
  # -----------------------------------------------------------
  intnames <- unique(git[["intnames"]])
  # check if we have selected plots only, and remove any plots
  # that should not be plotted. but be careful for out of bound index!
  if (!is.null(int.plot.index) && !any(int.plot.index > length(intnames))) intnames <- intnames[int.plot.index]
  # -----------------------------------------------------------
  # get model frame, needed later for label detection
  # -----------------------------------------------------------
  modfram <- stats::model.frame(fit)
  # -----------------------------------------------------------
  # Now iterate all significant interaction terms
  # and manually calculate the linear regression by inserting
  # the estimates of each term and the associated interaction term,
  # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
  # -----------------------------------------------------------
  for (cnt in seq_len(length(intnames))) {
    # -----------------------------------------------------------
    # first, retrieve and split interaction term so we know
    # the two predictor variables of the interaction term
    # -----------------------------------------------------------
    interactionterms <- unlist(strsplit(intnames[cnt], ":"))
    labx <- c()
    # Label on y-axis is name of dependent variable
    laby <- paste0("Change in ", sjlabelled::get_label(modfram[[git[["depvar.label"]]]],
                                                   def.value = git[["depvar.label"]]))
    # -----------------------------------------------------------
    # find estimates (beta values) for each single predictor of
    # the interaction as well as of the interaction term
    # -----------------------------------------------------------
    b1 <- as.numeric(estimates[match(interactionterms[1], estimates.names)])
    b2 <- as.numeric(estimates[match(interactionterms[2], estimates.names)])
    b3 <- as.numeric(estimates[match(intnames[cnt], estimates.names)])
    # -----------------------------------------------------------
    # check whether each predictor was included in the model
    # as single term as well
    # -----------------------------------------------------------
    if (is.na(b1) || is.na(b2) || is.na(b3)) {
      stop("Predictors of interaction terms (main effects) must be included as single term as well. See Note in ?sjp.int", call. = FALSE)
    }
    # -----------------------------------------------------------
    # retrieve number of unique values in each predictor variable.
    # depending on the amount of values the variable for the x-axis
    # is chosen. In this case, we use the predictor with the higher
    # number of unique values on the x-axis.
    # -----------------------------------------------------------
    # retrieve values as data frame
    df_pred1uniquevals <- unique(stats::na.omit(fitdat[, interactionterms[1]]))
    df_pred2uniquevals <- unique(stats::na.omit(fitdat[, interactionterms[2]]))
    # convert data frame to numeric vector
    pred1uniquevals <- pred2uniquevals <- as.numeric(c())
    pred1uniquevals <- sort(as.numeric(sapply(df_pred1uniquevals, as.numeric)))
    pred2uniquevals <- sort(as.numeric(sapply(df_pred2uniquevals, as.numeric)))
    # init data frame
    intdf <- c()
    # -----------------------------------------------------------
    # choose x-value according to higher number of unique values
    # choose minimum and maximum value from predictor that has
    # a "smaller range" (i.e. less unique values)
    # or swap predictors on axes if requested
    # -----------------------------------------------------------
    if (swap.pred) {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), F, T)
    } else {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), T, F)
    }
    # -----------------------------------------------------------
    # calculate regression line
    # -----------------------------------------------------------
    if (useFirstPredOnY) {
      labx <- sjlabelled::get_label(modfram[[interactionterms[1]]],
                                def.value = interactionterms[1])
      predy <- interactionterms[2]
      # -----------------------------------------------------------
      # define predictor and moderator values
      # -----------------------------------------------------------
      pred.value <- pred1uniquevals
      mod.value <- pred2uniquevals
      # -----------------------------------------------------------
      # define predictor beta
      # -----------------------------------------------------------
      b.pred <- b1
    } else {
      labx <- sjlabelled::get_label(modfram[[interactionterms[2]]],
                                def.value = interactionterms[2])
      predy <- interactionterms[1]
      # -----------------------------------------------------------
      # define predictor and moderator values
      # -----------------------------------------------------------
      pred.value <- pred2uniquevals
      mod.value <- pred1uniquevals
      # -----------------------------------------------------------
      # define predictor beta
      # -----------------------------------------------------------
      b.pred <- b2
    }
    # -----------------------------------------------------------
    # Check whether moderator value has enough unique values
    # for quartiles
    # -----------------------------------------------------------
    mdrt.values <- mv_check(mdrt.values, mod.value)
    # -----------------------------------------------------------
    # check which values of moderator should be plotted, i.e. if
    # lower/upper bound (min-max) or mean and standard-deviation
    # should be used as valus for the moderator.
    # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
    # -----------------------------------------------------------
    if (mdrt.values == "minmax") {
      mw <- NA
      ymin <- min(mod.value, na.rm = T)
      ymax <- max(mod.value, na.rm = T)
    } else if (mdrt.values == "meansd") {
      mw <- mean(mod.value, na.rm = T)
      ymin <- mw - stats::sd(mod.value, na.rm = T)
      ymax <- mw + stats::sd(mod.value, na.rm = T)
    } else if (mdrt.values == "zeromax") {
      mw <- NA
      ymin <- 0
      ymax <- max(mod.value, na.rm = T)
    } else if (mdrt.values == "quart") {
      qu <- as.vector(stats::quantile(mod.value, na.rm = T))
      mw <- qu[3]
      ymin <- qu[2]
      ymax <- qu[4]
    }
    # -----------------------------------------------------------
    # Create data frame for plotting the interactions by
    # manually calculating the linear regression by inserting
    # the estimates of each term and the associated interaction term,
    # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
    # -----------------------------------------------------------
    # We now calculate the conditional effect of predictor 1 under absence
    # (or lowest impact) of predictor 2 on the dependent variable. Thus,
    # the slope for predictor 2 is not calculated. see
    # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
    # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
    # ------------------------------
    miny <- (b.pred * pred.value) + (b3 * pred.value * ymin)
    # ------------------------------
    # here we calculate the conditional effect of predictor 1 under presence
    # (or strongest impact) of predictor 2 on the dependent variable. Thus,
    # the slope for predictor 2 only is not needed. see references above
    # ------------------------------
    maxy <- (b.pred * pred.value) + (b3 * pred.value * ymax)
    # store in df
    tmp <- data.frame(x = pred.value, y = miny, ymin = miny, ymax = maxy, grp = "min")
    intdf <- as.data.frame(rbind(intdf, tmp))
    # store in df
    tmp <- data.frame(x = pred.value, y = maxy, ymin = miny, ymax = maxy, grp = "max")
    intdf <- as.data.frame(rbind(intdf, tmp))
    # store in df
    if (mdrt.values == "meansd" || mdrt.values == "quart") {
      # ------------------------------
      # here we calculate the effect of predictor 1 under presence
      # of mean of predictor 2 on the dependent variable. Thus, the slope for
      # predictor 2 only is not needed. see references above
      # ------------------------------
      mittelwert <- (b.pred * pred.value) + (b3 * pred.value * mw)
      tmp <- data.frame(x = pred.value, y = mittelwert, ymin = miny, ymax = maxy, grp = "mean")
      intdf <- as.data.frame(rbind(intdf, tmp))
    }
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    if (fun == "lm" || fun == "lmer" || fun == "lme") {
      intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
      intdf$y <- sjmisc::to_value(intdf$y, keep.labels = F)
      intdf$ymin <- sjmisc::to_value(intdf$ymin, keep.labels = F)
      intdf$ymax <- sjmisc::to_value(intdf$ymax, keep.labels = F)
      intdf$ydiff <- intdf$ymax - intdf$ymin
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(ylim)) {
        if (diff) {
          lowerLim.y <- floor(min(intdf$ydiff, na.rm = T))
          upperLim.y <- ceiling(max(intdf$ydiff, na.rm = T))
        } else {
          lowerLim.y <- floor(min(intdf$y, na.rm = T))
          upperLim.y <- ceiling(max(intdf$y, na.rm = T))
        }
      } else {
        lowerLim.y <- ylim[1]
        upperLim.y <- ylim[2]
      }
    } else {
      invlink <- stats::family(fit)
      intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
      intdf$y <- invlink$linkinv(eta = sjmisc::to_value(intdf$y, keep.labels = F))
      intdf$ymin <- invlink$linkinv(eta = sjmisc::to_value(intdf$ymin, keep.labels = F))
      intdf$ymax <- invlink$linkinv(eta = sjmisc::to_value(intdf$ymax, keep.labels = F))
      intdf$ydiff <- invlink$linkinv(eta = intdf$ymax - intdf$ymin)
    }
    # -----------------------------------------------------------
    # retrieve lowest and highest x and y position to determine
    # the scale limits
    # -----------------------------------------------------------
    if (is.null(ylim)) {
      if (binom_fam) {
        lowerLim.y <- as.integer(floor(10 * min(intdf$y, na.rm = T) * .9)) / 10
        upperLim.y <- as.integer(ceiling(10 * max(intdf$y, na.rm = T) * 1.1)) / 10
      } else {
        if (diff) {
          lowerLim.y <- floor(min(intdf$ydiff, na.rm = T))
          upperLim.y <- ceiling(max(intdf$ydiff, na.rm = T))
        } else {
          lowerLim.y <- floor(min(intdf$y, na.rm = T))
          upperLim.y <- ceiling(max(intdf$y, na.rm = T))
        }
      }
    } else {
      lowerLim.y <- ylim[1]
      upperLim.y <- ylim[2]
    }
    # -----------------------------------------------------------
    # check x-axis limits
    # -----------------------------------------------------------
    if (!is.null(xlim)) {
      lowerLim.x <- xlim[1]
      upperLim.x <- xlim[2]
    } else {
      lowerLim.x <- floor(min(intdf$x, na.rm = T))
      upperLim.x <- ceiling(max(intdf$x, na.rm = T))
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(grid.breaks)) {
      gridbreaks.x <- seq(lowerLim.x, upperLim.x, by = grid.breaks)
      gridbreaks.y <- seq(lowerLim.y, upperLim.y, by = grid.breaks)
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Conditional effect of ",
                         interactionterms[ifelse(isTRUE(useFirstPredOnY), 1, 2)],
                         " (by ",
                         interactionterms[ifelse(isTRUE(useFirstPredOnY), 2, 1)],
                         ") on ", git[["depvar.label"]])
    } else {
      # copy plot counter
      l_nr <- cnt
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(title)) l_nr <- length(title)
      # set legend labels for plot
      labtitle <- title[l_nr]
    }
    # -----------------------------------------------------------
    # get model frame, needed for label detection
    # -----------------------------------------------------------
    modfram <- stats::model.frame(fit)
    modfound <- modfram[[predy]]
    # -----------------------------------------------------------
    # legend labels
    # -----------------------------------------------------------
    if (is.null(legend.labels)) {
      # ---------------------------------
      # find moderator variable in data
      # ---------------------------------
      if (!is.null(modfound)) {
        lLabels <- sjlabelled::get_labels(modfound, attr.only = F)
      } else {
        lLabels <- NULL
      }
      # if we still have no labels, prepare generic labels
      if (is.null(lLabels)) {
        if (mdrt.values == "minmax") {
          lLabels <- c(paste0("lower bound of ", predy), paste0("upper bound of ", predy))
        } else if (mdrt.values == "meansd") {
          lLabels <- c(paste0("lower sd of ", predy), paste0("upper sd of ", predy), paste0("mean of ", predy))
        } else if (mdrt.values == "quart") {
          lLabels <- c(paste0("lower quartile of ", predy), paste0("upper quartile of ", predy), paste0("median of ", predy))
        } else {
          lLabels <- c(paste0("0 for ", predy), paste0("upper bound of ", predy))
        }
      } else {
        if (mdrt.values == "minmax") {
          lLabels <- lLabels[c(1, length(lLabels))]
        } else if (mdrt.values == "meansd") {
          lLabels <- c(paste0("lower sd of ", predy), paste0("upper sd of ", predy), paste0("mean of ", predy))
        } else if (mdrt.values == "quart") {
          lLabels <- c(paste0("lower quartile of ", predy), paste0("upper quartile of ", predy), paste0("median of ", predy))
        } else {
          lLabels <- c(paste0("0 for ", predy), lLabels[length(lLabels)])
        }
      }
    } else {
      # copy plot counter
      l_nr <- cnt
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(legend.labels)) l_nr <- length(legend.labels)
      # set legend labels for plot
      lLabels <- legend.labels[[l_nr]]
    }
    # -----------------------------------------------------------
    # legend titles
    # -----------------------------------------------------------
    if (is.null(legend.title)) {
      lTitle <- sjlabelled::get_label(modfound, def.value = predy)
    } else {
      # copy plot counter
      l_nr <- cnt
      # check if we have enough legend titles, if not, use last legend title
      if (l_nr > length(legend.title)) l_nr <- length(legend.title)
      # set legend title for plot
      lTitle <- legend.title[l_nr]
    }
    # -----------------------------------------------------------
    # x axis titles
    # -----------------------------------------------------------
    if (!is.null(axis.title)) {
      # copy plot counter
      l_nr <- cnt
      # check if we have enough axis titles, if not, use last legend title
      if (l_nr > length(axis.title)) l_nr <- length(axis.title)
      # set axis title
      labx <- axis.title[l_nr]
    }
    if (!is.null(y_title)) laby <- y_title
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    # wrap title
    labtitle <- sjmisc::word_wrap(labtitle, wrap.title)
    # wrap legend labels
    lLabels <- sjmisc::word_wrap(lLabels, wrap.legend.labels)
    # wrap legend title
    lTitle <- sjmisc::word_wrap(lTitle, wrap.legend.title)
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    if (diff) {
      baseplot <- ggplot(intdf, aes_string(x = "x", y = "ydiff")) +
        # -----------------------------------------------------------
      # add a shaded region between minimun
      # and maximum curve of interactions
      # -----------------------------------------------------------
      geom_ribbon(aes(ymin = 0, ymax = ydiff),
                  fill = fill.color,
                  alpha = fill.alpha) +
        geom_line(size = geom.size)
      # -----------------------------------------------------------
      # show value labels
      # -----------------------------------------------------------
      if (show.values) {
        baseplot <- baseplot +
          geom_text(aes(label = round(ydiff, 1)),
                    nudge_y = y.offset,
                    show.legend = FALSE)
      }
    } else {
      baseplot <- ggplot(intdf, aes_string(x = "x", y = "y", colour = "grp"))
      # the shaded area between line only if plots are not faceted
      if (!facet.grid) {
        baseplot <- baseplot +
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(ymin = ymin, ymax = ymax, colour = NULL),
                      fill = fill.color,
                      alpha = fill.alpha,
                      show.legend = FALSE)
      }
      # add line
      baseplot <- baseplot + geom_line()
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (show.values) {
        baseplot <- baseplot +
          geom_point() +
          geom_text(aes(label = round(y, 1)),
                    nudge_y = y.offset,
                    show.legend = FALSE)
      }
    }
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (diff) {
      col.len <- 1
      lLabels <- NULL
      lTitle <- NULL
    } else {
      if (mdrt.values == "minmax" || mdrt.values == "zeromax") {
        col.len <- 2
      } else {
        col.len <- 3
      }
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot +
      # set plot and axis titles
      labs(title = labtitle, x = labx, y = laby, colour = lTitle) +
      # set axis scale breaks
      scale_x_continuous(limits = c(lowerLim.x, upperLim.x), breaks = gridbreaks.x) +
      scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
    # ---------------------------------------------------------
    # facet grids?
    # ---------------------------------------------------------
    if (facet.grid && !diff) baseplot <- baseplot + facet_grid(~grp)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot, geom.colors, col.len, !is.null(lLabels), lLabels)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (prnt.plot) graphics::plot(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- baseplot
    dflist[[length(dflist) + 1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpint",
                      list(plot.list = plotlist,
                           data.list = dflist)))
}


#' @importFrom stats na.omit model.frame
#' @importFrom dplyr if_else
sjp.eff.int <- function(fit,
                        int.term = NULL,
                        int.plot.index = NULL,
                        mdrt.values = "minmax",
                        swap.pred = FALSE,
                        plevel = 0.05,
                        title = NULL,
                        geom.colors = "Set1",
                        geom.size = 0.7,
                        axis.title = NULL,
                        legend.title = NULL,
                        legend.labels = NULL,
                        show.values = FALSE,
                        wrap.title = 50,
                        wrap.legend.labels = 20,
                        wrap.legend.title = 20,
                        xlim = NULL,
                        ylim = NULL,
                        y.offset = 0.07,
                        grid.breaks = NULL,
                        show.ci = FALSE,
                        jitter.ci = FALSE,
                        p.kr = FALSE,
                        facet.grid = FALSE,
                        prnt.plot = TRUE,
                        fun,
                        ...) {
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) geom.size = .7
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package `effects` needed for this function to work. Please install it.", call. = FALSE)
  }
  # gridbreaks
  if (is.null(grid.breaks)) gridbreaks.x <- gridbreaks.y <- ggplot2::waiver()
  # init default
  binom_fam <- FALSE

  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(add.args) == "transformation")
  # if we have a "transformation" argument, and it's NULL,
  # no transformation of scale
  no.transform <- !sjmisc::is_empty(t.add) && is.null(eval(add.args[[t.add]]))
  # ---------------------------------------
  # get ...-arguments
  # ---------------------------------------
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)

  # ------------------------
  # calculate effects of higher order terms and
  # check if fitted model contains any interaction terms
  # allEffects returns a list, with all interaction effects
  # (or higher order terms) as separate list element. each list
  # element contains the higher-order-term of the fitted model,
  # where the 'term' attribute of interaction terms have a "*".
  # So we just need to look at each 'term' attribute of each
  # list element and see if there is a "*"...
  # ------------------------
  if (is.null(int.term)) {
    eff <- effects::allEffects(fit, KR = F)
    int <- unlist(lapply(eff, function(x) grep("*", x['term'], fixed = T)))
  } else {
    eff <- effects::effect(int.term, fit, KR = F)
    int <- grep("*", eff$term, fixed = T)
  }
  if (length(int) == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return(invisible(NULL))
  }
  # ------------------------
  # retrieve position of interaction terms in effects-object
  # ------------------------
  if (is.null(int.term)) {
    intpos <- which(as.vector(sapply(eff, function(x) sjmisc::str_contains(x['term'], "*"))))
  } else {
    intpos <- 1
  }
  # select only specific plots
  if (!is.null(int.plot.index) && !any(int.plot.index > length(intpos))) intpos <- intpos[int.plot.index]
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # iterate all interaction terms
  # -----------------------------------------------------------
  for (i in seq_len(length(intpos))) {
    # -----------------------------------------------------------
    # copy "eff" object, so we don't confuse with effect-return-
    # value from single term and multiple terms
    # -----------------------------------------------------------
    if (is.null(int.term)) {
      dummy.eff <- eff[[intpos[i]]]
    } else {
      dummy.eff <- eff
    }
    # -----------------------------------------------------------
    # retrieve data frame
    # -----------------------------------------------------------
    intdf <- data.frame(dummy.eff)
    # -----------------------------------------------------------
    # save response, predictor and moderator names
    # -----------------------------------------------------------
    pred_x.name <- colnames(intdf)[ifelse(isTRUE(swap.pred), 1, 2)]
    moderator.name <- colnames(intdf)[ifelse(isTRUE(swap.pred), 2, 1)]
    response.name <- dummy.eff$response
    # prepare axis titles
    labx <- sjlabelled::get_label(stats::model.frame(fit)[[pred_x.name]], def.value = pred_x.name)
    # check whether x-axis-predictor is a factor or not
    x_is_factor <- is.factor(intdf[[pred_x.name]]) || (length(unique(na.omit(intdf[[pred_x.name]]))) < 3)
    mod_is_factor <- is.factor(intdf[[moderator.name]])
    # -----------------------------------------------------------
    # check for moderator values, but only, if moderator
    # is no factor value. In this case, we can choose
    # the values for continuous moderator intentionally,
    # e.g. only min/max, or mean and sd. We don't need these
    # values for categorical moderator values.
    # -----------------------------------------------------------
    if (!mod_is_factor) {
      # retrieve moderator value
      modval <- dummy.eff$data[[moderator.name]]
      # retrieve predictor value
      predval <- dummy.eff$data[[pred_x.name]]
      # -----------------------------------------------------------
      # Check whether moderator value has enough unique values
      # for quartiles
      # -----------------------------------------------------------
      mdrt.values <- mv_check(mdrt.values, modval)
      # we have more than two values, so re-calculate effects, just using
      # min and max value of moderator.
      if (mdrt.values == "minmax" && length(unique(intdf[[moderator.name]])) > 2) {
        # retrieve min and max values
        mv.min <- min(modval, na.rm = T)
        mv.max <- max(modval, na.rm = T)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(mv.min, mv.max))
        # we have more than two values, so re-calculate effects, just using
        # 0 and max value of moderator.
      } else if (mdrt.values == "zeromax" && length(unique(intdf[[moderator.name]])) > 2) {
        # retrieve max values
        mv.max <- max(modval, na.rm = T)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(0, mv.max))
        # compute mean +/- sd
      } else if (mdrt.values == "meansd") {
        # retrieve mean and sd
        mv.mean <- round(mean(modval, na.rm = T), 2)
        mv.sd <- round(sd(modval, na.rm = T), 2)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(mv.mean - mv.sd, mv.mean, mv.mean + mv.sd))
      } else if (mdrt.values == "all") {
        # re-compute effects, prepare xlevels
        xl1 <- list(x = as.vector((unique(sort(modval, na.last = NA)))))
      } else if (mdrt.values == "quart") {
        # re-compute effects, prepare xlevels
        xl1 <- list(x = as.vector(stats::quantile(modval, na.rm = T)))
      }
      # change list name to moderator value name
      names(xl1) <- moderator.name
      # add values of interaction term
      # first, get all unqiue values
      prvl <- sort(unique(stats::na.omit(predval)))
      # add them to list as well
      xl2 <- list(y = prvl)
      # change list name
      names(xl2) <- pred_x.name
      # combine lists
      if (is.null(int.term)) {
        # re-compute effects
        eff.tmp <- effects::allEffects(fit, xlevels = c(xl1, xl2), KR = p.kr,
                                       confidence.level = dot.args[["ci.lvl"]], ...)
        # reset data frame
        intdf <- data.frame(eff.tmp[[intpos[i]]])
      } else {
        # re-compute effects
        eff.tmp <- effects::effect(int.term, fit, xlevels = c(xl1, xl2),
                                   KR = p.kr, confidence.level = dot.args[["ci.lvl"]], ...)
        # reset data frame
        intdf <- data.frame(eff.tmp)
      }
      # -----------------------------------------------------------
      # check for predictor values on x-axis. if it
      # is no factor, select whole range of possible
      # values.
      # -----------------------------------------------------------
    } else if (!x_is_factor) {
      # retrieve predictor value
      predval <- dummy.eff$data[[pred_x.name]]
      # add values of interaction term
      # first, get all unqiue values
      prvl <- sort(unique(stats::na.omit(predval)))
      # add them to list as well
      xl <- list(x = prvl)
      # change list name
      names(xl) <- pred_x.name
      # combine lists
      if (is.null(int.term)) {
        # re-compute effects
        eff.tmp <- effects::allEffects(
          fit,
          xlevels = xl,
          KR = p.kr,
          confidence.level = dot.args[["ci.lvl"]],
          ...
        )
        # reset data frame
        intdf <- data.frame(eff.tmp[[intpos[i]]])
      } else {
        # re-compute effects
        eff.tmp <-
          effects::effect(
            int.term,
            fit,
            xlevels = xl,
            KR = p.kr,
            confidence.level = dot.args[["ci.lvl"]],
            ...
          )
        # reset data frame
        intdf <- data.frame(eff.tmp)
      }
    }
    # -----------------------------------------------------------
    # change column names
    # -----------------------------------------------------------
    if (swap.pred) {
      colnames(intdf) <- c("x", "grp", "y", "se", "conf.low", "conf.high")
    } else {
      colnames(intdf) <- c("grp", "x", "y", "se", "conf.low", "conf.high")
    }
    # -----------------------------------------------------------
    # effects-package creates "NA" factor levels, which
    # need to be removed
    # -----------------------------------------------------------
    intdf <- droplevels(intdf)
    # group as factor
    intdf$grp <- factor(intdf$grp, levels = unique(as.character(intdf$grp)))
    # reset labels
    x_labels <- NULL
    lLabels <- NULL
    # does model have labels? we want these if x is a factor.
    # first we need to know whether we have a model-data-frame
    if (x_is_factor) {
      # do we have a factor with level-labels for "x"?
      # if yes, use these as labels
      if (!sjmisc::is_num_fac(intdf$x)) {
        x_labels <- levels(intdf$x)
      } else {
        x_labels <- sjlabelled::get_labels(stats::model.frame(fit)[[pred_x.name]], attr.only = F)
      }
    }
    # make sure x is numeric
    intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
    # get name of response, for axis title
    yaxisname <-
      sjlabelled::get_label(stats::model.frame(fit)[[response.name]], def.value = response.name)
    # -----------------------------------------------------------
    # check if we have linear regression
    # -----------------------------------------------------------
    if (fun == "lm" || fun == "lmer" || fun == "lme" || fun == "gls") {
      # Label on y-axis is name of dependent variable
      y_title <- sprintf("Predicted values of %s", yaxisname)
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(ylim)) {
        if (show.ci) {
          lowerLim.y <- floor(min(intdf$conf.low, na.rm = T))
          upperLim.y <- ceiling(max(intdf$conf.high, na.rm = T))
        } else {
          lowerLim.y <- floor(min(intdf$y, na.rm = T))
          upperLim.y <- ceiling(max(intdf$y, na.rm = T))
        }
      } else {
        lowerLim.y <- ylim[1]
        upperLim.y <- ylim[2]
      }
    } else {
      # ------------------------
      # do we have glm? if so, get link family. make exceptions
      # for specific models that don't have family function
      # ------------------------
      fitfam <- get_glm_family(fit)
      # --------------------------------------------------------
      # create logical for family
      # --------------------------------------------------------
      binom_fam <- fitfam$is_bin
      poisson_fam <- fitfam$is_pois
      # --------------------------------------------------------
      # Label on y-axis is fixed
      # --------------------------------------------------------
      # for logistic reg.
      if (binom_fam) {
        ysc <- dplyr::if_else(isTRUE(no.transform),
                              true = "log-odds",
                              false = "probabilities",
                              missing = "values")
      } else if (poisson_fam) {
        ysc <- dplyr::if_else(isTRUE(no.transform),
                              true = "log-mean",
                              false = "incidents",
                              missing = "values")
      } else {
        ysc <- "values"
      }
      y_title <- sprintf("Predicted %s for %s", ysc, yaxisname)
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(ylim)) {
        if (binom_fam) {
          if (show.ci) {
            lowerLim.y <- as.integer(floor(10 * min(intdf$conf.low, na.rm = T) * .9)) / 10
            upperLim.y <- as.integer(ceiling(10 * max(intdf$conf.high, na.rm = T) * 1.1)) / 10
          } else {
            lowerLim.y <- as.integer(floor(10 * min(intdf$y, na.rm = T) * .9)) / 10
            upperLim.y <- as.integer(ceiling(10 * max(intdf$y, na.rm = T) * 1.1)) / 10
          }
        } else {
          if (show.ci) {
            lowerLim.y <- floor(min(intdf$conf.low, na.rm = T))
            upperLim.y <- ceiling(max(intdf$conf.high, na.rm = T))
          } else {
            lowerLim.y <- floor(min(intdf$y, na.rm = T))
            upperLim.y <- ceiling(max(intdf$y, na.rm = T))
          }
        }
      } else {
        lowerLim.y <- ylim[1]
        upperLim.y <- ylim[2]
      }
    }
    # -----------------------------------------------------------
    # check x-axis limits
    # -----------------------------------------------------------
    if (!is.null(xlim)) {
      lowerLim.x <- xlim[1]
      upperLim.x <- xlim[2]
    } else {
      lowerLim.x <- floor(min(intdf$x, na.rm = T))
      upperLim.x <- ceiling(max(intdf$x, na.rm = T))
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(grid.breaks)) {
      gridbreaks.x <- seq(lowerLim.x, upperLim.x, by = grid.breaks)
      gridbreaks.y <- seq(lowerLim.y, upperLim.y, by = grid.breaks)
    } else if (x_is_factor) {
      gridbreaks.x <- sort(unique(intdf$x))
    } else {
      gridbreaks.x <- gridbreaks.y <- ggplot2::waiver()
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Interaction effect of ", moderator.name, " and ",
                         pred_x.name, " on ", response.name)
    } else {
      # copy plot counter
      l_nr <- i
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(title)) l_nr <- length(title)
      # set legend labels for plot
      labtitle <- title[l_nr]
    }
    # -----------------------------------------------------------
    # legend labels
    # -----------------------------------------------------------
    if (is.null(legend.labels)) {
      # try to get labels, but only for factors
      if (mod_is_factor) {
        lLabels <- sjlabelled::get_labels(stats::model.frame(fit)[[moderator.name]],
                                      attr.only = F)
      }
      # if we still have no labels, get values from group
      if (is.null(lLabels)) lLabels <- unique(as.character(intdf$grp))
    } else {
      # copy plot counter
      l_nr <- i
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(legend.labels)) l_nr <- length(legend.labels)
      # set legend labels for plot
      lLabels <- legend.labels[[l_nr]]
    }
    # -----------------------------------------------------------
    # prepare facet-labels
    # -----------------------------------------------------------
    if (length(unique(intdf$grp)) == length(lLabels) && isTRUE(facet.grid)) {
      levels(intdf$grp) <- lLabels
    }
    # -----------------------------------------------------------
    # legend titles
    # -----------------------------------------------------------
    if (is.null(legend.title)) {
      lTitle <- sjlabelled::get_label(stats::model.frame(fit)[[moderator.name]],
                                  def.value = moderator.name)
    } else {
      # copy plot counter
      l_nr <- i
      # check if we have enough legend titles, if not, use last legend title
      if (l_nr > length(legend.title)) l_nr <- length(legend.title)
      # set legend title for plot
      lTitle <- legend.title[l_nr]
    }
    # -----------------------------------------------------------
    # x axis titles
    # -----------------------------------------------------------
    if (!is.null(axis.title)) {
      # copy plot counter
      l_nr <- i
      # check if we have enough axis titles, if not, use last legend title
      if (l_nr > length(axis.title)) l_nr <- length(axis.title)
      # set axis title
      labx <- axis.title[l_nr]
    }
    # y-axis title.
    laby <- y_title
    # -----------------------------------------------------------
    # wrap titles
    # -----------------------------------------------------------
    labtitle <- sjmisc::word_wrap(labtitle, wrap.title)
    labx <- sjmisc::word_wrap(labx, wrap.title)
    laby <- sjmisc::word_wrap(laby, wrap.title)
    # wrap legend labels
    lLabels <- sjmisc::word_wrap(lLabels, wrap.legend.labels)
    # wrap legend title
    lTitle <- sjmisc::word_wrap(lTitle, wrap.legend.title)
    # ------------------------------------------------------------
    # start plot
    # ------------------------------------------------------------
    baseplot <- ggplot(intdf, aes_string(x = "x", y = "y", colour = "grp", linetype = "grp"))
    # ------------------------------------------------------------
    # confidence interval?
    # ------------------------------------------------------------
    if (show.ci) {
      # -------------------------------------------------
      # for factors, we add error bars instead of
      # continuous confidence region
      # -------------------------------------------------
      if (x_is_factor) {
        # -------------------------------------------------
        # check if to add jittering
        # -------------------------------------------------
        if (jitter.ci) {
          baseplot <- baseplot +
            geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high", colour = "grp", linetype = "grp"),
                          width = dot.args[["eb.width"]], show.legend = FALSE,
                          position = position_dodge(.2)) +
            geom_point(position = position_dodge(.2), shape = 16) +
            geom_line(size = geom.size, position = position_dodge(.2))
          # adjust axis limits, so jittered geoms are within plot boundaries
          lowerLim.x <- lowerLim.x - .2
          upperLim.x <- upperLim.x + .2
        } else {
          baseplot <- baseplot +
            geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high", colour = "grp", linetype = "grp"),
                          width = dot.args[["eb.width"]], show.legend = FALSE) +
            geom_point(shape = 16) +
            geom_line(size = geom.size)
        }
      } else {
        # -------------------------------------------------
        # for continuous variables, we add  continuous
        # confidence region instead of error bars
        # -------------------------------------------------
        baseplot <- baseplot +
          geom_ribbon(aes_string(ymin = "conf.low", ymax = "conf.high", colour = NULL, fill = "grp"),
                      alpha = dot.args[["ci.alpha"]], show.legend = FALSE) +
          geom_line(size = geom.size)
      }
    } else {
      baseplot <- baseplot + geom_line(size = geom.size)
    }
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (show.values) {
      # don't need geom_point, because point-layer already
      # added with x_is_factor
      if (!x_is_factor) baseplot <- baseplot + geom_point()
      # add value label text
      baseplot <- baseplot +
        geom_text(aes(label = round(y, 1)), nudge_y = y.offset, show.legend = FALSE)
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot +
      # set plot and axis titles
      labs(title = labtitle, x = labx, y = laby, colour = lTitle)
    # we have specified labels for factors on x-axis only...
    if (x_is_factor && !is.null(x_labels)) {
      baseplot <- baseplot +
        scale_x_continuous(limits = c(lowerLim.x, upperLim.x),
                           breaks = gridbreaks.x,
                           labels = x_labels)

    } else {
      # ...else, we use waiver-labels
      baseplot <- baseplot +
        scale_x_continuous(limits = c(lowerLim.x, upperLim.x),
                           breaks = gridbreaks.x)
    }
    # ------------------------
    # for logistic regression, use
    # 0 to 1 scale limits and percentage scale
    # ------------------------
    if (binom_fam && !no.transform) {
      baseplot <- baseplot +
        scale_y_continuous(limits = c(lowerLim.y, upperLim.y),
                           breaks = gridbreaks.y,
                           labels = scales::percent)
    } else {
      baseplot <- baseplot +
        # set axis scale breaks
        scale_y_continuous(limits = c(lowerLim.y, upperLim.y),
                           breaks = gridbreaks.y)
    }
    # ---------------------------------------------------------
    # facet grids?
    # ---------------------------------------------------------
    if (facet.grid) baseplot <- baseplot + facet_grid(~grp)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot,
                                 geom.colors,
                                 pal.len = length(unique(stats::na.omit(intdf$grp))),
                                 show.legend = !is.null(lLabels) & !facet.grid,
                                 lLabels)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (prnt.plot) graphics::plot(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- baseplot
    dflist[[length(dflist) + 1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjPlot", "sjpint"),
                      list(plot.list = plotlist,
                           data.list = dflist)))
}


#' @importFrom stats quantile
mv_check <- function(mdrt.values, x) {
  mvc <- length(unique(as.vector(stats::quantile(x, na.rm = T))))
  if (mdrt.values == "quart" && mvc < 3) {
    # tell user that quart won't work
    message("Could not compute quartiles, too small range of moderator variable. Defaulting `mdrt.values` to `minmax`.")
    mdrt.values <- "minmax"
  }
  return(mdrt.values)
}


# get all (significant) interaction terms from model
# the function "getInteractionTerms" checks if a fitted
# model contains any interaction terms that are significant
# at the level specified by "plevel". returns NULL, if model
# contains no interaction terms or no significant interaction term.
# else, information on model and interaction terms is returned
#' @importFrom stats model.matrix model.frame
getInteractionTerms <- function(fit, fun, plevel, p.kr) {
  # -----------------------------------------------------------
  # retrieve coefficients
  # -----------------------------------------------------------
  coef.tab <- summary(fit)$coefficients
  pval <- rep(0, times = nrow(coef.tab) - 1)
  # -----------------------------------------------------------
  # Help-function that removes AsIS I from formulas.
  # If someone know regular expressions better than me,
  # please provide a one-liner solution for the 3 sub commands.
  # -----------------------------------------------------------
  remove_I <- function(xnames) {
    fpos <- grep("I(", xnames, fixed = T)
    if (length(fpos) > 0 && fpos > 0) {
      xnames <- sub("I(", "", xnames, fixed = T)
      xnames <- sub(")", "", xnames, fixed = T)
      xnames <- sub(" * ", ":", xnames, fixed = T)
    }
    return(xnames)
  }
  # -----------------------------------------------------------
  # prepare values for (generalized) linear models
  # -----------------------------------------------------------
  if (fun == "lm" || fun == "glm" || fun == "plm" || fun == "lme" || fun == "gls") {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    if (fun == "plm") {
      # plm objects have different structure than (g)lm
      depvar.label <- attr(attr(attr(fit$model, "terms"), "dataClasses"), "names")[1]
      # retrieve model matrix
      fitdat <- data.frame(cbind(as.vector(fit$model[, 1]), stats::model.matrix(fit)))
    } else {
      depvar.label <- colnames(stats::model.frame(fit))[1]
      # retrieve model matrix
      fitdat <- data.frame(stats::model.matrix(fit))
    }
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    if (ncol(coef.tab) > 3) pval <- coef.tab[-1, 4]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- coef.tab[-1, 1]
    # -----------------------------------------------------------
    # need to remove "I(...)"?
    # -----------------------------------------------------------
    estimates.names <- names(estimates)
    estimates.names <- remove_I(estimates.names)
    it <- estimates.names
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- coef.tab[1, 1]
    # -----------------------------------------------------------
    # prepare values for (generalized) linear mixed effecrs models
    # -----------------------------------------------------------
  } else if (fun == "lmer" || fun == "glmer" || fun == "nlmer") {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    depvar.label <- colnames(stats::model.frame(fit))[1]
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    pval <- sjstats::get_model_pval(fit, p.kr)[["p.value"]][-1]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- unname(lme4::fixef(fit)[-1])
    estimates.names <- names(lme4::fixef(fit)[-1])
    # -----------------------------------------------------------
    # retrieve model matrix with all relevant predictors
    # -----------------------------------------------------------
    fitdat <- stats::model.matrix(fit)
    # -----------------------------------------------------------
    # need to remove "I(...)"?
    # -----------------------------------------------------------
    estimates.names <- remove_I(estimates.names)
    it <- estimates.names
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- unname(lme4::fixef(fit)[1])
  } else {
    stop("Unsupported model-class. This type of regression is not yet supported by 'sjp.int'.", call. = F)
  }
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # find interaction terms, which contains a colon, in row names
  firstit <- grep(":", it, fixed = TRUE)[1]
  # check whether we have any interaction terms included at all
  if (is.null(firstit) || is.na(firstit) || firstit == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return(invisible(NULL))
  }
  # save names of interaction predictor variables into this object
  intnames <- c()
  int.dropped <- c()
  non.p.dropped <- FALSE
  for (i in firstit:length(pval)) {
    if (pval[i] < plevel) {
      intnames <- c(intnames, it[i])
    } else {
      non.p.dropped <- T
      int.dropped <- c(int.dropped, it[i], "\n")
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) {
    warning("No significant interactions found... Try to adjust `plevel` argument.", call. = FALSE)
    return(invisible(NULL))
  } else if (non.p.dropped) {
    message(sprintf("Following non-significant interaction terms were omitted from the output:\n%s\nUse `plevel` to show more interaction terms.",
                    paste(int.dropped, collapse = "")))
  }
  return(list(intnames = intnames,
              estimates = estimates,
              estimates.names = estimates.names,
              b0 = b0,
              fitdat = fitdat,
              depvar.label = depvar.label))
}
