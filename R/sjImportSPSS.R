#' @title Import SPSS dataset as data frame into R
#' @name read_spss
#' 
#' @description Import data from SPSS, including NA's, value and variable labels.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{view_spss}}
#'            }
#'          
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset.
#' @param autoAttachVarLabels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute.
#'          See \code{\link{set_var_labels}} for details.
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          SPSS (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @return A data frame containing the SPSS data. retrieve value labels with \code{\link{get_val_labels}}
#'   and variable labels with \code{\link{get_var_labels}}.
#'   
#' @note This is a wrapper function for \code{\link{read.spss}} of the
#'         \code{foreign} package, using convenient parameter default
#'         settings. This function attaches value and variable
#'         labels to the imported variables of the data frame. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' @importFrom foreign read.spss
#' @export
read_spss <- function(path, 
                     enc=NA, 
                     autoAttachVarLabels=FALSE,
                     atomic.to.fac=FALSE) {
  # import data as data frame
  data.spss <- suppressWarnings(foreign::read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc))
  # convert atomic values to factors
  if (atomic.to.fac) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- txtProgressBar(min = 0, 
                         max = ncol(data.spss), 
                         style = 3)
    # tell user...
    message("Converting atomic to factors. Please wait...\n")
    # iterate all columns
    for (i in 1:ncol(data.spss)) {
      # copy column to vector
      x <- data.spss[, i]
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(attr(x, "value.labels"))) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to 
        # factor, but need to capture labels attribute first
        labs <- attr(x, "value.labels")
        # to factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, "value.labels") <- labs
        # copy vector back to data frame
        data.spss[, i] <- x
      }
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  # auto attach labels
  if (autoAttachVarLabels) {
    message("Attaching variable labels. Please wait...\n")
    data.spss <- set_var_labels(data.spss, get_var_labels(data.spss))
  }
  # return data frame
  return(data.spss)
}


#' @title Write content of data frame to SPSS sav-file
#' @name write_spss
#' 
#' @description This function saves the content of a data frame to an SPSS sav-file.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{view_spss}}
#'            \item \code{\link{read_spss}}
#'            }
#'          
#' @note You don't need to take care whether variables have been imported with
#'         the \code{\link{read_spss}} function from this package or from \code{haven}
#'         or even the \code{foreign} package, or if you have imported SPSS data and
#'         created new variables. This function does all necessary data preparation
#'         to write a properly labelled SPSS sav file.
#' 
#' @param x data frame that should be saved as SPSS sav-file.
#' @param path file path to the SPSS dataset.
#'   
#' @export
write_spss <- function(x, path) {
  write_data(x, path, "spss")
}


write_data <- function(x, path, type = "spss") {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  pb <- txtProgressBar(min = 0, 
                       max = ncol(x), 
                       style = 3)
  # tell user...
  message(sprintf("Prepare writing %s file. Please wait...\n", type))
  # check if variables should be converted to factors
  for (i in 1:ncol(x)) {
    # haven labelled objects don't need conversion
    if (!is_labelled(x[[i]])) {
      # get variable value
      var.lab <- get_var_labels(x[[i]], "label")
      # convert variable to labelled factor, so it can be saved
      x[[i]] <- to_label(x[[i]])
      # set back variable label
      x[[i]] <- set_var_labels(x, var.lab, "label")
    }
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  if (type == "spss") {
    haven::write_sav(x, path)
  }
  close(pb)
}


is_labelled <- function(x) {
  return (class(x) == "labelled")
}


#' @title Convert a haven-imported data frame to sjPlot format
#' @name to_sjPlot
#' 
#' @description This function converts a data frame, which was imported with any of
#'                \code{haven}'s read functions and contains \code{labelled} class
#'                vectors, into an sjPlot friendly data frame format.
#' 
#' @param x a data frame, which contains \code{labelled} class vectors
#' @return a data frame with 'sjPlot' friendly vector classes and attached
#'           label attributes.
#' 
#' @export
to_sjPlot <- function(x) {
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  pb <- txtProgressBar(min = 0, 
                       max = ncol(x), 
                       style = 3)
  # tell user...
  message("Cconverting from haven to sjPlot. Please wait...\n")
  for (i in 1:ncol(x)) {
    # haven labelled vector?
    if (is_labelled(x[[i]])) {
      # read current labels
      var.lab <- attr(x[[i]], "label")
      val.lab <- unname(attr(x[[i]], "labels"))
      val.lab.names <- names(attr(x[[i]], "labels"))
      # delete old attributes
      attr(x[[i]], "label") <- NULL
      names(attr(x[[i]], "labels")) <- NULL
      attr(x[[i]], "labels") <- NULL
      # set back labels
      x[[i]] <- set_val_labels(x[[i]], val.lab.names)
      x[[i]] <- set_var_labels(x[[i]], var.lab)
      # remove labelled class attribute
      class(x[[i]]) <- NULL
    }
    else {
      # read current labels
      var.lab <- attr(x[[i]], "label")
      # do we have any?
      if (!is.null(var.lab)) {
        # delete old attributes
        attr(x[[i]], "label") <- NULL
        # set back labels
        x[[i]] <- set_var_labels(x[[i]], var.lab)
      }
      # read current labels
      val.lab <- unname(attr(x[[i]], "labels"))
      if (!is.null(val.lab)) {
        val.lab.names <- names(attr(x[[i]], "labels"))
        # delete old attributes
        names(attr(x[[i]], "labels")) <- NULL
        attr(x[[i]], "labels") <- NULL
        # set back labels
        x[[i]] <- set_val_labels(x[[i]], val.lab.names)
      }
    }
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  # remove redundant class attributes
  class(x) <- "data.frame"
  return (x)
}


#' @title Retrieve value labels of a variable or an SPSS-imported data frame
#' @name get_val_labels
#' @description This function retrieves the value labels of an imported
#'                SPSS data set (via \code{\link{read_spss}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame, returns the all variable's value labels as \code{\link{list}} object
#'                  \item or, if \code{x} is a vector, returns the label as string.
#'                  }
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{set_val_labels}}
#'            }
#'
#' @param x a data frame with variables that have attached value labels (e.g.
#'          from an imported SPSS data (see \code{\link{read_spss}})) or a variable
#'          (vector) with attached value labels.
#' @return Either a list with all value labels from the data frame's variables,
#'           or a string with the value labels, if \code{x} is a variable.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#'        
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' data(efc)
#' get_val_labels(efc$e42dep)
#' 
#' @export
get_val_labels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    a <- lapply(x, FUN = sji.getValueLabel)
  }
  else {
    a <- sji.getValueLabel(x)
  }
  
  return (a)
}
sji.getValueLabel <- function(x) {
  labels <- NULL
  attr.string <- "value.labels"
  # retrieve named labels
  lab <- attr(x, "value.labels")
  # check if we have anything
  if (is.null(lab)) {
    # perhaps haven import?
    lab <- attr(x, "labels")
    attr.string <- "labels"
  }
  if (!is.null(lab)) {
    # retrieve order of value labels
    reihenfolge <- sji.getValueLabelValues(x, attr.string)
    # retrieve label values in correct order
    labels <- names(lab)[reihenfolge]
  }
  # return them
  return (labels)
}
sji.getValueLabelValues <- function(x, attr.string = "value.labels") {
  if (is.null(attr.string)) attr.string <- "value.labels"
  return (order(as.numeric(unname(attr(x, attr.string)))))
}


#' @title Attach value labels to a variable or vector
#' @name set_val_labels
#' @description This function attaches character labels as \code{"value.labels"} attribute
#'                to a variable or vector \code{"x"}, resp. to all variables of a data frame
#'                if \code{"x"} is a \code{\link{data.frame}}. These value labels will be accessed
#'                by most of this package's functions, in order to automatically set values
#'                or legend labels.
#'                
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{get_val_labels}}
#'            }
#' 
#' @param x a variable (vector) or a data frame where labels should be attached. Replaces former value labels.
#' @param labels a character vector of labels that will be attached to \code{"x"} by setting
#'          the \code{"value.labels"} attribute. The length of this character vector must equal
#'          the value range of \code{"x"}, i.e. if \code{"x"} has values from 1 to 3,
#'          \code{"labels"} should have a length of 3. 
#'          If \code{"x"} is a data frame, \code{labels} may also be a \code{\link{list}} of
#'          character vectors. If \code{labels} is a list, it must have the same length as
#'          number of columns of \code{x}. If \code{labels} is a vector and \code{x} is a data frame,
#'          the \code{labels} will be applied to each column of \code{x}.
#' @return \code{"x"} with attached value labels.
#' 
#' @note With attached value and variable labels, most functions of this package
#'       automatically detect labels and uses them as axis, legend or title labels
#'       in plots (\code{sjp.}-functions) respectively as column or row headers 
#'       in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'       and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'       label detection.
#'       
#' @examples
#' dummy <- sample(1:4, 40, replace=TRUE)
#' sjp.frq(dummy)
#' 
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)
#' 
#' @export
set_val_labels <- function(x, labels) {
  if (is.vector(x) || is.atomic(x)) {
    return (sji.setValueLabel.vector(x, labels))
  }
  else if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if (is.list(labels)) {
        x[,i] <- sji.setValueLabel.vector(x[,i], labels[[i]])
      }
      else if (is.vector(labels)) {
        x[,i] <- sji.setValueLabel.vector(x[,i], labels)
      }
      else {
        warning("'labels' must be a list of same length as 'ncol(x)' or a vector.", call. = F)
      }
    }
    return (x)
  }
}
sji.setValueLabel.vector <- function(var, labels) {
  # check for null
  if (!is.null(labels)) {
    if (is.null(var) || is.character(var)) {
      warning("Can't attach labels to string or NULL vectors.\n")
    }
    else {
      # check if var is a factor
      if (is.factor(var)) {
        # retrieve levels
        minval <- 1
        maxval <- length(levels(var))
      }
      else {
        # retrieve values
        minval <- min(var, na.rm=TRUE)
        maxval <- max(var, na.rm=TRUE)
      }
      # check for unlisting
      if (is.list(labels)) {
        labels <- as.vector(unlist(labels))
      }
      lablen <- length(labels)
      valrange <- maxval-minval+1
      if (is.infinite(valrange)) {
        warning("Can't set value labels. Infinite value range.\n")
      }
      # check for valid length of labels
      else if (valrange<lablen) {
        message(sprintf("More labels than values of \"var\". Using first %i labels.\n", valrange))
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- labels[1:valrange]
      }
      else if (valrange>lablen) {
        warning("Can't set value labels. Value range of \"var\" is longer than length of \"labels\".\n")
      }
      else {
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- labels
      }
    }
  }
  return (var)
}


#' @title Retrieve variable labels of (an SPSS-imported) data frame or of a specific variable
#' @name get_var_labels
#' 
#' @description This function retrieves the variable labels of an imported
#'                SPSS data set (via \code{\link{read_spss}}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame, returns the all variable labels as \code{\link{list}} object
#'                  \item or, if \code{x} is a vector, returns the variable label as string.
#'                  }
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{set_val_labels}}
#'            }
#' 
#' @param x A data frame (containing imported SPSS data or with attached variable labels) or
#'          a vector with \code{"variable.label"} attribute.
#' @param attr.string The attribute string for the variable label. To ensure
#'          compatibility to the \code{foreign}-package, use the default string
#'          \code{"variable.label"}. If you want to save data with the \code{haven}
#'          package, use \code{attr.string = "label"}. There is a wrapper function
#'          \code{\link{write_spss}} to save SPSS files, so you don't need to take
#'          care of this.
#' 
#' @return A named char vector with all variable labels from the SPSS dataset,
#'           or a simple string vector with the variable label, if \code{x} is a variable.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' data(efc)
#' # sample data set has not attached variable labels to each vector
#' # so we have to do this first... use 'autoAttachVarLabels' in
#' # function 'read_spss' to automatically perform this step.
#' efc <- set_var_labels(efc, get_var_labels(efc))
#' 
#' # get variable lable
#' get_var_labels(efc$e42dep)
#' 
#' # alternative way
#' get_var_labels(efc)["e42dep"]
#' 
#' @export
get_var_labels <- function(x, attr.string = "variable.label") {
  # default string
  if (is.null(attr.string)) attr.string <- "variable.label"
  # do we have a df?
  if (is.data.frame(x) || is.matrix(x)) {
    # if yes, check if we have attached labels from
    # foreign import
    labels <- attr(x, "variable.labels")
    # if not, get labels from each single vector
    if (is.null(labels)) {
      # return value
      all.labels <- c()
      # iterate df
      for (i in 1:ncol(x)) {
        # get label
        label <- attr(x[[i]], "variable.label")
        # haven?
        if (is.null(label)) {
          # get label
          label <- attr(x[[i]], "label")
        }
        # any label?
        if (!is.null(label)) {
          all.labels <- c(all.labels, label)
        }
        else {
          all.labels <- c(all.labels, "")
        }
      }
      return (all.labels)
    }
    else {
      return(attr(x, "variable.labels"))
    }
  }
  else {
    return(attr(x, attr.string))
  }
}


#' @title Attach variable label(s) to a single variable or data frame
#' @name set_var_labels
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a data frame. To each variable, the
#'                attribute \code{"variable.label"} with the related variable
#'                name is attached. Most of this package's functions can automatically
#'                retrieve the variable name to use it as axis labels or plot title.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{set_val_labels}}
#'            }
#' 
#' @param x A single variable (vector) or data frame with variables.
#' @param lab If \code{x} is a vector (single variable), use a single character string with 
#'          the variable label for \code{x}. If \code{x} is a \code{\link{data.frame}}, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#' @param attr.string The attribute string for the variable label. To ensure
#'          compatibility to the \code{foreign}-package, use the default string
#'          \code{"variable.label"}. If you want to save data with the \code{haven}
#'          package, use \code{attr.string = "label"}. There is a wrapper function
#'          \code{\link{write_spss}} to save SPSS files, so you don't need to take
#'          care of this.
#' @return \code{x}, with attached \code{"variable.label"} attribute(s), which contains the
#'           variable name(s).
#' 
#' @note With attached value and variable labels, most functions of this package
#'       automatically detect labels and uses them as axis, legend or title labels
#'       in plots (\code{sjp.}-functions) respectively as column or row headers 
#'       in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'       and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'       label detection.
#'        
#' @examples
#' # sample data set, imported from SPSS. Variable labels are attached
#' # as attribute to the data frame (so variables currently don't have this attribute)
#' data(efc)
#' # get variable labels
#' variable.labels <- get_var_labels(efc)
#' # set variable labels as attribute to each single variable of data frame
#' efc <- set_var_labels(efc, variable.labels)
#' 
#' \dontrun{
#' sjt.frq(efc$e42dep)
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#' 
#' # ---------------------------------------------
#' # manually set value and variable labels
#' # ---------------------------------------------
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- set_var_labels(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if parameter "title" set to NULL.
#' sjp.frq(dummy, title=NULL)
#' 
#' @export
set_var_labels <- function(x, lab, attr.string = "variable.label") {
  if (is.null(attr.string)) attr.string <- "variable.label"
  if (!is.null(lab) && !is.null(x)) {
    if (is.data.frame(x)) {
      if (ncol(x)!=length(lab)) {
        message("Parameter \"lab\" must be of same length as numbers of columns in \"x\".")
      }
      else {
        # -------------------------------------
        # create progress bar
        # -------------------------------------
        pb <- txtProgressBar(min=0, max=ncol(x), style=3)
        for (i in 1:ncol(x)) {
          # set variable label
          attr(x[,i], attr.string) <- lab[i]
          # set names attribute. equals variable name
          names(attr(x[,i], attr.string)) <- colnames(x)[i]
          # update progress bar
          setTxtProgressBar(pb, i)
        }
        close(pb)
      }
      # attach also all labels to df
      attr(x, attr.string) <- lab
      # and name attribute
      names(attr(x, attr.string)) <- colnames(x)
    }
    else {
      attr(x, attr.string) <- lab
    }
  }
  return (x)
}


#' @title Converts variable into factor and replaces values with associated value labels
#' @name to_label
#' 
#' @description This function converts (replaces) variable values (also of factors) 
#'                with their associated value labels. Might be helpful for factor variables.
#'                For instance, if you have a Gender variable with 0/1 value, and associated
#'                labels are male/female, this function would convert all 0 to male and
#'                all 1 to female and returns the new variable as \code{\link{factor}}.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            }
#' 
#' @param x A variable of type \code{\link{numeric}}, \code{\link{atomic}}
#'          \code{\link{factor}} or \code{labelled} (see \code{haven} package)
#'          \emph{with associated value labels}
#'          (see \code{\link{set_val_labels}}).
#' @return A factor variable with the associated value labels as factor levels.
#' 
#' @note The \code{"value.labels"} attribute will be removed when converting
#'         variables to factors.
#' 
#' @examples
#' data(efc)
#' print(get_val_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#' 
#' print(get_val_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(to_label(efc$e42dep))
#' 
#' # structure of numeric values won't be changed
#' # by this function, it only applies to labelled vectors
#' # (typically categorical or factor variables)
#' print(str(efc$e17age))
#' print(str(to_label(efc$e17age)))
#' 
#' @export
to_label <- function(x) {
  # get value labels
  vl <- get_val_labels(x)
  # check if we have any labels, else
  # return variable "as is"
  if (!is.null(vl)) {
    # haven or sjPlot?
    if(is_labelled(x))
      attr.string <- "labels"
    else
      attr.string <- NULL
    # get associated values for value labels
    vn <- rev(sji.getValueLabelValues(x, attr.string))
    # replace values with labels
    for (i in 1:length(vl)) {
      x[x==vn[i]] <- vl[i]
    }
    # to factor
    x <- factor(x, levels = vl)
  }
  # return as factor
  return (x)
}


#' @title Convert variable into factor and keep value labels
#' @name to_fac
#' 
#' @description This function converts a variable into a factor, but keeps
#'                variable and value labels, if these are attached as attributes
#'                to the variale \code{var}. See examples.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_value}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            }
#' 
#' @param x A (numeric or atomic) variable.
#' @return A factor variable, including variable and value labels.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions.
#' 
#' @examples
#' \dontrun{
#' data(efc)
#' # normal factor conversion, loses value attributes
#' efc$e42dep <- as.factor(efc$e42dep)
#' sjt.frq(efc$e42dep)
#' 
#' data(efc)
#' # factor conversion, which keeps value attributes
#' efc$e42dep <- to_fac(efc$e42dep)
#' sjt.frq(efc$e42dep)}
#' 
#' @export
to_fac <- function(x) {
  # retrieve value labels
  lab <- get_val_labels(x)
  # retrieve variable labels
  varlab <- get_var_labels(x)
  # convert variable to factor
  x <- as.factor(x)
  # set back value labels
  x <- set_val_labels(x, lab)
  # set back variable labels
  x <- set_var_labels(x, varlab)
  return (x)
}


#' @title Converts factors to numeric variables
#' @name to_value
#' 
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to 
#' a numeric variable.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{write_spss}}
#'            }
#'            
#' @param x A (factor) variable.
#' @param startAt the starting index, i.e. the lowest numeric value of the variable's
#'          value range.
#' @param keep.labels logical, if \code{TRUE}, former factor levels will be attached as
#'          value labels. See \code{\link{set_val_labels}} for more details.
#' @return A numeric variable with values ranging from \code{startAt} to
#'           \code{startAt} + length of factor levels.
#' 
#' @examples
#' data(efc)
#' test <- to_label(efc$e42dep)
#' table(test)
#' 
#' table(to_value(test))
#' hist(to_value(test, 0))
#' 
#' # set lowest value of new variable
#' # to "5".
#' table(to_value(test, 5))
#' 
#' @export
to_value <- function(x, startAt = 1, keep.labels = TRUE) {
  # get amount of categories
  l <- length(levels(x))
  # determine highest category value
  end <- startAt+l-1
  # retrieve "value labels"
  labels <- levels(x)
  # replace labels with numeric values
  levels(x) <- c(startAt:end)
  # convert to numeric
  new_value <- as.numeric(as.character(x))
  # check if we should attach former labels as value labels
  if (keep.labels) new_value <- set_val_labels(new_value, labels)
  return (new_value)
}
