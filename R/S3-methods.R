#' @importFrom utils browseURL
#' @export
print.sjTable <- function(x, ...) {
  # check if we have filename specified
  if (!is.null(x$file)) {
    # write file
    write(x$knitr, file = x$file)
  } else {
    x$page.complete <- replace_umlauts(x$page.complete)
    # else open in viewer pane
    htmlFile <- tempfile(fileext = ".html")
    write(x$page.complete, file = htmlFile)
    # check whether we have RStudio Viewer
    viewer <- getOption("viewer")
    if (x$viewer && !is.null(viewer)) {
      viewer(htmlFile)
    } else {
      utils::browseURL(htmlFile)
    }
  }
}


#' @importFrom knitr knit_print asis_output
#' @export
knit_print.sjTable <-  function(x, ...) {
  out <- x$knitr
  out <- replace_umlauts(out)
  knitr::asis_output(out)
  # knitr::asis_output(input$knitr)
}


replace_umlauts <- function(x) {
  x <- gsub("\u00E4", "&auml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00F6", "&ouml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00FC", "&uuml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00C4", "&Auml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00D6", "&Ouml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00DC", "&Uuml;", x, fixed = TRUE, useBytes = FALSE)
  x <- gsub("\u00DF", "&szlig;", x, fixed = TRUE, useBytes = FALSE)
  # x <- gsub("ä", "&auml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("ö", "&ouml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("ü", "&uuml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("Ä", "&Auml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("Ö", "&Ouml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("Ü", "&Uuml;", x, fixed = TRUE, useBytes = TRUE)
  # x <- gsub("ß", "&szlig;", x, fixed = TRUE, useBytes = TRUE)

  x
}


# knitr method for grpmean() ----

#' @export
knit_print.sjt_grpmean <-  function(x, ...) {
  knitr::asis_output(pgrpmean(x, ...)$knitr)
}

#' @export
knit_print.sjt_grpmeans <-  function(x, ...) {
  knitr::asis_output(pgrpmeans(x, ...)$knitr)
}


# knitr method method for reliab_test() ----

#' @export
knit_print.sjt_reliab <-  function(x, ...) {
  knitr::asis_output(preliab(x, ...)$knitr)
}


# knitr method method for descr() ----

#' @export
knit_print.sjt_descr <-  function(x, ...) {
  knitr::asis_output(pdescr(x, ...)$knitr)
}

#' @export
knit_print.sjt_grpdescr <-  function(x, ...) {
  knitr::asis_output(pgdescr(x, ...)$knitr)
}


# knitr method method for equi_test() ----

#' @export
knit_print.sjt_descr <-  function(x, ...) {
  knitr::asis_output(pequi_test(x, ...)$knitr)
}


# knitr method for frq() ----

#' @export
knit_print.sjt_frq <-  function(x, ...) {
  knitr::asis_output(pfrq(x, ...)$knitr)
}


# knitr method for mwu() ----

#' @export
knit_print.sjt_mwu <-  function(x, ...) {
  knitr::asis_output(pmwu(x, ...)$knitr)
}


# HTMl table method for grpmean() ----

#' @export
print.sjt_grpmean <- function(x, ...) {
  print(pgrpmean(x, ...), ...)
}

#' @export
print.sjt_grpmeans <- function(x, ...) {
  print(pgrpmeans(x, ...), ...)
}


# HTMl table method for reliab_test() ----

#' @export
print.sjt_reliab <- function(x, ...) {
  print(preliab(x, ...), ...)
}


# HTMl table method for equi_test() ----

#' @export
print.sjt_equi_test <- function(x, ...) {
  print(pequi_test(x, ...), ...)
}


# HTMl table method for descr() ----

#' @export
print.sjt_descr <- function(x, ...) {
  print(pdescr(x, ...), ...)
}

#' @export
print.sjt_grpdescr <- function(x, ...) {
  print(pgdescr(x, ...), ...)
}


# HTMl table method for frq() ----

#' @export
print.sjt_frq <- function(x, ...) {
  print(pfrq(x, ...), ...)
}


# HTMl table method for mwu() ----

#' @export
print.sjt_mwu <- function(x, ...) {
  print(pmwu(x, ...), ...)
}



pgrpmean <- function(x, ...) {
  title <- sprintf(
    "Mean for %s by %s",
    attr(x, "dv.label", exact = TRUE),
    attr(x, "grp.label", exact = TRUE)
  )

  footnote <- sprintf(
    "Anova: R<sup>2</sup>=%.3f; adj.R<sup>2</sup>=%.3f; F=%.3f; p=%.3f",
    attr(x, "r2", exact = TRUE),
    attr(x, "adj.r2", exact = TRUE),
    attr(x, "fstat", exact = TRUE),
    attr(x, "p.value", exact = TRUE)
  )

  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  tab_df(
    x = x,
    title = title,
    footnote = footnote,
    col.header = NULL,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-top:1px solid; border-bottom: double;'
    ),
    file = file,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )
}


#' @importFrom purrr map_chr
pgrpmeans <- function(x, ...) {
  uv <- attr(x, "print", exact = TRUE) == "viewer"
  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  titles <- purrr::map_chr(x, ~ sprintf(
    "Mean for %s by %s<br><span class=\"subtitle\">grouped by %s</span>",
    attr(.x, "dv.label", exact = TRUE),
    attr(.x, "grp.label", exact = TRUE),
    gsub(pattern = "\n", replacement = "<br>", attr(.x, "group", exact = TRUE), fixed = T)
  ))

  footnotes <- purrr::map_chr(x, ~ sprintf(
    "Anova: R<sup>2</sup>=%.3f; adj.R<sup>2</sup>=%.3f; F=%.3f; p=%.3f",
    attr(.x, "r2", exact = TRUE),
    attr(.x, "adj.r2", exact = TRUE),
    attr(.x, "fstat", exact = TRUE),
    attr(.x, "p.value", exact = TRUE)
  ))


  tab_dfs(
    x = x,
    titles = titles,
    footnotes = footnotes,
    col.header = NULL,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-top:1px solid; border-bottom: double;'
    ),
    file = file,
    use.viewer = uv,
    ...
  )
}


pequi_test <- function(x, ...) {
  chead <- c(
    "Term",
    "H<sub>0</sub>",
    "% in ROPE",
    "HDI (95%)"
  )

  x$inside.rope <- sprintf("%.1f%%", x$inside.rope)
  x$hdi <- sprintf("%.2f &ndash; %.2f", x$hdi.low, x$hdi.high)

  x <- dplyr::select(x, c(1:3, 6))

  footnote <- sprintf(
    "Effect Size: %.2f &middot; ROPE: %.2f &ndash; %.2f &middot; Samples: %i",
    attr(x, "eff_size", exact = TRUE),
    attr(x, "rope", exact = TRUE)[1],
    attr(x, "rope", exact = TRUE)[2],
    attr(x, "nsamples", exact = TRUE)
  )

  if (isTRUE(attr(x, "critical"))) {
    footnote <- paste0(
      footnote,
      "<br>(*) number of effective samples may be insufficient for this parameter"
    )
  }


  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  tab_df(
    x = x,
    title = "Test for Practical Equivalence of Model Parameters",
    footnote = footnote,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;',
      css.col3 = '+text-align:right;'
    ),
    file = file,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )
}


preliab <- function(x, ...) {
  chead <- c(
    "Variable",
    "&alpha; if deleted",
    "Item Discrimination"
  )

  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  tab_df(
    x = x,
    title = "Reliability Test",
    footnote = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = FALSE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;'
    ),
    file = file,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )
}


#' @importFrom purrr map_if
#' @importFrom sjmisc is_float
pdescr <- function(x, ...) {
  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  uv <- attr(x, "print", exact = TRUE) == "viewer"
  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  chead <- c(
    "Variable",
    "Type",
    "Label",
    "N",
    "Missings (%)",
    "Mean",
    "SD",
    "SE",
    "Median",
    "Trimmed Mean",
    "Range",
    "Skewness"
  )

  present_columns <- c("var", "type", "label", "n", "NA.prc", "mean", "sd", "se", "md", "trimmed", "range", "skew")
  chead <- chead[which(present_columns %in% colnames(x))]

  x <- x %>%
    purrr::map_if(sjmisc::is_float, ~ round(.x, digits)) %>%
    as.data.frame()

  tab_df(
    x = x,
    title = "Basic descriptive statistics",
    footnote = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = TRUE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;',
      css.centeralign = 'text-align:right;',
      css.col2 = '+text-align:left;',
      css.col3 = '+text-align:left;'
    ),
    file = file,
    use.viewer = uv,
    ...
  )
}


#' @importFrom purrr map_if map_chr map
#' @importFrom sjmisc is_float
pgdescr <- function(x, ...) {
  titles <- purrr::map_chr(x, ~ sprintf(
    "Basic descriptives<br><span class=\"subtitle\"><em>grouped by</em> %s</span>",
    gsub(pattern = "\n", replacement = "<br>", attr(.x, "group", exact = TRUE), fixed = T)
  ))

  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  uv <- attr(x, "print", exact = TRUE) == "viewer"
  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"

  chead <- c(
    "Variable",
    "Type",
    "Label",
    "N",
    "Missings (%)",
    "Mean",
    "SD",
    "SE",
    "Median",
    "Trimmed Mean",
    "Range",
    "Skewness"
  )

  x <- x %>%
    purrr::map(~ purrr::map_if(
      .x,
      sjmisc::is_float,
      ~ round(.x, digits)
    ) %>% as.data.frame())

  tab_dfs(
    x = x,
    titles = titles,
    footnotes = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = TRUE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;',
      css.col3 = '+text-align:left;'
    ),
    file = file,
    use.viewer = uv,
    ...
  )
}


#' @importFrom purrr map_if map_chr map
#' @importFrom dplyr n_distinct select
#' @importFrom sjmisc is_empty
pfrq <- function(x, ...) {

  uv <- attr(x, "print", exact = TRUE) == "viewer"
  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"


  titles <- purrr::map_chr(x, function(i) {

    ret <- ""

    # get variable label
    lab <- attr(i, "label", exact = T)
    vt <- attr(i, "vartype", exact = T)

    # fix variable type string
    if (!sjmisc::is_empty(vt))
      vt <- sprintf(" <span style=\"font-weight: normal; font-style: italic\">&lt;%s&gt;</span>", vt)
    else
      vt <- ""

    if (!is.null(lab)) ret <- sprintf("%s%s", lab, vt)

    # get grouping title label
    grp <- attr(i, "group", exact = T)

    if (!is.null(grp))
      ret <- sprintf("%s<br><span class=\"subtitle\"><em>grouped by:</em><br>%s</span>", ret, grp)

    gsub(pattern = "\n", replacement = "<br>", x = ret, fixed = T)
  })


  footnotes <- purrr::map_chr(x, ~ sprintf(
    "total N=%i &middot; valid N=%i &middot; x&#772;=%.2f &middot; &sigma;=%.2f\n",
    sum(.x$frq, na.rm = TRUE),
    sum(.x$frq[1:(nrow(.x) - 1)], na.rm = TRUE),
    attr(.x, "mean", exact = T),
    attr(.x, "sd", exact = T)
  )
  )


  x <- purrr::map(x, function(i) {
    if (dplyr::n_distinct(i$label) == 1 && unique(i$label) == "<none>")
      i <- dplyr::select(i, -.data$label)
    i
  })


  tab_dfs(
    x = x,
    titles = titles,
    footnotes = footnotes,
    col.header = NULL,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;',
      css.col2 = 'text-align: left;',
      css.col3 = 'text-align: right;',
      css.col4 = 'text-align: right;',
      css.col5 = 'text-align: right;',
      css.col6 = 'text-align: right;'
    ),
    file = file,
    use.viewer = uv,
    ...
  )
}


#' @importFrom stats na.omit kruskal.test
pmwu <- function(x, ...) {
  fn <- NULL

  chead <- c(
    "Groups",
    "N",
    "Mean Rank",
    "Mann-Whitney U",
    "Wilcoxon W",
    "Z",
    "Effect Size",
    "p-value"
  )

  # if we have more than 2 groups, also perfom kruskal-wallis-test
  if (length(unique(stats::na.omit(x$data$grp))) > 2) {

    kw <- stats::kruskal.test(x$data$x, x$data$grp)

    if (kw$p.value < 0.001) {
      p  <- 0.001
      p.string <- "<"
    } else {
      p <- kw$p.value
      p.string <- "="
    }

    fn <-
      sprintf(
        "Kruskal-Wallis-Test: &chi;<sup>2</sup>=%.3f &middot; df=%i &middot; p%s%.3f",
        kw$statistic,
        kw$parameter,
        p.string,
        p
      )
  }

  enc <- attr(x, "encoding", exact = TRUE)
  file <- attr(x, "file", exact = TRUE)

  if (is.null(enc)) enc <- "UTF-8"


  tab_df(
    x = x$tab.df,
    title = "Mann-Whitney U-Test",
    footnote = fn,
    col.header = chead,
    show.rownames = FALSE,
    show.type = FALSE,
    show.footnote = !is.null(fn),
    alternate.rows = TRUE,
    file = file,
    encoding = enc,
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;'
    ),
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )
}
