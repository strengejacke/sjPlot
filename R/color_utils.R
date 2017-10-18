#' @importFrom scales brewer_pal grey_pal
col_check2 <- function(geom.colors, collen) {
  # --------------------------------------------
  # check color argument
  # --------------------------------------------
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (is.sjplot.pal(geom.colors[1])) {
      geom.colors <- get_sjplot_colorpalette(geom.colors[1], collen)
    } else if (is.wes.pal(geom.colors[1])) {
      geom.colors <- get_wesanderson_colorpalette(geom.colors[1], collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(collen)
      # do we have correct amount of colours?
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # repeat color palette
      geom.colors <- rep(geom.colors, times = collen)
      # shorten to required length
      geom.colors <- geom.colors[1:collen]
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }

  geom.colors
}


# check whether a color value is indicating
# a color brewer palette
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  bp <- c(bp.seq, bp.div, bp.qul)
  pal %in% bp
}


is.sjplot.pal <- function(pal) {
  pal %in% c("aqua", "warm", "dust", "blambus", "random")
}


is.wes.pal <- function(pal) {
  pal %in% c("GrandBudapest", "Moonrise1", "Royal1", "Moonrise2", "Cavalcanti", "Royal2",
             "GrandBudapest2", "Moonrise3", "Chevalier", "Zissou", "FantasticFox",
             "Darjeeling", "Rushmore", "BottleRocket", "Darjeeling2")
}


get_wesanderson_colorpalette <- function(pal, len) {
  if (!requireNamespace("wesanderson", quietly = TRUE)) {
    warning("Package `wesanderson` required for this color palette.", call. = F)
    return(NULL)
  }

  wesanderson::wes_palette(name = pal, n = len)
}


get_sjplot_colorpalette <- function(pal, len) {
  col <- NULL

  if (pal == "random")
    pal <- sample(c("aqua", "warm", "dust", "blambus", "simply", "us"), size = 1)

  if (pal == "aqua")
    col <- c("#BAF5F3", "#46A9BE", "#8B7B88", "#BD7688", "#F2C29E", "#BAF5F3", "#46A9BE", "#8B7B88")
  else if (pal == "warm")
    col <- c("#F8EB85", "#F1B749", "#C45B46", "#664458", "#072835", "#F8EB85", "#F1B749", "#C45B46")
  else if (pal == "dust")
    col <- c("#AAAE9D", "#F8F7CF", "#F7B98B", "#7B5756", "#232126", "#AAAE9D", "#F8F7CF", "#F7B98B")
  else if (pal == "blambus")
    col <- c("#5D8191", "#F2DD26", "#494949", "#BD772D", "#E02E1F", "#5D8191", "#F2DD26", "#494949")
  else if (pal == "simply")
    col <- c("#CD423F", "#FCDA3B", "#0171D3", "#018F77", "#F5C6AC", "#CD423F", "#FCDA3B", "#0171D3")
  else if (pal == "us")
    col <- c("#004D80", "#376C8E", "#37848E", "#9BC2B6", "#B5D2C0", "#004D80", "#376C8E", "#37848E")

  if (len > length(col)) {
    warning("More colors requested than length of color palette.", call. = F)
    len <- length(col)
  }

  col[1:len]
}
