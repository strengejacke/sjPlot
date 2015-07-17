#' @title Plot grouped proportional tables
#' @name sjp.gpt
#' 
#' @description Plot proportional crosstables (contingency tables) of two variables as ggplot diagram.
#' 
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#'
#'
#' @import ggplot2
#' @import dplyr
#' @import sjmisc
#' @importFrom scales percent
#' @importFrom stats na.omit
#' @export
sjp.gpt <- function(x, y, groups, legendTitle, legendLabels) {
  # ------------------------------------
  # final data frae for plot
  # ------------------------------------
  newdf <- data.frame()
  group.p <- c()
  group.n <- c()
  # ------------------------------------
  # create data frame
  # ------------------------------------
  mydf <- data.frame(grp = sjmisc::to_value(groups, keep.labels = F),
                     x = sjmisc::to_factor(x),
                     dep = sjmisc::to_value(y, keep.labels = F))
  len.x <- length(unique(na.omit(x)))
  # ------------------------------------
  # create grouping variable
  # ------------------------------------
  groups <- sort(unique(na.omit(sjmisc::to_value(groups))))
  # ------------------------------------
  # iterate all groups
  # ------------------------------------
  for (cnt in groups) {
    # ------------------------------------
    # select cases from groups
    # ------------------------------------
    dummy <- dplyr::filter(mydf, grp == cnt)
    # ------------------------------------
    # 
    # ------------------------------------
    y <- prop.table(table(dummy$x, dummy$dep), margin = 1)[, 2]
    # -----------------
    # p-Werte f?r Gruppenunterschiede lo/hi income
    #---------------------
    pval <- chisq.test(table(dummy$x, dummy$dep))$p.value
    stern <- c("")
    if (pval < 0.001) {
      stern <- c("***")
    } else if (pval < 0.01) {
      stern <- c("**")
    } else if (pval < 0.05) {
      stern <- c("*")
    }
    group.p <- c(group.p, stern)
    group.n <- c(group.n, prettyNum(sum(table(dummy$x, dummy$dep)),
                                    big.mark = ",",
                                    scientific = F))
    newdf <- data.frame(rbind(newdf,
                              cbind(grp = cnt,
                                    x = 1:len.x,
                                    y = y)))
  }
  y <- prop.table(table(mydf$x, mydf$dep), margin = 1)[, 2]
  
  newdf <- data.frame(rbind(newdf,
                            cbind(grp = "Total",
                                  x = 1:len.x,
                                  y = y)))

  pval <- chisq.test(table(mydf$x, mydf$dep))$p.value
  stern <- c("")
  if (pval < 0.001) {
    stern <- c("***")
  } else if (pval < 0.01) {
    stern <- c("**")
  } else if (pval < 0.05) {
    stern <- c("*")
  }
  group.p <- c(group.p, stern)
  group.n <- c(group.n, prettyNum(sum(table(mydf$x, mydf$dep)),
                                  big.mark = ",",
                                  scientific = F))
  
  # ------------------------------------
  # country und inc sind kategoriale daten. wichtig
  # für die Achsendarstellung beim Plot
  # ------------------------------------
  newdf$grp <- sjmisc::to_factor(newdf$country)
  newdf$x <- sjmisc::to_facto(newdf$x)
  
  #Einf?gen anderer Labels f?r Legende??!!###############
  levels(newdf$x) <- legendLabels
  newdf$x <- set_label(newdf$x, legendTitle)
  # ------------------------------------
  # Hier die Größen für den Plot einstellen!!!
  # ------------------------------------
  symbolgroesse <- 4
  # ------------------------------------
  # plot starten. mit "aes" geben wir allgemeine
  # Merkmale für den Plot an, und welche Variablen
  # in unserem Datensatz diese Merkmale beschreiben.
  # der x-Achsenabschnitt soll durch die Ländergruppen
  # dargestellt werden. Auf der Y-Achse werden die Forgone-Anteile
  # gezeichnet. "Colour" und "shape" bedeutet, dass die gemalten
  # "geoms" (Objekte wie Punkte, Linien, was auch immer) sich
  # in ihrer Farbe (colour) und Form (shape) unterscheiden sollen.
  # Das Unterscheidungsmerkmal ist "inc", also die Einkommensgruppen.
  # Würde man shape und colour nicht definieren, wären alle
  # geoms einfarbig.
  # ------------------------------------
  sternnamen <- paste0(axisLabels, " (n=", group.n, ") ",  group.p)
  p <- ggplot(newdf, aes(x = rev(grp),     # country auf der x-achse abtragen
                         y = y,     # forgone ist metrisch (prozenanteil), kommt auf die y-achse
                         colour = x,    # "inc" ist gruppierungsvariable. Farben und
                         shape = x)) +  # Punkt-Form sollen sich nach inc-gruppen unterscheiden
    # Punkte malen
    geom_point(size = symbolgroesse, fill = "#f0f0f0") +
    # die Y-Achse soll keine Zahlen-, sondern Prozent
    # Werte als Labels haben
    scale_y_continuous(labels = percent, breaks = seq(0,0.35, 0.05)) +
    # Bei X-Achse Länder-ID durch Namen ersetzen
    scale_x_discrete(labels = rev(sternnamen)) +
    # farben für die Punkte
    scale_color_manual(values = c("#000000","#555555","#999999"), name = "Income tertiles") +
    scale_shape_manual(name = "Income tertiles", values = c(21, 22, 24)) +
    # Achsenbeschriftungen
    labs(x = axisTitle.x,
         y = axisTitle.y,
         title = title) +
    # Plot "drehen", sodass Länder links stehen
    coord_flip() +
    annotate("rect", xmin = 0.5,  xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.15)
  
  print(p)
}
