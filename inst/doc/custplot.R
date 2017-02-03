## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
# load libraries
library(sjPlot)  # for plotting
library(sjmisc)  # for sample data
library(ggplot2) # to access ggplot-themes
# load sample data set
data(efc)
sjp.setTheme(geom.outline.color = "antiquewhite4", 
             geom.outline.size = 1, 
             geom.label.size = 2,
             geom.label.color = "grey50",
             title.color = "red", 
             title.size = 1.5, 
             axis.angle.x = 45, 
             axis.textcolor = "blue", 
             base = theme_bw())
sjp.grpfrq(efc$e42dep, 
           efc$e16sex, 
           title = NULL, 
           geom.colors = c("cadetblue", "coral"), 
           geom.size = 0.4)

## ----results='hide', warning=FALSE, message=FALSE, echo=FALSE------------
sjp.setTheme(geom.label.size = 3)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
# blank theme
set_theme(theme = "blank",
          axis.title.size = .9,
          axis.textsize = .9,
          legend.size = .7,
          legend.title.size = .8,
          geom.label.size = 3)
sjp.grpfrq(efc$e42dep, 
           efc$e15relat, 
           geom.colors = "PuRd", 
           show.values = FALSE)

## ---- fig.width=7, fig.height=10, warning=FALSE, message=FALSE-----------
library(RColorBrewer)
display.brewer.all()

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(geom.label.color = "white",
             geom.label.size = 3)
# labels appear very large due to export metrics
sjp.grpfrq(efc$e42dep, 
           efc$e16sex, 
           coord.flip = TRUE)

## ----results='hide', warning=FALSE, message=FALSE, echo=FALSE------------
sjp.setTheme(axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             legend.title.size = .8,
             geom.label.size = 3)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.grpfrq(efc$e42dep,
           efc$e16sex,
           expand.grid = TRUE)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(base = theme_light())
sjp.frq(efc$e42dep)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
set_theme("blank")
sjp.frq(efc$e42dep)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
set_theme("539")
sjp.frq(efc$e42dep)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
set_theme("scatter")
sjp.frq(efc$e42dep,
        geom.colors = "#2255aa")

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(base = theme_bw(),            # bw-base
             axis.linecolor = "darkgreen") # with green axes.
sjp.frq(efc$e42dep)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(
  base = theme_classic(),
  axis.tickslen = 0, # hides tick marks
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3.5
)
  
sjp.grpfrq(
  efc$e42dep,
  efc$e16sex,
  coord.flip = TRUE,
  show.axis.values = FALSE,
  prnt.plot = FALSE
)$plot +
  theme(axis.line.x = element_line(color = "white"))

## ----results='hide', warning=FALSE, message=FALSE, echo=FALSE------------
sjp.setTheme(base = theme_classic(),
             axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             legend.title.size = .8)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.grpfrq(efc$e42dep,
           efc$e16sex,
           ylim = c(0, 350),
           coord.flip = TRUE,
           show.axis.values = FALSE)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(base = theme_classic(), 
             legend.title.face = "italic", # title font face
             legend.inside = TRUE,         # legend inside plot
             legend.color = "grey50",      # legend label color
             legend.pos = "bottom right",  # legend position inside plot
             axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             legend.title.size = .8,
             geom.label.size = 3)
sjp.grpfrq(efc$e42dep, 
           efc$e16sex, 
           coord.flip = TRUE)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(base = theme_classic(), 
             axis.linecolor = "white",     # "remove" axis lines
             axis.textcolor.y = "darkred", # set axis label text only for y axis
             axis.tickslen = 0,            # "remove" tick marks
             legend.title.color = "red",   # legend title color
             legend.title.size = 2,        # legend title size
             legend.color = "green",       # legend label color
             legend.pos = "top",           # legend position above plot
             axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             geom.label.size = 3)
sjp.grpfrq(efc$e42dep, efc$e16sex)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(legend.inside = TRUE,    # legend inside plot
             legend.pos = c(.3, .7),  # relative legend position inside plot
             legend.just = c(.1, .8), # legend justification relative to position
             axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             legend.title.size = .8,
             geom.label.size = 3)
sjp.grpfrq(efc$e42dep, 
           efc$e16sex, 
           coord.flip = TRUE)

## ---- fig.width=7, fig.height=5, warning=FALSE, message=FALSE------------
sjp.setTheme(legend.inside = TRUE,    # legend inside plot
             legend.pos = c(.3, .7),  # relative legend position inside plot
             legend.just = c(.8, .1), # legend justification relative to position
             axis.title.size = .9,
             axis.textsize = .9,
             legend.size = .7,
             legend.title.size = .8,
             geom.label.size = 3)
sjp.grpfrq(efc$e42dep, 
           efc$e16sex, 
           coord.flip = TRUE)

