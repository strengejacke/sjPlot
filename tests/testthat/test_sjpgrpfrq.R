library(sjPlot)
library(sjmisc)
data(efc)

sjp.grpfrq(efc$e42dep, efc$e16sex)

sjp.grpfrq(efc$c12hour, 
           efc$e16sex,
           interactionVar = efc$c172code,
           type = "v",
           innerBoxPlotWidth = .05,
           innerBoxPlotDotSize = 2)

sjp.grpfrq(efc$e17age, 
           efc$e16sex,
           type = "lines",
           smoothLines = T,
           showValueLabels = F)
