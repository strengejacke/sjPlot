library(sjPlot)
library(sjmisc)
data(efc)

sjt.xtab(efc$e42dep, efc$e16sex)

sjt.xtab(efc$e42dep, 
         efc$e16sex,
         efc$c172code)

sjt.xtab(efc$e42dep, 
         efc$e16sex,
         digits = 2,
         showCellPerc = T,
         showNA = T,
         showRowPerc = T,
         showColPerc = T,
         showExpected = T,
         useViewer = F)

sjt.xtab(efc$e42dep, 
         efc$e16sex,
         showHorizontalLine = T)
