## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(sjPlot)
library(sjmisc)
library(dplyr)
data(efc)
# create data frame with COPE-index scale
mydf <- dplyr::select(efc, contains("cop"))

## ----eval=FALSE----------------------------------------------------------
#  sjt.itemanalysis(mydf)

## ------------------------------------------------------------------------
# Compute PCA on Cope-Index, and retrieve 
# factor indices for each COPE index variable
factor.groups <- sjt.pca(mydf, no.output = TRUE)$factor.index

## ----eval=FALSE----------------------------------------------------------
#  sjt.itemanalysis(mydf, factor.groups)

## ----eval=FALSE----------------------------------------------------------
#  sjt.itemanalysis(mydf, factor.groups,
#                   show.shapiro = TRUE, show.kurtosis = TRUE)

