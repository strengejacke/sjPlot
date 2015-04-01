test_that("Check plot grouped frq", { 
  skip_on_cran()
  library(sjmisc)
  data(efc)
  sjp.grpfrq(efc$e42dep, efc$e16sex)
})