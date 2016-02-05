library(sjPlot)
library(sjmisc)
data(efc)

col_1 <- c(2,1,1,2)
col_2 <- c(2,1,1,2)
col_3 <- c(2,1,1,5)
col_4 <- c(2,1,3,2)
col_5 <- c(2,1,1,2)

df <- as.data.frame(cbind(col_1,col_2,col_3,col_4,col_5))

sjp.likert(df, cat.neutral = 5, catcount = 4)
sjp.likert(df, cat.neutral = 3, catcount = 4)

col_1 <- c(2,1,1,2)
col_2 <- c(2,1,1,2)
col_3 <- c(2,1,1,5)
col_4 <- c(2,1,2,2)
col_5 <- c(2,1,1,2)

df <- as.data.frame(cbind(col_1,col_2,col_3,col_4,col_5))

sjp.likert(df, cat.neutral = 5)
sjp.likert(df, cat.neutral = 5, catcount = 4)

col_1 <- c(2,1,1,2)
col_2 <- c(2,1,1,2)
col_3 <- c(2,1,1,2)
col_4 <- c(2,1,2,2)
col_5 <- c(2,1,1,2)

df <- as.data.frame(cbind(col_1,col_2,col_3,col_4,col_5))

sjp.likert(df)
sjp.likert(df, catcount = 4)

col_1 <- c(2,1,1,2)
col_2 <- c(2,1,3,2)
col_3 <- c(2,1,1,2)
col_4 <- c(2,1,2,2)
col_5 <- c(2,3,1,2)

df <- as.data.frame(cbind(col_1,col_2,col_3,col_4,col_5))

sjp.likert(df)
sjp.likert(df, catcount = 4)
