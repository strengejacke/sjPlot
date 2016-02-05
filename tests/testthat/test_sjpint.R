library(sjPlot)
fit <- lm(weight ~ Diet * Time, data = ChickWeight)

# show summary to see significant interactions
summary(fit)

# plot regression line of interaction terms
sjp.int(fit, type = "eff")
# plot regression line of interaction terms, including value labels
sjp.int(fit, type = "eff", showValueLabels = TRUE)


# load sample data set
library(sjmisc)
data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(usage = efc$tot_sc_e,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   burden = efc$neg_c_7,
                   dependency = efc$e42dep)
# convert gender predictor to factor
mydf$sex <- relevel(factor(mydf$sex), ref = "2")
# fit "dummy" model
fit <- lm(usage ~ .*., data = mydf)
summary(fit)

# plot interactions
sjp.int(fit, type = "cond")

# plot interactions, using mean and sd as moderator
# values to calculate interaction effect
sjp.int(fit, type = "cond", moderatorValues = "meansd")

# use zero and maximum value of moderation effect
sjp.int(fit, type = "cond", moderatorValues = "zeromax")


library(sjmisc)
data(efc)
# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
# create data frame for fitted model
mydf <- data.frame(y = as.factor(y),
                   sex = as.factor(efc$c161sex),
                   barthel = as.numeric(efc$barthtot))
# fit model
fit <- glm(y ~ sex * barthel,
           data = mydf,
           family = binomial(link = "logit"))
# plot interaction, increase p-level sensivity
sjp.int(fit,
        type = "cond",
        legendLabels = get_labels(efc$c161sex),
        plevel = 0.1)

sjp.int(fit, type = "eff", showCI = T)
sjp.int(fit, type = "eff", showCI = T, facet.grid = T)  

library(sjmisc)
data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(burden = efc$neg_c_7,
                   sex = efc$c161sex,
                   education = efc$c172code)
# convert gender predictor to factor
mydf$sex <- factor(mydf$sex)
mydf$education <- factor(mydf$education)
# name factor levels and dependent variable
levels(mydf$sex) <- c("female", "male")
levels(mydf$education) <- c("low", "mid", "high")
mydf$burden <- set_label(mydf$burden, "care burden")
# fit "dummy" model
fit <- lm(burden ~ .*., data = mydf)
summary(fit)

# plot marginal means of interactions, no interaction found
sjp.int(fit, type = "emm")
# plot marginal means of interactions, including those with p-value up to 1
sjp.int(fit, type = "emm", plevel = 1)
# swap predictors
sjp.int(fit,
        type = "emm",
        plevel = 1,
        swapPredictors = TRUE)  

sjp.int(fit,
        type = "emm",
        plevel = 1,
        facet.grid = T,
        showCI = TRUE,
        swapPredictors = TRUE)  

mydf$barthel <- efc$barthtot
# re-fit model with continuous variable
fit <- lm(burden ~ .*., data = mydf)

# plot effects
sjp.int(fit, type = "eff", showCI = TRUE)

# plot effects, faceted
sjp.int(fit,
        type = "eff",
        int.plot.index = 3,
        showCI = TRUE,
        facet.grid = TRUE)


library(sjmisc)
data(efc)
fit <- lm(neg_c_7 ~ c12hour + barthtot + c12hour:barthtot, data = efc)
sjp.int(fit, type = "eff")
sjp.int(fit, type = "eff", moderatorValues = "zeromax")
sjp.int(fit, type = "eff", moderatorValues = "meansd", showCI = T)
sjp.int(fit, type = "eff", moderatorValues = "meansd", showCI = T, facet.grid = T)

sjp.int(fit, type = "eff", moderatorValues = "quart", showCI = T)
sjp.int(fit, type = "cond", moderatorValues = "quart", showCI = T)

sjp.int(fit, type = "cond", int.plot.index = 3, showCI = TRUE, facet.grid = TRUE)



# test mixed models
library(lme4)
library(sjmisc)
data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(usage = efc$tot_sc_e,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   burden = efc$neg_c_7,
                   dependency = efc$e42dep,
                   randomeff = rep(1:10,length.out = nrow(efc)))
# convert gender predictor to factor
mydf$sexf <- relevel(factor(mydf$sex), ref = "2")
mydf$sexb <- mydf$sex == 2
# fit "dummy" model
fit <- lme4::lmer(usage ~ sex*burden + (1|randomeff), data = mydf)
sjp.int(fit, type = "cond", plevel = 1)
sjp.int(fit, type = "eff")
fit <- lme4::lmer(usage ~ sexf*burden + (1|randomeff), data = mydf)
sjp.int(fit, type = "cond", plevel = 1)
sjp.int(fit, type = "eff")
# this one breaks
fit <- lme4::lmer(usage ~ sexb * burden + (1|randomeff), data = mydf)
sjp.int(fit, type = "cond", plevel = 1)
# message("Error incoming!")
# sjp.int(fit, type = "eff")
fit <- lm(usage ~ sexb * burden * education, data = mydf)
sjp.int(fit, type = "cond", plevel = 1)
# message("Error incoming!")
# sjp.int(fit, type = "eff")


library(sjmisc)
data(efc)
# create data frame with variables that should be included
# in the model
mydf <- data.frame(burden = efc$neg_c_7,
                   sex = efc$c161sex,
                   education = efc$c172code,
                   groups = efc$e15relat)
# convert gender predictor to factor
mydf$sex <- factor(mydf$sex)
mydf$education <- factor(mydf$education)
mydf$groups <- factor(mydf$groups)
# name factor levels and dependent variable
levels(mydf$sex) <- c("female", "male")
levels(mydf$education) <- c("low", "mid", "high")
mydf$burden <- set_label(mydf$burden, "care burden")
# fit "dummy" model
fit <- lme4::lmer(burden ~ sex + education + sex:education + (1|groups), data = mydf)
fit <- lme4::lmer(burden ~ education*sex + (1|groups), data = mydf)
summary(fit)

sjp.int(fit, type = "emm")
sjp.int(fit, type = "emm", plevel = 1)

library(lmerTest)
fit <- lmerTest::lmer(burden ~ sex + education + sex:education + (1|groups), data = mydf)
sjp.int(fit, type = "emm")
sjp.int(fit, type = "emm", plevel = 1)
sjp.int(fit, type = "emm", plevel = 1, showCI = T)
sjp.int(fit, type = "emm", plevel = 1, showCI = T, facet.grid = T)
