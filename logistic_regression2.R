## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

## all in one spot - COPY
NH11_sub <- NH11[c("region", "hispan_i", "mracrpi2", "age_p","r_maritl","sex","everwrk", "wkdayr")]
NH11_sub$everwrk <- factor(NH11_sub$everwrk, levels=c("1 Yes", "2 No"))
NH11_sub <- NH11_sub %>% mutate(ethnicity = if_else(hispan_i != "12 Not Hispanic/Spanish origin", "04 Hispanic", as.character(mracrpi2)),
                                ethnicity = factor(ethnicity, levels = unique(ethnicity))) %>% select(-hispan_i, -mracrpi2)
NH11_sub[which(is.na(NH11_sub$everwrk) & NH11_sub$wkdayr > 0), "everwrk"] <- "1 Yes"
  # mice imputation
  NH11_sub <- NH11_sub %>% select(-wkdayr)
  set.seed(1000)
  imputed <- complete(mice(NH11_sub))
  summary(NH11_sub)
  summary(imputed)
  NH11_sub$everwrk <- imputed$everwrk
NH11_sub <- NH11_sub %>% filter(r_maritl != "9 Unknown marital status")
NH11_sub$r_maritl <- factor(NH11_sub$r_maritl, levels = c("7 Never married", "8 Living with partner", "1 Married - spouse in household", "2 Married - spouse not in household"
                                                          , "6 Separated", "5 Divorced", "4 Widowed", "0 Under 14 Years", "3 Married - spouse in household unknown"))
levels(NH11_sub$r_maritl) <- c("1 Never married", "2 Living with partner", "3 Married - spouse in household", "4 Married - spouse not in household",
                               "5 Separated", "6 Divorced", "7 Widowed", "98 Under 14 Years", "99 Married - spouse in household unknown")
NH11_sub$r_maritl <- as.factor(as.character(NH11_sub$r_maritl))
NH11_sub <- NH11_sub %>% filter(age_p < 85) %>% mutate(age_p = factor(cut(age_p, breaks=6)))
levels(NH11_sub$everwrk) <- c(1,0)





  
  
  
library(dplyr)
str(NH11)
# take all demographic data
NH11_sub <- NH11[c("region", "hispan_i", "mracrpi2", "age_p","r_maritl","sex","everwrk", "wkdayr")]
summary(NH11_sub)

# collapse any everwrk values which are not yes or no
NH11_sub$everwrk <- factor(NH11_sub$everwrk, levels=c("1 Yes", "2 No"))

# impute everwrk where days missed from work
NH11_sub[which(is.na(NH11_sub$everwrk) & NH11_sub$wkdayr > 0), "everwrk"] <- "1 Yes"

# combine race/ethnicity in one factor, generalise Hispanic
NH11_sub <- NH11_sub %>% mutate(ethnicity = if_else(hispan_i != "12 Not Hispanic/Spanish origin", "04 Hispanic", as.character(mracrpi2)),
                                ethnicity = factor(ethnicity, levels = unique(ethnicity))) %>% select(-hispan_i, -mracrpi2)
                                
head(NH11_sub[is.na(NH11_sub$everwrk),], 20)






# impute remaining everwrk NAs using mice
library(mice)
NH11_sub <- NH11_sub %>% select(-wkdayr)
summary(NH11_sub)
set.seed(1000)
imputed <- complete(mice(NH11_sub))
summary(imputed)

NH11_sub$everwrk <- imputed$everwrk


# adjust marital status
NH11_sub %>% group_by(r_maritl) %>% tally
NH11_sub <- NH11_sub %>% filter(r_maritl != "9 Unknown marital status")
# relevel marital status
NH11_sub$r_maritl <- factor(NH11_sub$r_maritl, levels = c("7 Never married", "8 Living with partner", "1 Married - spouse in household", "2 Married - spouse not in household"
                                                          , "6 Separated", "5 Divorced", "4 Widowed", "0 Under 14 Years", "3 Married - spouse in household unknown"))
levels(NH11_sub$r_maritl) <- c("1 Never married", "2 Living with partner", "3 Married - spouse in household", "4 Married - spouse not in household",
                               "5 Separated", "6 Divorced", "7 Widowed", "98 Under 14 Years", "99 Married - spouse in household unknown")
NH11_sub$r_maritl <- as.factor(as.character(NH11_sub$r_maritl))
# check
# head(NH11_sub)
# head(as.numeric(NH11_sub$r_maritl))

# relevel everwrk
levels(NH11_sub$everwrk) <- c(1,0)


library(ggplot2)
ggplot(NH11_sub, aes(x=age_p)) + geom_histogram(binwidth = 1, col="white")
NH11_sub <- NH11_sub[NH11_sub$age_p < 85, ]







# Modelling

# split data 65% training, 35% test
library(caTools)
set.seed(1000)
split <- sample.split(NH11_sub$everwrk, SplitRatio = 0.65)
NH11_subTrain <- subset(NH11_sub, split==TRUE)
NH11_subTest <- subset(NH11_sub, split==FALSE)

# baseline model - assume everyone works
table(NH11_subTrain$r_maritl, NH11_subTrain$everwrk)
nrow(NH11_subTrain[NH11_subTrain$everwrk==1,])/nrow(NH11_subTrain)

# check for multicollinearity
NH11_subTrain_num <- sapply(NH11_subTrain, as.numeric)
cor(NH11_subTrain_num)

everwrkLog1 <- glm(everwrk ~ r_maritl + age_p, data = NH11_subTrain, family = binomial)
summary(everwrkLog1)

everwrkLog2 <- glm(everwrk ~ ., data = NH11_subTrain, family = binomial)
summary(everwrkLog2)


# check model accuracy using t=0.5
pred1 <- predict(everwrkLog1, type="response", newdata = NH11_subTest)
table(NH11_subTest$everwrk, pred1 >= 0.5)

pred2 <- predict(everwrkLog2, type="response", newdata = NH11_subTest)
table(NH11_subTest$everwrk, pred2 >= 0.5)

# check AUC
library(ROCR)
ROCR_pred1 <- prediction(pred1, NH11_subTest$everwrk)
as.numeric(performance(ROCR_pred1, "auc")@y.values)

ROCR_pred2 <- prediction(pred2, NH11_subTest$everwrk)
as.numeric(performance(ROCR_pred2, "auc")@y.values)



ROCR_perf1 = performance(ROCR_pred1, "tpr", "fpr")
plot(ROCR_perf1, colorize=TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))

ROCR_perf2 = performance(ROCR_pred2, "tpr", "fpr")
plot(ROCR_perf2, colorize=TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))

