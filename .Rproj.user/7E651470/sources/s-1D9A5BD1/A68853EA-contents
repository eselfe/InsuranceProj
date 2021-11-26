# Insurance



library(readr)
library(fastGraph)
library(Amelia)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(reticulate)
library(magrittr)




data <- py$Num_data


data$AGE <- as.numeric(unlist(data$AGE))
data$INCOME <- as.numeric(unlist(data$INCOME))
data$PARENT1 <- as.numeric(unlist(data$PARENT1))
data$MSTATUS <- as.numeric(unlist(data$MSTATUS))
data$SEX <- as.numeric(unlist(data$SEX))
data$EDUCATION <- as.numeric(unlist(data$EDUCATION))
data$JOB <- as.numeric(unlist(data$JOB))
data$CAR_USE <- as.numeric(unlist(data$CAR_USE))
data$CAR_TYPE <- as.numeric(unlist(data$CAR_TYPE))
data$RED_CAR <- as.numeric(unlist(data$RED_CAR))
data$REVOKED <- as.numeric(unlist(data$REVOKED))
data$URBANICITY <- as.numeric(unlist(data$URBANICITY))


attach(data)


# -----------------------------------------------------------------------------
# --- TARGET FLAG PREDICTIONS -------------------------------------------------
# -----------------------------------------------------------------------------


# Defining Variables
Y <- cbind(TARGET_FLAG)

X <- cbind(KIDSDRIV, HOMEKIDS, 
           TRAVTIME, TIF) # 6002 / 6008 correctly




X <- cbind(KIDSDRIV, URBANICITY, 
           TRAVTIME, TIF, AGE, INCOME, EDUCATION) # 6086 / 6008

X <- cbind(INCOME, EDUCATION, TRAVTIME, KIDSDRIV, HOMEKIDS) # 5981 / 6008



# Summarize Data
summary(Y)
summary(X)

table(Y)  # instances
table(Y) / sum(table(Y)) # marginal(?) probability



# Logit model coefficients
logit <- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit)

# Logit odds ratio
exp(logit$coefficients)



# Probit model coefficients
probit <- glm(Y ~ X, family = binomial (link = "probit"))
summary(probit)




# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
LogitScalar * coef(logit)

# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)



# Logit predicted probabilities 
plogit <- predict(logit, type = "response")
summary(plogit)

# Probit model predicted probabilityes
pprobit <- predict(probit, type = "response")
summary(pprobit)





# Percent correctly predicted values
table(true = Y, pred = round(fitted(probit)))
table(true = Y, pred = round(fitted(logit)))










# -----------------------------------------------------------------------------
# --- TARGET AMOUNT PREDICTIONS -----------------------------------------------
# -----------------------------------------------------------------------------



crashData <- data %>% filter(data$TARGET_FLAG == 1)
attach(crashData)

Y <- cbind(TARGET_AMT)
X <- cbind(INCOME, HOME_VAL, SEX, JOB, CLM_FREQ) # no significance, r^2=0.005

X <- cbind(AGE, HOMEKIDS, YOJ, PARENT1, MSTATUS, BLUEBOOK) # r^2=0.01587

X <- cbind(BLUEBOOK, OLDCLAIM, CLM_FREQ, CAR_AGE) # r^2=0.01523

X <- cbind(KIDSDRIV, YOJ, INCOME, PARENT1, HOME_VAL, MSTATUS, 
           SEX, EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, 
           TIF, CAR_TYPE, RED_CAR, OLDCLAIM, CLM_FREQ, REVOKED, 
           MVR_PTS, CAR_AGE, URBANICITY)

summary(Y)
summary(X)

myglm1 <- lm(Y ~ X)
summary(myglm1)































