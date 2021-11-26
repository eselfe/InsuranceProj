
# Probit and Logit Models

# Probit and Logit Models
data <- py$data
attach(data)

# Define Variables
Y <- cbind(TARGET_FLAG)
X <- cbind(KIDSDRIV, HOMEKIDS, 
           TRAVTIME, TIF)

# Descriptive Statistics
summary(Y)
summary(X)

table(Y)
table(Y) / sum(table(Y))



# Regression Coefficients (shouldn't do this)
olsreg <- lm(Y ~ X)
summary(olsreg)

# Logit model coefficients
logit <- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit)

# Logit model odds ration
exp(logit$coefficients)

# Probit model coefficients
probit <- glm(Y ~ X, family=binomial (link="probit"))
summary(probit)



# Regression marginal effects
coef(olsreg)

# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
LogitScalar * coef(logit)

# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)



# Regression predicted probabilities
polsreg <- predict(olsreg)
summary(polsreg)

# Logit model predicted probabilities
plogit <- predict(logit, type = "response")
summary(plogit)

# Probit model predicted probabilities
pprobit <- predict(probit, type = "response")
summary(pprobit)



# Percent correctly predicted values
table(true = Y, pred = round(fitted(probit)))
table(true = Y, pred = round(fitted(logit)))

# McFadden's Pseudo R-squared
probit0 <- update(probit, formula= Y ~ 1)
McFadden <- 1 - as.vector(logLik(probit) / logLik(probit0))
McFadden




missmap(data)

