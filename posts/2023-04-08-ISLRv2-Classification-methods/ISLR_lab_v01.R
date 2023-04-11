library(tidyverse)
library(ISLR2)
library(plotly)
glimpse(Smarket)

# This data set consists of
# percentage returns for the S&P 500 stock index over 1, 250 days, from the
# beginning of 2001 until the end of 2005. For each date, we have recorded
# the percentage returns for each of the five previous trading days, Lag1
# through Lag5. We have also recorded Volume (the number of shares traded
# on the previous day, in billions), Today (the percentage return on the date
# in question) and Direction (whether the market was Up or Down on this
# date). Our goal is to predict Direction (a qualitative response) using the
# other features.

summary(Smarket)
df <- Smarket

df %>% 
  group_by(Year) %>% 
  count(Direction)
df %>% 
  count(Year)

# What is the correlation between the variables? 

cor(df[,-9])

# there is little correlation between lag variables and Today. 
# However tehre is some degree of correlation between lag and Volume
# substantial correlation between Volume and Year


# generating a quick plot
plot(df$Volume)
# this shows that volumnei s increasing over a period of time


# Logisitic regression ----------------------------------------------------

# we use glm() as it considers a family of distributions. 
# for logistic regression, we pass the argument `family = binomial`

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag4 + Lag5 + Volume, 
                data = df, family = binomial)

summary(glm.fits)
# we see that p value of only Lag1 is the least but still not sufficiently small

coef(glm.fits)

summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

# predicted with `type = "response"` means that we are calculating the 
# probabilities of each variable as opposed to any other logit function
glm.probs <- predict(glm.fits, type = "response")
levels(df$Direction)
contrasts(df$Direction)

# however for us to predict the direction of the market, we have to convert
# the probabilities into either `Up` or `Down`

glm.pred <- rep("Down", nrow(df))   # creating vector of only `Down`
glm.pred[glm.probs >0.5] = "Up"     # filling up those with prob > 0.5 as `Up`

table(glm.pred, df$Direction)
mean(glm.pred== df$Direction)

# we see the accuracy is 52.16%. there is still room for improvement.

# now we specifically want to create test data for 2005 and 
# train on all years before it
attach(df)
train <- (Year< 2005)
df.2005 <- df[!train,]
dim(df.2005)

# creating a new glm by only considering 2 predictors
glm.fits2 <- glm(Direction ~ Lag1 + Lag2, family = binomial,
                 data = df, subset = train)
# predicting on test 2005 data
glm.probs2 <- predict(glm.fits2, df.2005, type = "response")

# creating new output df 
glm.pred2 <- rep("Down", nrow(df.2005))
glm.pred2[glm.probs2 > 0.5] = "Up"

# creating confusion matrix
table(glm.pred2, df.2005$Direction)
mean(glm.pred2==df.2005$Direction)
# result is 56% accuracy


# Linear Discriminant Analysis --------------------------------------------
library(MASS)
# using the same test & train data to create LDA model
lda.fit <- lda(Direction ~ Lag1 + Lag2, subset = train)
lda.fit
lda.fit$scaling # here are the coefficients of the LD used to make the plot

# The plot() function produces plots of the linear discriminants, obtained
# by computing −0.642×Lag1−0.514×Lag2 for each of the training observations.
plot(lda.fit)

# The predict() function returns a list with three elements. The first element,
# class, contains LDA’s predictions about the movement of the market.
# The second element, posterior, is a matrix whose kth column contains the
# posterior probability that the corresponding observation belongs to the kth
# class, computed from (4.15). Finally, x contains the linear discriminants,
# described earlier.
lda.pred <- predict(lda.fit, df.2005)
names(lda.pred)
head(lda.pred$class)

# checking the accuracy
table(lda.pred$class, df.2005$Direction)
mean(lda.pred$class==df.2005$Direction)

# accuracy is 56% 


# Quadratic Discriminant Analysis -----------------------------------------

qda.fit <- qda(Direction ~ Lag1+ Lag2, subset = train)
qda.pred <- predict(qda.fit, df.2005)
table(qda.pred$class, df.2005$Direction)
mean(qda.pred$class== df.2005$Direction)
# accuracy is 60% which is considered good for stock market


# Naive Bayes -------------------------------------------------------------
install.packages("e1071")
library(e1071) #package for Naive Bayes

nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, 
                     subset = train, data = df)
nb.fit

# The output contains the estimated mean and standard deviation for each
# variable in each class. For example, the mean for Lag1 is 0.0428 for
# Direction=Down, and the standard deviation is 1.23. We can easily verify
# this:

mean(Lag1[train][Direction[train]== "Down"])
sd(Lag1[train][Direction[train]== "Down"])

# predicting the output
nb.pred <- predict(nb.fit, df.2005)
table(nb.pred, df.2005$Direction)
mean(nb.pred ==df.2005$Direction)

# Poisson Regression ------------------------------------------------------
# using the Bikeshare dataset from ISRL2 package
attach(Bikeshare)
glimpse(Bikeshare)
# task is to predict `bikers` which is a whole number 
hist(bikers)

# begin by fitting ordinary least sqares regression aka linear regression
model.lm <- lm(bikers ~ mnth + hr + workingday + temp + weathersit, 
               data = Bikeshare)
summary(model.lm)

# this part i don't understand
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)

head(Bikeshare$mnth)

model.lm2 <- lm(bikers ~ mnth + hr + workingday + temp + weathersit, 
                data = Bikeshare)
summary(model.lm2)

# What is the difference between the two codings? In mod.lm2, a coefficient
# estimate is reported for all but the last level of hr and mnth. Importantly,
# in mod.lm2, the coefficient estimate for the last level of mnth is not zero:
# instead, it equals the negative of the sum of the coefficient estimates for
# all of the other levels.
# Similarly, in mod.lm2, the coefficient estimate for the
# last level of hr is the negative of the sum of the coefficient estimates for
# all of the other levels. This means that the coefficients of hr and mnth in
# mod.lm2 will always sum to zero, and can be interpreted as the difference
# from the mean level. For example, the coefficient for January of −46.087
# indicates that, holding all other variables constant, there are typically 46
# fewer riders in January relative to the yearly average.

# It is important to realize that the choice of coding really does not matter,
# provided that we interpret the model output correctly in light of the coding
# used. For example, we see that the predictions from the linear model are
# the same regardless of coding:

all.equal(predict(model.lm), predict(model.lm2))

coef(model.lm2)[2:12] # coeff from jan to Nov
sum(coef(model.lm2)[2:12]) # coeff for Dec

coef_months = c(coef(model.lm2)[2:12], sum(coef(model.lm2)[2:12]))
plot(coef_months, xlab = "Month", ylab = "coefficients", 
     xaxt = "n", # removes the x-axis labels
     col = "blue", pch = 19, type= "o")
axis(side = 1, at = 1:12, # adding month names to the above plot
     labels = c("J", "F", "M", "A","M", "J", "J", "A", "S", "O", "N", "D"))

# now trying poisson regression
model.pois <- glm(bikers ~ mnth + hr + workingday + temp + weathersit, 
                   data = Bikeshare, family = poisson)
summary(model.pois)

# plotting the difference b/w predictions of lm and poisson
# However, we must use the
# argument type = "response" to specify that we want R to output exp( ˆ β0 +
# ˆ β1X1+. . .+ ˆ βpXp) rather than ˆ β0+ ˆ β1X1+. . .+ ˆ βpXp, 
# which it will output  by default.

plot(predict(model.lm2), predict(model.pois, type = "response"))
abline(0,1, col  =2, lwd = 3)


