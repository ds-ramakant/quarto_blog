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
attach(df)
plot(Volume)
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

plot(df$Volume)
