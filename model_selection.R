
#importations
library(car)
library(gmodels)
library(lsr)


#Load Data from CSV
data = read.csv("loans_oct_dec_2017/combined.csv")
summary(data)

#Model 1 ->
cs = chisq.test(data$Employment, data$status)

summary(data$ave)

