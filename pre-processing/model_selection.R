
#importations
library(car)
library(gmodels)
library(lsr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(scales)
library(ztable)


setwd("/Users/monicah/Desktop/Loanbee/Loanbee/pre-processing/")

#Load Data from CSV
data = read.csv("../data/loans_oct_dec_2017/combined.csv")


#Hypothesis 1 -> Mpesa Transactions

#Draw a box plot to visualize the groups.
boxplot(data$avg_daily_mpesa_txns_amount~data$status, main="MPesa Transactions for complete, default and late loans", xlab="Group", ylab="Average Daily mPesa Transactions")
#In addition to the box plot, show means per group
tapply(data$avg_daily_mpesa_txns_amount, data$status, mean, na.rm=T)

#We conduct an anova test to compare the means of 3 groups
AnovaModel <- aov(avg_daily_mpesa_txns_amount ~ status, data=data)
summary(AnovaModel)

#Anova might show that the 3 means are statistically different, but doesnt show which relationships are significant. The Tukey Honest Significant Difference compares all pairs of means to evaluate significance
TukeyHSD(AnovaModel, conf.level=0.99)

#As an alternative we can also calculate adjusted p-values using the bonferroni correction. This confirms what we already figured out using Tukey HSD
pairwise.t.test(data$avg_daily_mpesa_txns_amount, data$status, p.adjust.method="bonferroni")

#Omega Squared is used to calculate the overall effect size
# Suggested interpretations for Omega Squared are: small (.01), medium (.06), and large (.14 or higher)
omega_sq <- function(Anov){
  sum_stats <- summary(Anov)[[1]]
  SSm <- sum_stats[["Sum Sq"]][1]
  SSr <- sum_stats[["Sum Sq"]][2]
  DFm <- sum_stats[["Df"]][1]
  MSr <- sum_stats[["Mean Sq"]][2]
  W2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
  return(W2)
}
omega_sq(AnovaModel)






#Hypothesis 2 -> External Credit Score
#Draw a box plot to visualize the groups.
boxplot(data$credit_score ~data$status, main="External Credit Score for complete, default and late loans", xlab="Group", ylab="External Credit Score")
#In addition to the box plot, show means per group
tapply(data$credit_score, data$status, mean, na.rm=T)

#We conduct an anova test to compare the means of 3 groups
AnovaModel2 <- aov(credit_score ~ status, data=data)
summary(AnovaModel2)

#Anova might show that the 3 means are statistically different, but doesnt show which relationships are significant. The Tukey Honest Significant Difference compares all pairs of means to evaluate significance
TukeyHSD(AnovaModel2, conf.level=0.99)

#As an alternative we can also calculate adjusted p-values using the bonferroni correction. This confirms what we already figured out using Tukey HSD
pairwise.t.test(data$credit_score, data$status, p.adjust.method="bonferroni")

#Omega Squared is used to calculate the overall effect size
# Suggested interpretations for Omega Squared are: small (.01), medium (.06), and large (.14 or higher)

omega_sq(AnovaModel2)



#Hypothesis 3 -> #No of Loans
boxplot(data$no_of_loans_completed_previously ~data$status, main="#No of Loans Completed Previously for complete, default and late loans", xlab="Group", ylab="No of Loans Completed Previously")
#In addition to the box plot, show means per group
tapply(data$no_of_loans_completed_previously, data$status, mean, na.rm=T)
#We conduct an anova test to compare the means of 3 groups
AnovaModel3 <- aov(no_of_loans_completed_previously ~ status, data=data)
summary(AnovaModel3)
#Anova might show that the 3 means are statistically different, but doesnt show which relationships are significant. The Tukey Honest Significant Difference compares all pairs of means to evaluate significance
TukeyHSD(AnovaModel3, conf.level=0.99)

#As an alternative we can also calculate adjusted p-values using the bonferroni correction. This confirms what we already figured out using Tukey HSD
pairwise.t.test(data$no_of_loans_completed_previously, data$status, p.adjust.method="bonferroni")

#Omega Squared is used to calculate the overall effect size
# Suggested interpretations for Omega Squared are: small (.01), medium (.06), and large (.14 or higher)
omega_sq(AnovaModel3)



#Hypothesis 4 -> #No of Dependants
#Do a cross table to better visualize the data.
CrossTable(data$dependants, data$status, fisher = FALSE) 
#Conduct the Chi-Test
cs = chisq.test(data$dependants, data$status)
cs
#Check the effect size
# As a rule of thumb, Cramer's V under .2 is weak, between .2 and .4 is strong and above .4 is very strong
cramersV(data$dependants, data$status)


#Hypothesis 5 -> Marital Status
#Do a cross table to better visualize the data.
CrossTable(data$marital, data$status, fisher = FALSE) 
#Conduct the Chi-Test
cs = chisq.test(data$marital, data$status)
cs
#Check the effect size
# As a rule of thumb, Cramer's V under .2 is weak, between .2 and .4 is strong and above .4 is very strong
cramersV(data$marital, data$status)


