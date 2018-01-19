# Short R script to show an example of an ANOVA
# For this example we will use some data on cell phones:
# phones = categorical, 3 types of phones,
# and "battery life" (in hours).

Phones <- read.csv("Phones.csv")
View(Phones)

# Load the CAR library, which we need for ANOVA
library(car)

# We can check battery life to see what the distribution
# looks like.
hist(Phones$BatteryLife)

# So, not great as far as normality-- but, remember that 
# ANOVA is robust, and this is not a big concern for us.

# Here we run the model and then show the results.

AnovaModel <- aov(BatteryLife ~ Phone, data=Phones)
summary(AnovaModel)

# What about our effect size for the ANOVA? 
# We can use Omega squared, which we can calculate manually.

omega_sq <- function(Anov){
  sum_stats <- summary(Anov)[[1]]
  SSm <- sum_stats[["Sum Sq"]][1]
  SSr <- sum_stats[["Sum Sq"]][2]
  DFm <- sum_stats[["Df"]][1]
  MSr <- sum_stats[["Mean Sq"]][2]
  W2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
  return(W2)
}

# Now we just run our function with the name of our model.
# Remember, suggested interpretations for Omega Squared are:
# small (.01), medium (.06), and large (.14 or higher)

omega_sq(AnovaModel)

# Based on those results, we can now look at our pairwise results
# between each of the three groups.
# First, we look at the means by category. I use 'tapply' which 
# allows us to get the means of battery life, by phone.

tapply(Phones$BatteryLife, Phones$Phone, mean, na.rm=T)

# Now, we calculate the corrected p-values for each pair
# using the bonferroni correction.

pairwise.t.test(Phones$BatteryLife, Phones$Phone, p.adjust.method="bonferroni")

# We can also just look at the means graphically to help us see 
# what is going on.
ggplot(Phones, aes(x=Phone, y=BatteryLife)) + geom_bar(stat = "identity")

