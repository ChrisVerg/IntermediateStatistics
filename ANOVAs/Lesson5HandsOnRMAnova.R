# Lesson 5 Hands on
# 

# Packages
library("rcompanion")
library("fastR2")
library("car")
library("IDPmisc")
library("dplyr")

# Use the dataset "honey"

# You will determine whether honey production "totalprod" has changed 
# over the years "year" using a repeated measures ANOVA

colnames(honey)

count(honey, year)
# Create a df for each year and test for normality

# 2008
honey08 <- subset(honey, year %in% 2008)

# 2009
honey09 <- subset(honey, year %in% 2009)

# 2010
honey10 <- subset(honey, year %in% 2010)

# 2011
honey11 <- subset(honey, year %in% 2011)

# 2012
honey12 <- subset(honey, year %in% 2012)


# Assess Normality


# 2008
plotNormalHistogram(honey08$totalprod)
#Positive Skew
# Square Root
honey08$totalprodSQRT <- sqrt(honey08$totalprod)
plotNormalHistogram(honey08$totalprodSQRT)
# Log
honey08$totalprodLOG <- log(honey08$totalprod)
plotNormalHistogram(honey08$totalprodLOG)

# Use honey08$totalprodLOG for 2008


# 2009
plotNormalHistogram(honey09$totalprod)
#Positive Skew
# Square Root
honey09$totalprodSQRT <- sqrt(honey09$totalprod)
plotNormalHistogram(honey09$totalprodSQRT)
# Log
honey09$totalprodLOG <- log(honey09$totalprod)
plotNormalHistogram(honey09$totalprodLOG)

# Use honey09$totalprodLOG for 2009




# 2010 
plotNormalHistogram(honey10$totalprod)
#Positive Skew
# Square Root
honey10$totalprodSQRT <- sqrt(honey10$totalprod)
plotNormalHistogram(honey10$totalprodSQRT)
# Log
honey10$totalprodLOG <- log(honey10$totalprod)
plotNormalHistogram(honey10$totalprodLOG)

# Use honey10$totalprodLOG for 2010




# 2011
plotNormalHistogram(honey11$totalprod)
#Positive Skew
# Square Root
honey11$totalprodSQRT <- sqrt(honey11$totalprod)
plotNormalHistogram(honey11$totalprodSQRT)
# Log
honey11$totalprodLOG <- log(honey11$totalprod)
plotNormalHistogram(honey11$totalprodLOG)

# Use honey11$totalprodLOG for 2011


# 2012
plotNormalHistogram(honey12$totalprod)
#Positive Skew
# Square Root
honey12$totalprodSQRT <- sqrt(honey12$totalprod)
plotNormalHistogram(honey11$totalprodSQRT)
# Log
honey12$totalprodLOG <- log(honey12$totalprod)
plotNormalHistogram(honey12$totalprodLOG)

# Use honey12$totalprodLOG for 2012


# Combine the datasets
honey_transformed <- rbind(honey08, honey09, honey10, honey11, honey12)
View(honey_transformed)

# Convert year from numeric to factor
honey_transformed$year <- as.factor(honey_transformed$year)


### Homogeneity of Variance
leveneTest(totalprodLOG~year, data = honey_transformed)
# pvalue is greater than .05 which passes assumption

# Run analysis
RManova <- aov(totalprodLOG~year+Error(state), honey_transformed)
summary(RMAnova)


# The pvalue is greater than .05 indicating there is not a significant difference in
# honey production from year to year. The null hypothesis is not rejected. 



