# Lesson 4 Hands on 

# Import Packages for ANOVA
library("rcompanion")
library("dplyr")
library("IDPmisc")
library("car")

# Answer the question:
# Does the average price of avocados differ between
# Albany, Houston, and Seattle?

# Calculate using 'AveragePrice' and 'region'

# Null is that the average price is the same between the 3 cities

# Create a working copy of the data
ava <- avocados
View(ava)




#### Reduce/Subset the data to only the 3 cities in the analysis
# look at the levels of region

print(ava %>% group_by(region)%>% summarize(), n=100)
# there are 54 levels


desiredRegions <- c("Albany", "Houston", "Seattle")
ava1 <- ava[ava$region %in% desiredRegions, ]
print(ava1 %>% group_by(region)%>% summarize(), n=100)
# Here we have the data narrowed down to just the 3 required 
# cities for analysis
####

####
# Subset only the price and region columns
View(ava1)
ava2 <- ava1[c("region","AveragePrice")]
View(ava2)

# Take a look at the subset data means
ava2 %>% group_by(region) %>% summarize(Mean = mean(AveragePrice))
####


# IV region   DV  AveragePrice
####  Assess Assumptions
## Normality
plotNormalHistogram(ava2$AveragePrice)
# This is pretty close to normal but still has some positive skew

# Square Root Transformation

ava2$AveragePriceSQRT <- sqrt(ava2$AveragePrice)
plotNormalHistogram(ava2$AveragePriceSQRT)
# Closer to normal distribution

# Logarithmic

ava2$AveragePriceLOG <- log(ava2$AveragePrice)
plotNormalHistogram(ava2$AveragePriceLOG)
# Log transformation looks pretty good


## Assess Homogeneity of Variance
# Barletts test
bartlett.test(AveragePriceLOG ~ region, data = ava2)
# pvalue indicated data fail homogeneity of Variance

# Fligners
fligner.test(AveragePrice ~ region, data = ava2)
# Also Fails             

# Sample size exceeds assumption
####



####  Computing ANOVA on data with unequal Variance
# Welch's Oneway Test
# (Anove from library(car))

WelchModel <- lm(AveragePrice ~ region, data = ava2)
Anova(WelchModel, Type ="II", white.adjust = TRUE)
# The pvalue is less than .05 indicating there is a statistically 
# Significant difference in the average price between the 3 regions
# The null hypothesis is rejected. 

#### 



#### Post Hoc
# Post Hoc with violation of Homogeneity
pairwise.t.test(ava2$AveragePrice, ava2$region, p.adjust="bonferroni", pool.sd=FALSE)
# Still appears to be a significant difference of AveragePrice between
# The 3 regions, the least significant appears to be between 
# Seattle and Albany, although still significant. 

## Means Analysis
ava2 %>% group_by(region) %>% summarize(Mean = mean(AveragePrice))

# Analysis of the means appears consistent with the above analysis
# Albany has the highest mean but is somewhat close to Seattle. 
# However, Houston is much lower than both other regions. 