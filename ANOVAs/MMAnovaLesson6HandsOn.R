# Lesson 6 Hands On Mixed Measure ANOVA

# You will determine whether suicide rates (suicides/100k pop) has changed
# over the years (year), and see if the generation has any influence.

# The Null Hypothesis is there is no change in suicide rates between generations

# Load Packages
library("rcompanion")
library("fastR2")
library("car")
library("IDPmisc")
library("dplyr")



# Determine the test and Variables

# The generation is the IV
# The year is the time line of the data/study also IV
# The suicide rate is continuous it is the DV

# IV1 generation
# IV2 year
# DV  rate
# Subject ID  country

# Wrangle the data
#   Create a df for each year, assess for normality for each year then append 
# them into a single dataset

View(suicide)
# first identify how many levels exist for suicide$generation
count(suicide, generation)
# 6 total generations


# Rename suicides/100k pop
suicide$rate <- suicide$`suicides/100k pop`



# Identify number levels of suicide$year
yearvector <- count(suicide, year)
View(yearvector)


### Assess Assumptions ########################################
####### Normality

# Assess Continuous Variable-- rate
plotNormalHistogram(suicide$rate)
# Positive Skew

# Square root Transformation
suicide$rateSQRT <- sqrt(suicide$rate)
plotNormalHistogram(suicide$rateSQRT)

# Log transformation
suicide$rateLOG <- log(suicide$rate)
plotNormalHistogram(suicide$rateLOG)

# infinite number error
# Remove Null/inf values
suicide1 <- NaRV.omit(suicide)

# Log transformation
suicide1$rateLOG <- log(suicide1$rate)
plotNormalHistogram(suicide1$rateLOG)
# This is close to normal distribution

###### Homogeneity of Variance
# Levene's Test

leveneTest(rate~generation*year, data= suicide1)
# Error check data types
class(suicide$year)
class(suicide$rate)
class(suicide$generation)
# Since year is a categorical IV it needs to be factor data type and not numeric

suicide$year <- as.factor(suicide$year)
suicide1$year <- as.factor(suicide1$year)

# Rerun the test
leveneTest(rate~generation*year, data= suicide1)
# this does not pass the assumption of Homogeneity of Variance

###### Sample size meets assumption


########## Analysis

MManova <- aov(rate~(generation*year)+Error(country/(year)), suicide1)
summary(MManova)
# The pvalue is significant this indicates there is a change in suicide rates
# between the generations

### Post  Hoc
pairwise.t.test(suicide1$rate, suicide1$generation, p.adjust="bonferroni")


#### Means

suicideMeans <- suicide1 %>% group_by(generation, year) %>% summarize(Mean=mean(rate))
print(suicideMeans)
View(suicideMeans)


suicideMeans1 <- suicide1 %>% group_by(generation) %>% summarize(Mean=mean(rate))
print(suicideMeans1)
View(suicideMeans1)


# It appears that Gen Z has the lowest rate. However, this means the youngest newest generation 
# That has not lived as long. Therefore the amount of time this generation for 
# data collection is different, which is important to note. However it does appear
# That as time progressed and perhaps standards of living, awareness and access
# to care have improved, so has the rate of suicide. 
# 