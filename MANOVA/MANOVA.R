# Lesson 8 MANOVA

# ANOVA with multiple DVs

# Using data on Heart Attacks condsider the question

# It is well-known that men are more likely to have heart attacks than
# women. How does gender (sex) influence some of the heart attack
# predictors like resting blood pressure (trestbps) and cholesterol
# (chol)?

library("multcomp")
library("mvnormtest")
library("car")
library("IDPmisc")
library("dplyr")

# Identify Variables

# IV sex
# DV1 trestbps
# DV2 chol

HT <- heartAttacks

# Wrangling & Data Preparation

class(HT$sex)
class(HT$trestbps)
class(HT$chol)
# All Variable are Numeric

# Format DVs as a matrix
DVs <- c("trestbps", "chol")
DVmatrix <- HT[DVs]

DVmatrix1 <- as.matrix(DVmatrix)

# Remove missing vvalues
DVmatrix2 <- NaRV.omit(DVmatrix1)

#####   Assumptions
##  Sample Size meets assumption

##  Multivariate Normality
# use mshapiro test with matrix
mshapiro.test(t(DVmatrix2))
#                           Assumption not met



## Homogeneity of Variance
leveneTest(HT$trestbps, HT$sex, data = kickstarter)
# trestbpd does meet assumption
leveneTest(HT$chol, HT$sex, data = kickstarter)
# chol does not meet assumption


## Absence of multicollinearity
cor.test(HT$trestbps, HT$chol, method="pearson", use="complete.obs")
# The meets the assumption for absence of multicollinearity


#####  Analysis

MANOVA <- manova(cbind(trestbps, chol) ~ sex, data = HT)
summary(MANOVA)


# The Difference is significant




##   Post Hocs

summary.aov(MANOVA, test ="wilks")


#### Means

Means <- HT %>% group_by(sex) %>% summarize(Mean=mean(trestbps))
Means

Means1 <- HT %>% group_by(sex) %>% summarize(Mean=mean(chol))
Means1


# Assuming 0 is male and 1 is female. The means suggest that males have higher 
# resting blood pressure and higher cholesterol. However the disparity in cholesterol
# is much higher. 