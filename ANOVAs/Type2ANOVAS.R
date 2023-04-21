# Lesson 5 Repeated Measures ANOVAS
# 

# Packages
library("rcompanion")
library("fastR2")
library("car")
library("IDPmisc")


# You will be examining df "breakfast from a study about the effect of eating breakfast on
# weight loss and associated metrics, such as resting metabolic rate and waist
# circumference. Most metrics were measured at baseline, and then again at
# follow-up, which was six weeks later.
#

# Null is that there is no difference
#
#

# Data Reshape and Exploration
#
#
#
# Assess for NaN
View(breakfast)
bk <- breakfast[1:33,]

View(bk)

# Create a dataset for the start of the study
colnames(bk)

# Create Vector for column numbers that are bring kept
keep <- c(1,2,3,4,5,51,52)

# Data set with original variables for test
bk1 <- bk[, c(1,2,3,4,5,51,52)]
View(bk1)
colnames(bk1)

# Drop metabolic rate columns
bk2 <- bk1[, c(1:5)]
colnames(bk2)


# Baseline dataset
# Create new columns for metabolic data and contrasts
# repdat--repeated data
bk2$repdat <- bk1$`Baseline Resting Metabolic Rate (kcal/d)`
bk2$contrasts <- "T1"

# Follow up dataset
bk3 <- bk1[, c(1:5)]
bk3$repdat <- bk1$`Follow-Up Resting Metabolic Rate (kcal/d)` 
bk3$contrasts <- "T2"

# Combine the datasets
bk4 <- rbind(bk2,bk3)
View(bk4)

bk4$TreatmentGroup <- bk4$`Treatment Group`

#
#
#
#
#
#
### Assumptions

# Normality
plotNormalHistogram(bk1$`Baseline Resting Metabolic Rate (kcal/d)`)
plotNormalHistogram(bk1$`Follow-Up Resting Metabolic Rate (kcal/d)`)
# Both appear to be pretty close to normal

# Homogeneity of Variance
# This function did not like column names with spaces
bk4$TreatmentGroup <- bk4$`Treatment Group`

leveneTest(repdat ~ TreatmentGroup*contrasts, data = bk4)
# this test is not significant and passes assumption

# Repeated Measure ANOVA

RMAnova <- aov(repdat~contrasts+Error(`Participant Code`), bk4)
summary(RMAnova)
