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

RMAnova<- aov(repdat~contrasts+Error(`Participant Code`), bk4)
summary(RMAnova)

MMAnova<- aov(repdat~(TreatmentGroup*contrasts)+Error(`Participant Code`/(contrasts)), bk4)
summary(MMAnova)
















# Lesson 5 Repeated Measures ANOVAS Activity
# 

# Packages
library("rcompanion")
library("fastR2")
library("car")
library("IDPmisc")

# Determine whether weight changes from baseline to follow up
# Null, there is no change in weights from baseline to follow up

# Wrangle the data
# Test Assumptions
# Run Analysis for repeated measure Anova

###   Wrangling
# Create working dataset
brk <- breakfast

# drop the null rows
brk1 <- brk[1:33 , ]
View(brk1)

# Subset the columns needed
brk2 <- brk1[, 1:7]
View(brk2)

###    brk2 is now the original dataset

# Using brk2 create two datasets one for baseline one for follow up
# using weight for the weights column
# create a column to indicate B for baseline and P for post

# Dataset with Baseline Data
brk_base <- brk2[, 1:5]
brk_base$weightKG <- brk2$`Baseline Body Mass (kg)`
brk_base$prepost <- "B"
View(brk_base)

# Dataset for Follow-up Data
brk_follow <- brk2[, 1:5]
brk_follow$weightKG <- brk2$`Follow-Up Body Mass (kg)`
brk_follow$prepost <- "P"

colnames(brk_base)
colnames(brk_follow)

# Combine them into one dataset (append using rbind())

brk_clean <- rbind(brk_base, brk_follow)
View(brk_clean)

#
#
#
#
#
#


####   Assumptions

# Normality

# Baseline
plotNormalHistogram(brk2$`Baseline Body Mass (kg)`)
plotNormalHistogram(brk2$`Follow-Up Body Mass (kg)`)
# Both Distributions are close to normal

# Homogeneity of Variance
# levene's Test
leveneTest(weightKG ~ `Treatment Group` *prepost, data=brk_clean)
# pvalue > .05  This passes the assumption

# Sample Size > 20

# Sphericity N/A for this analysis


#
#
#
#
#
#

# Compute ANOVA
# Create model
RMAnova <-aov(weightKG~prepost+Error(`Participant Code`), brk_clean)
summary(RMAnova)
# pvalue > .05 indicates the null hypothesis is rejected, there is not a difference 
# in body weight from Baseline to Follow-up.

# Post Hoc
means <- brk_clean %>% group_by(`Treatment Group`, prepost) %>% summarize(Mean = mean(weightKG))
means
