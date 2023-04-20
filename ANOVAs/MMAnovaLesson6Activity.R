# Mixed Measure ANOVA Activity Lesson 6

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
brk_clean$TreamentGroup <- brk_clean$`Treatment Group`
brk_clean$ParticipantCode <- brk_clean$`Participant Code`

# Compute ANOVA
# Create model
MMAnova1<- aov(weightKG~(TreamentGroup*prepost)+Error(ParticipantCode/(prepost)), brk_clean)
summary(MMAnova1)

# Subject ID 
# IV1 Breakfast or No Breakfast
# IV2 Baseline and Post
# DV Measure of body weight 



