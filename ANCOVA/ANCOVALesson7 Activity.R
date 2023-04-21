# Computing ANCOVA using data about graduate school admissions

# Load Packages
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("dplyr")
library("psych")

# Lesson 7 Activity

# Predict a students college GPA holding the TOEFL score constant and
# using the University Rating as a predictor. 

# Does the the University Rating significaly predict CGPA when TOEFL when
# Holding TOEFL Score Constant?

# To use an ANCOVA for this analysis Id 

# IV  University Rating (Categorical)
# DV CGPA (Continuous)
# CoV TOEFL (Continuous)


# Import Data
admis<- graduate_admissions
# admis <- NaRV.omit(admis$CGPA)
View(admis)


# Check Data Types
class(admis$`TOEFL Score`)
class(admis$CGPA)
class(admis$`University Rating`)



# Rename and convert
admis$TOEFL <- admis$`TOEFL Score`

admis$UniversityRating <- admis$`University Rating`
class(admis$UniversityRating)




#### Test Assumptions
## Normality

plotNormalHistogram(admis$TOEFL)
# Looks pretty Normal

plotNormalHistogram(admis$CGPA)
# Close to normal but slight Negative Skew

# Square Transformation
admis$CGPA_SQ <- admis$CGPA * admis$CGPA
plotNormalHistogram(admis$CGPA_SQ)
# Much closer to normal

##Homogeneity of Variance
# Levene's Test
admis$UniversityRating <- as.factor(admis$UniversityRating)  
leveneTest(CGPA_SQ~UniversityRating, data= admis)

# Not appropriate for these variables?

## Homogeneity of Regression Slopes
Homogeneity_regrSlp <- lm(CGPA_SQ ~ TOEFL, data= admis)
anova(Homogeneity_regrSlp)
# pvalue is significant
#### This should change to two way ANOVA


## Sample n > 40 Assumption is met

# ANCOVA Analysis

# IV  University Rating (Categorical)
# DV CGPA (Continuous)
# CoV TOEFL (Continuous)

ANCOVA<- lm(CGPA_SQ~UniversityRating + UniversityRating*TOEFL, data = admis)
anova(ANCOVA)



#### Post Hoc
postHoc<- glht(ANCOVA, linfct = mcp(UniversityRating = "Tukey"))
summary(postHoc)

adjmean <- effect("UniversityRating", ANCOVA)
adjmean
