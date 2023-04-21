# Computing ANCOVA using data about graduate school admissions

# Load Packages
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("dplyr")
library("psych")


# The Question
# Controlling for students' research participation in undergrad, does the 
# rating of the students' undergraduate university impact their chance of
# admittance into graduate school?

grad<- graduate_admissions
View(grad)

# Look a the means to see how it affects chance of admission  

grad %>% group_by(`University Rating`) %>% summarise(Mean = mean(grad$`Chance of Admit`))
grad %>% group_by(Research) %>% summarise(Mean = mean(grad$`Chance of Admit`))
# Neither Variables appear to have much of an effect based on the means

# For this Analysis select IV, DV anc CoV
# IV : Rating of undergrad university
# DV : Chance of Admission
# CoV: Research Participation

# Rename Columns
colnames(grad)
grad$UniversityRating <- grad$`University Rating`
grad$AdmitChance <- grad$`Chance of Admit`

# Check Data types
class(grad$UniversityRating)
#Numeric
class(grad$AdmitChance)
#Numeric
class(grad$Research)
#Numeric 

# Convert to factor
grad$UniversityRating <- as.factor(grad$UniversityRating)
grad$Research <- as.factor(grad$Research)


##### TEST ASSUMPTIONS

## Normality 
plotNormalHistogram(grad$AdmitChance)
# Close to Normal with slight Negative Skew, Try Square Transformaiton

## Square Transformation
grad$AdmitChanceSQ <- grad$AdmitChance * grad$AdmitChance
plotNormalHistogram(grad$AdmitChanceSQ)
# Normal Distributed

## Homogeneity of  Variance 
# levene's test
leveneTest(AdmitChanceSQ ~ UniversityRating, data= grad)
# .047 violated HoV assumption

## Homogeneity of Regression Slopes
# using lm()
Homogeneity_regrslp <- lm(AdmitChanceSQ ~ Research, data= grad)
anova(Homogeneity_regrslp)
# pvalue is again significant this indicates the Research should be used as an IV
# in a two way anova rather than a ANCOVA

## Sample Size
# n = 400

#### Run Analysis
# For this Analysis select IV, DV anc CoV
# IV : Rating of undergrad university
# DV : Chance of Admission
# CoV: Research Participation


## When Homogeneity of Variance assumption is met 
# model <- lm(DV~covariant + IV * CoV data = dataset)
ANCOVA <- lm(AdmitChanceSQ~Research + UniversityRating*Research, data= grad)
# run small a anova
anova(ANCOVA)



## When Homogeneity of Variance assumption is NOT!! met 
# Use the Big A Anova()

Anova(ANCOVA, Type = "I", white.adjust = TRUE)



# If Error presents try to :
# Rerun the above model, drop CoV from IV * Cov
#   ANCOVA1 <- lm(AdmitChanceSQ~Research + UniversityRating, data= grad)
#   Anova(ANCOVA1, Type = "I", white.adjust = TRUE)




#### Post Hocs 
# postHocs <- glht(ANCOVA,linfct=mcp(UniversityRating = "Tukey"))
# model <- glht() is multcomp function linfct=mcp is standard code for this 
# function linfct=mcp(IV = "Tukey")

postHocs <- glht(ANCOVA,linfct=mcp(UniversityRating = "Tukey"))
summary(postHocs)


### Due to the use of covariate must use adjust means for post hoc
adjMean <- effect('UniversityRating', ANCOVA)
adjMean







