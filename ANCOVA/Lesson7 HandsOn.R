# Lesson 7  Hands On

# Load Packages
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("dplyr")
library("psych")


# Many folks with international relatives often find themselves calling at
# odd hours to fit typical schedules in other time zones. How does the
# presence or absence of an international phone plan
# (International.Plan) influence the use of nighttime minutes
# (Night.Mins), holding whether or not the client has a voice mail plan
# (vMail.Plan) constant?

# IV  International Plan Categorical
# DV Night.Mins Continuous 
# CoV vMail.Plan Categorical

# Explore Data
phone<- cellPhone
View(phone)
phone$IV_IntPlan <- phone$`International Plan`
phone$DV_NightMins <- phone$`Night Mins`
phone$CoV_Vmail <- phone$`vMail Plan`

class(phone$IV_IntPlan)
class(phone$DV_NightMins)
class(phone$CoV_Vmail)

#phone$IV_IntPlan <- dummy.code(phone$IV_IntPlan)
#phone$CoV_Vmail <- dummy.code(phone$CoV_Vmail)

phone$IV_IntPlan <- as.factor(phone$IV_IntPlan)
phone$CoV_Vmail <- as.factor(phone$CoV_Vmail)


#####  Check Assumptions

## Normality
plotNormalHistogram(phone$DV_NightMins)
# Looks Pretty Normal


## Homogeneity of Variance
leveneTest(DV_NightMins~IV_IntPlan, data = phone)
# Meets Assumption

## Homogeneity of Regression Slopes
Homgty_REG<- lm(DV_NightMins~CoV_Vmail, data = phone)
anova(Homgty_REG)
# Meets Assumption

## Sample size
# Meets Assumption


#### Computing ANCOVA
# Create Model having met Assumption for Homogeneity of Variance
ANCOVA <- lm(DV_NightMins~CoV_Vmail + IV_IntPlan * CoV_Vmail, data = phone)
anova(ANCOVA)



#### PostHoc 
PostHoc<- glht(ANCOVA, linfct = mcp(IV_IntPlan = "Tukey"))
summary(PostHoc)


# Adjusted Means
adjmean <- effect("IV_IntPlan", ANCOVA)
adjmean


# There is not statistically significant pvalue and the adjust means appear to be similar
# The presence or absence does not have a significant effect on Night Minutes