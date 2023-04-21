# Lesson 4 ANOVAs

# Does the number of views differ from the grade.

# The grade is Categorical and views is continuous. 

#libraries
library("rcompanion")
library(dplyr)
library("IDPmisc")
library("car")

# Wrangling

YT <- YouTubeChannels
View(YT)

YT %>% group_by(Grade) %>% summarize(count = n())
# It appears some channels are not graded leaving null values. 
YT1 <- NaRV.omit(YT)
YT1 %>% group_by(Grade) %>% summarize(count = n())
# The Grade variable looks good now. 

# Take a quick look at average views by grade
YT1 %>% group_by(Grade) %>% summarize(Mean = mean(`Video views`))
# There appears to be a pretty clear correlation between Grade and Mean views

# Assess Assumptions
#
#


# Assess for normality
#
#
#

plotNormalHistogram(YT1$`Video views`)
# This has very positive skew

# Square Root Transformation
YT1$viewsSQRT <- sqrt(YT1$`Video views`)
plotNormalHistogram(YT1$viewsSQRT)
# Still not very close to normal

# Logarithmic Transformation
YT1$viewsLOG <- log(YT1$`Video views`)
plotNormalHistogram(YT1$viewsLOG)

# Tukey Transformation
YT1$Tukey <- transformTukey(YT1$`Video views`, plotit = FALSE)
plotNormalHistogram(YT1$Tukey)
# This is the closest to normal distribution
#
#
#
#
#
#
# Recode 'Video Views' to views  
YT1$views <- YT1$`Video views`

### Assess for Homogeneity for Variance
# Bartlett test for normally distributed data
bartlett.test(views ~ Grade, data = YT1)
bartlett.test(Tukey ~ Grade, data = YT1)
# Even using normally distributed variable still have significant pvalue.

# Fligners test
fligner.test(views ~ Grade, data = YT1)
fligner.test(Tukey ~ Grade, data = YT1)
# These both turn up witha pvalue that is significant 
# need to add white.adjust = TRUE when computing ANOVA

#
#
#
#
#
#
# Sample Size > 20 is met
# The groups are believed to be unrelated. 

###

# Computing ANOVA with unequal variance
# Create linear model
anovaModel <- lm(views~Grade, data = YT1)
Anova(anovaModel, Type="II", white.adjust = TRUE)

# Ad Hoc
# No adjustment for Type 1 error
pairwise.t.test(YT1$Tukey, YT1$Grade, p.adjust="none")


# Mild correction using Bonferroni
pairwise.t.test(YT1$Tukey, YT1$Grade, p.adjust="bonferroni")


means <- YT1 %>% group_by(Grade) %>% summarize(Mean = mean(`Video views`))
means

# As noted with earlier viewing of the means. The grade and the mean of the views
# is correlated with the Best channels having the most views and the lowest grade 
# channels having the least mean of the views. The proportion of the views matches the 
# grades in order. 
