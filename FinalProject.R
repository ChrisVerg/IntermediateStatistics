library("tidyr")
library("gmodels")
library(dplyr)
library("IDPmisc")
library("rcompanion")
library("car")

# DS 105
# Lesson 10 Final Project 

# Scenario 1
# Compare the expected rate of fraud .16 to the observed value of 28 in 94 subjects. 
# This will be a One proportion Z test
observed <- 28
n <-94
expected_rate <- 0.16

prop.test(observed, n, p = expected_rate)
# the test shows the proportion is .297 or about 30% which is almost double the 
# expected rate of 16%. The hypothesis of 16% is rejected.



# Scenario 2
# Are 4 different topical antiseptics being used in the same ration at 
# 3 different clinics. 
# This is analyzing frequency of Categorical IV and DV Chi Square. 

antiseptics1 <- antiseptics
antiseptics2 <- antiseptics1[rep(row.names(antiseptics1), antiseptics1$`Number of applications` ), 1:2]
View(antiseptics2)

# Create Cross-Tab
crosstab <- CrossTable(antiseptics2$`Antiseptic Type`, antiseptics2$Clinic,fisher = TRUE, chisq = TRUE, expected = TRUE, resid = TRUE, format = "SPSS" )
# Based on the Chi square analysis it appears the the antiseptic topical medications
# Are proportionally prescribed at the same rate between the three clinics. This makes 
# Sense as common ailments are usually distributed evenly in a population barring 
# any local or regional causes that may differ. It appears from looking at the total number
# that clinic 3 is the largest and clinic one is the smallest by total number of 
# prescribed antiseptic topical medications. The p-value is .97 indicating the null 
# hypothesis that the proportions are about the same is accepted. 




# Scenario 3
# Import new dataset 
# Given 4 categories of bank customers, each category has 40 to 60 accounts. 
# Assess the saving habits by comparing the change the mean account balance within each category and 
# compare between category. 

# One categorical IV and continuous DV comparing means indicates ANOVA. 

savings1 <- savings
# Changing data from wide to long
savings2 <- gather(savings1, key="Group", value="Balance")
View(savings2)

# Remove missing/null values
savings3 <- NaRV.omit(savings2)

# Assess Assumptions
#
# Assess for normality
plotNormalHistogram(savings3$Balance)
# Pretty Close to normal distribution

### Assess for Homogeneity for Variance
# Bartlett test for normally distributed data
bartlett.test(Balance ~ Group , data = savings3)
# We do not meet the assumption for Homogeneity of Variance

## Sample Size
View(savings3)
n > 20

# Computing ANOVA with unequal variance
# Create linear model
anovaModel <- lm(Balance~Group, data = savings3)
Anova(anovaModel, Type="II", white.adjust = TRUE)
# The P value indicates there is a statistically signigicant difference between the 
# customer groups

# Pairwise t-test corrected for homogeneity assumption violation
pairwise.t.test(savings3$Balance, savings3$Group, p.adjust="bonferroni", pool.sd =FALSE)
means <- savings3 %>% group_by(Group) %>% summarize(Mean = mean(Balance))
means


# The t-test indicates there each group is pretty different and no two groups are
# statistically similar. It appears group A customers are the best savers and 
# a campaign to increase deposits may work. However for targeting customers that 
# my be interested in other products associated with a lower savings rate, 
# group C may be a good potential base. 





####   Scenario 4

# Use the data to help gauge public sentiment about a school bond. 
# Two proportion Z test

# With school age children and favorable - 374  X Success
# With school age children and unfavorable - 129 X Failure
X_total <-  374 + 129
X_total
# Without school age children and favorable - 171 Y Success
# Without school age children and unfavorable - 74 Y Failure
Y_total <- 171 + 74
Y_total

# Create matrix with values as success/failure by group
observed <- matrix(c(374, 129, 171, 74), nrow=2, byrow=TRUE)

# Sample size
n <- rowSums(observed) 
n
# Calculate
twoPropTest <- prop.test(observed, n)
twoPropTest

# With a p value of 0.21 there does not appear to be a significant difference in the
# proportion between the two groups. At 69 and 74 percent both groups overall
# favor a bond



