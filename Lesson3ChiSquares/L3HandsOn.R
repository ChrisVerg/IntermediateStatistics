# Lesson 3 Hands on 

# This assignment will be analyzing bank loan data in R
# For each part:
# 1. Check and correct assumptions
# 2. perform appropriate categorical data analysis in R
# 3. Provide conclusion about analysis

# Packages:
library("tidyr")
library("gmodels")
library(dplyr)


# Part 1:   Does the term of the loan influence loan status, If so, how?


# This is comparing two categorical variables, will use Independent Chi-Square
# The null hypothesis is that loan term does not influence loan status, they are not related. 


# What are the levels for each category
loans %>% group_by(term) %>% summarize(count=n())
loans %>% group_by(loan_status) %>% summarize(count=n())


# Use gmodels to create cross tab and run Chi_Square analysis

CrossTable(loans$term, loans$loan_status,fisher = TRUE, chisq = TRUE, expected = TRUE, resid = TRUE, format = "SPSS" )
# Each cell contains greater than 5 for expected frequencies
# The p value indicates the loan term does influence the loan status. The null hypothesis is rejected
# The 36 month loan makes up nearly 80% of all of the loans of that 80% nearly 90% of those are fully paid. 
# The 60 month loans make up only about 20% of all the loans. Of that 20% only 65% are fully paid. 
# The loss rate for 36 month loans is 11% where as 60 month loans have a loss rate of 25%. 
# The 36 month loans are more profitable. 





#    Part 2:
# How has the ability to own a home changed after 2009?

# Recode the string date to Date format.
# Recode dates to pre and post 2009
# Recode Own or rent to binary

# Changing date format
# separate() is from tidyr


# Recode Date
loans$DateR <- as.Date(loans$Date, format="%m/%d/%Y")
Loans1 <- separate(loans, DateR, into = c("year", "month", "day"), sep = "-")
View(Loans1)

# change variable to binary
Loans1$year[Loans1$year <= 2009] <- 0
Loans1$year[Loans1$year > 2009] <- 1

# Check the levels of home_ownership
Loans1 %>% group_by(home_ownership) %>% summarize(count=n())
# Great, only two levels, will leave these as is. There are about 5 times more renters
# Recoding home_ownership
Loans1$home_ownership[Loans1$home_ownership == "RENT"] <- 0
Loans1$home_ownership[Loans1$home_ownership == "OWN"] <- 1

# Run McNemar Chi-Square
CrossTable(Loans1$year, Loans1$home_ownership,fisher = TRUE, chisq = TRUE,mcnemar = TRUE, expected = TRUE, resid = TRUE, format = "SPSS" )

# Expected values meet assumption
# The pvalue is significant and indicates there is a change.
# It appears there is a difference in the count however, for both renting and owing
# the numbers increase by about 5 times from before and after 2009. 





# Part 3:

# The news just ran a story that only 15% of homes are fully paid for in America, 
# and that another 10% have given up on paying it back, so the bank has "charged off"
# the loan. Does it seem likely that the data for this hands on came from the 
# larger population of America?

# Comparing frequencies of a single variable in a sample to a population calls for 
# Goodness of Fit Chi-Square
# Null hypothesis, they data are the same

# View loan status data
loans %>% group_by(loan_status) %>% summarize(count=n())

# Vectors for observed and expected values
# Charged off, current, fully paid
observed <- c(3282, 502, 18173)
expected <- c(0.10 ,0.75 ,0.15 )
chisq.test(x= observed, p= expected)

# The pvalue is significant and indicates there is a difference in the data given
# and the data from the news story.

# View the distribution of loan status by percentage
loans %>% group_by(loan_status) %>% summarize(count=n()) %>% mutate(percent = count /sum(count)  *100)
# The percentage for the data compared to the news story as follows.

# Fully paid: News 15% / Given Data 82.8%
# Charged Off: News 10% / Given Data 14.9%
# Current: News 75% / Given Data 2.29%


# The data suggests there are more lost loans in that time period than current.
# It also suggest that most homes are paid off. 