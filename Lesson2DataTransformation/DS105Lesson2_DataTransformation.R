# Import Data

# Import Package
# The package/library used for this lesson is rcompanion

install.packages("rcompanion")
library("rcompanion")

# Use plotNormalHistogram function to view the score variable in anime dataset

plotNormalHistogram(anime$score)
# score appears to be normally distributed

# View the scored_by
plotNormalHistogram(anime$scored_by)


# Transforming positively skewed data
# Using sqrt()

anime$scored_bySQRT <- sqrt(anime$scored_by)
plotNormalHistogram(anime$scored_bySQRT)
# This still has a strong postitive skew

# Use log()
anime$scored_byLOG <- log(anime$scored_by)
plotNormalHistogram(anime$scored_byLOG)
# Returns Error

# R calculated -inf or infinitely small using the library "IDPmisc"
# Create a new dataset that will omit missing and infinite data

install.packages("IDPmisc")
library("IDPmisc")

# Omit data
anime2 <- NaRV.omit(anime)

# Run log() on new data
anime2$scored_byLOG2 <- log(anime2$scored_by)

# The two lines put out the same normally distributed transformed data
plotNormalHistogram(anime2$scored_byLOG2)

plotNormalHistogram(anime2$scored_byLOG)








