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


# Now Transform Negatively Skewed Data
#
#
#
#
#

# View aired_from_year
plotNormalHistogram(anime$aired_from_year)

# Transform the data using square
anime$aired_from_yearSQ <- anime$aired_from_year ^2

# view transformed
plotNormalHistogram(anime$aired_from_yearSQ)
# The squre transformation is still skewed negative

# Transform the data using cube
anime$aired_from_yearCube <- anime$aired_from_year ^3
plotNormalHistogram(anime$aired_from_yearCube)
# It is still not normally distributed by it looks much better. 

#
#
#
#
#
#

# Tukey's Ladder of Power Transformation in library("rcompanion")

plotNormalHistogram(cruise_ship$Tonnage)
# This data is naturally postively skewed

# Use Tukey's Ladder of Power with argument plotit = FALSE
cruise_ship$TonnageTukey <- transformTukey(cruise_ship$Tonnage, plotit = FALSE)
plotNormalHistogram(cruise_ship$TonnageTukey)
# Now the data is normally distributed

# Try Tukey with the plotit = True
transformTukey(cruise_ship$Tonnage, plotit = TRUE)

#The Square Root show the most normal distribution

#
#
#
#
#
#


