# Analyze each continuous variable in the cruise_ship dataset and 
# perfrom transformations to get them as close to normal distribution as possible.

library("rcompanion")
colnames(cruise_ship)
# The Continuous Variables are YearBlt , Tonnage , passngrs , Length ,
# Cabins , Crew , PassSpcR , outcab , TonnageTukey


#
# YearBlt
plotNormalHistogram(cruise_ship$YearBlt)
# Negative distribution

# Create new column of squared transformation
cruise_ship$YearBltSQ <- cruise_ship$YearBlt^2
plotNormalHistogram(cruise_ship$YearBltSQ)
# Still showing a negative distribution

# Create new column of cubed transformation
cruise_ship$YearBltCubed <- cruise_ship$YearBlt^3
plotNormalHistogram(cruise_ship$YearBltCubed)
# Still showing a negative distribution but is closer to normal distribution

# Try it using Tukeys Ladder of Power
cruise_ship$YearBltTukey <- transformTukey(cruise_ship$YearBlt, plotit = FALSE)
plotNormalHistogram(cruise_ship$YearBltTukey)
# Still appears normally distributed similar to the cubed, but with a dip



#Tonnage
#
#
plotNormalHistogram(cruise_ship$Tonnage)
# Positive Skew

# Create new column of square root transformation
cruise_ship$TonnageSQRT <- sqrt(cruise_ship$Tonnage)
plotNormalHistogram(cruise_ship$TonnageSQRT)
# This make the positive distribution more Normal. 

# Create new column of Log transformation
cruise_ship$TonnageLog <- log(cruise_ship$Tonnage)
plotNormalHistogram(cruise_ship$TonnageLog)
# Log appears to create a negative distribution

# Try it using Tukeys Ladder of Power
cruise_ship$YearBltTukey <- transformTukey(cruise_ship$YearBlt, plotit = FALSE)
plotNormalHistogram(cruise_ship$YearBltTukey)



#The Square Root show the most normal distribution

#
#
#


# Passengers or passngrs

plotNormalHistogram(cruise_ship$passngrs)
# Positive Skew

# Create new column of square root transformation
cruise_ship$passngrsSQRT <- sqrt(cruise_ship$passngrs)
plotNormalHistogram(cruise_ship$passngrsSQRT)
# This make the positive distribution more Normal. 

# Create new column of Log transformation
cruise_ship$passngrsLog <- log(cruise_ship$passngrs)
plotNormalHistogram(cruise_ship$passngrsLog)
# Log appears to create a negative distribution

# Try it using Tukeys Ladder of Power
cruise_ship$passngrsTukey <- transformTukey(cruise_ship$passngrs, plotit = FALSE)
plotNormalHistogram(cruise_ship$passngrsTukey)
# Tukeys make it more normal but less so than the square root

# The Square Root transforms to the most normal distribution. 

#
#
#
#
#

# Length
plotNormalHistogram(cruise_ship$Length)
# This is close to normal but still slightly negative

# Create new column of squared transformation
cruise_ship$LengthSQ <- cruise_ship$Length^2
plotNormalHistogram(cruise_ship$LengthSQ)
# Mostly Normally distributed

# Create new column of cubed transformation
cruise_ship$LengthCubed <- cruise_ship$Length^3
plotNormalHistogram(cruise_ship$LengthCubed)
# Normal but slightly postive skew

# Try it using Tukeys Ladder of Power
cruise_ship$LengthTukey <- transformTukey(cruise_ship$Length, plotit = FALSE)
plotNormalHistogram(cruise_ship$LengthTukey)
# Still appears normally distributed similar to the cubed, but with a dip

# Squared Transformation appears to show the best transformation to normal distribution

#
#
#
#
#
#
#

# Cabins
plotNormalHistogram(cruise_ship$Cabins)
# This is close to normal but with positive skew

# Create new column of square root transformation
cruise_ship$CabinsSQRT <- sqrt(cruise_ship$Cabins)
plotNormalHistogram(cruise_ship$CabinsSQRT)
# This make the positive distribution more Normal. 

# Create new column of Log transformation
cruise_ship$CabinsLog <- log(cruise_ship$Cabins)
plotNormalHistogram(cruise_ship$CabinsLog)
# Log appears to create a negative distribution

# Try it using Tukeys Ladder of Power
cruise_ship$CabinsTukey <- transformTukey(cruise_ship$Cabins, plotit = FALSE)
plotNormalHistogram(cruise_ship$CabinsTukey)
# Tukeys make it more normal with some positive skew

# The Square Root transforms to the most normal distribution. 

#
#
#
#
#
# Crew
plotNormalHistogram(cruise_ship$Crew)
# This close to normal distribution with positive skew

# Create new column of square root transformation
cruise_ship$CrewSQRT <- sqrt(cruise_ship$Crew)
plotNormalHistogram(cruise_ship$CrewSQRT)
# Mostly normally distributed. 

# Create new column of Log transformation
cruise_ship$CrewLog <- log(cruise_ship$Crew)
plotNormalHistogram(cruise_ship$CrewLog)
# Log appears to create a negative skew

# Try it using Tukeys Ladder of Power
cruise_ship$CrewTukey <- transformTukey(cruise_ship$Crew, plotit = FALSE)
plotNormalHistogram(cruise_ship$CrewTukey)
# Tukeys make it more normal with some positive skew

# The Square Root transforms to the most normal distribution. 

#
#
#
#
#
#
#
#


# PassSpcR
plotNormalHistogram(cruise_ship$PassSpcR)
# Appears normally distributed with slight positive skew

# Create new column of square root transformation
cruise_ship$PassSpcRSQRT <- sqrt(cruise_ship$PassSpcR)
plotNormalHistogram(cruise_ship$PassSpcRSQRT)
# Normally distributed. 

# Create new column of Log transformation
cruise_ship$PassSpcRLog <- log(cruise_ship$PassSpcR)
plotNormalHistogram(cruise_ship$PassSpcRLog)
# Normally Distributed

# Try it using Tukeys Ladder of Power
cruise_ship$PassSpcRTukey <- transformTukey(cruise_ship$PassSpcR, plotit = FALSE)
plotNormalHistogram(cruise_ship$PassSpcRTukey)
# Tukeys is Normally Distributed

# Most of the transformations yield normally distributed data

#
#
#
#
#
#

# outcab
plotNormalHistogram(cruise_ship$outcab)
# Positive Skew

# Create new column of square root transformation
cruise_ship$outcabSQRT <- sqrt(cruise_ship$outcab)
plotNormalHistogram(cruise_ship$outcabSQRT)
# Normally distributed. 

# Create new column of Log transformation
cruise_ship$outcabLog <- log(cruise_ship$outcab)
plotNormalHistogram(cruise_ship$outcabLog)
# Negative Skew

# Try it using Tukeys Ladder of Power
cruise_ship$outcabTukey <- transformTukey(cruise_ship$outcab, plotit = FALSE)
plotNormalHistogram(cruise_ship$outcabTukey)
# slight positive skew

# Square root transformation yields normally distributed data
