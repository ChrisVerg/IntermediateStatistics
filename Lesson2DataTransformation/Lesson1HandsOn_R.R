# Lesson 2 Hands On Part 2

# Using the Seattle_ParksnRec Data set assess and transform the data for the 
# following variables as needed. 

# # of trips Fall
# # of participants Fall
# # of trips per Year
# # participants per Year
# increase/decrease of prior year
# Average # people per trip


# Load data set

# Load package
library("rcompanion")

parks <- Seattle_ParksnRec
View(parks)




# Fall Trips

# View Distribution
plotNormalHistogram(parks$`# of trips Fall`)
# Fall trips has a positive Skew

# Square Root Transformation
parks$fallTripsSQRT <- sqrt(parks$`# of trips Fall`)
plotNormalHistogram(parks$fallTripsSQRT)
# This is slightly more normal but still positive skew

# Logarithmic Transformation
parks$fallTripsLOG <- log(parks$`# of trips Fall`)
plotNormalHistogram(parks$fallTripsLOG)
# This looks nice and normal

#
#
#
#
#

# Fall Participants

# View Distribution
plotNormalHistogram(parks$`# of participants Fall`)
# This is close to normal with a postive skew

# Square Root Transformation
parks$fallParticSQRT <- sqrt(parks$`# of participants Fall`)
plotNormalHistogram(parks$fallParticSQRT)
# This looks very close to normal distribution

# Logarithmic Transformation
parks$fallParticLOG <- log(parks$`# of participants Fall`)
plotNormalHistogram(parks$fallParticLOG)
# This has a slight negative skew

#
#
#
#
#

# Trips per Year
plotNormalHistogram(parks$`# of trips per year`)
# Positive Skew

# Square Root Transformation
parks$annualTripsSQRT <- sqrt(parks$`# of trips per year`)
plotNormalHistogram(parks$annualTripsSQRT)
# More normal but still with positive skew

# Logarithmic Transformation
parks$annualTripsLOG <- log(parks$`# of trips per year`)
plotNormalHistogram(parks$annualTripsLOG)
# This appears to be the closest to normal distribution

#
#
#
#
#

# Participants per Year
plotNormalHistogram(parks$`# participants per year`)
# Positive Skew

# Square Root Transformation
parks$annualParticSQRT <- sqrt(parks$`# participants per year`)
plotNormalHistogram(parks$annualParticSQRT)
# More normal but still with positive skew

# Logarithmic Transformation
parks$annualParticLOG <- log(parks$`# participants per year`)
plotNormalHistogram(parks$annualParticLOG)
# This appears to be the closest to normal distribution

#
#
#
#
#

# Year over Year change
plotNormalHistogram(parks$`increase/decrease of prior year`)
# This appears to be very close to normal distribution

#### NaN interfering with analysis. 
library("IDPmisc")
parks2 <- NaRV.omit(parks)

# Square Root Transformation
parks2$YoYSQRT <- sqrt(parks2$`increase/decrease of prior year`)
plotNormalHistogram(parks2$YoYSQRT)
# Less normal with positive skew

# Logarithmic Transformation
parks2$YoYLOG <- log(parks2$`increase/decrease of prior year`)
plotNormalHistogram(parks2$YoYLOG)
# This has negative skew

#
#
#
#
#

# Average Travelers per Trip (TpT)
plotNormalHistogram(parks$`Average # people per trip`)
# This appears very close to normal distribution slightly Leptokurtic and 
# slightly positive

# Square Root Transformation
parks$TpTSQRT <- sqrt(parks$`Average # people per trip`)
plotNormalHistogram(parks$TpTSQRT)
# This appears to be the best normal distribution

# Logarithmic Transformation
parks$TpTLOG <- log(parks$`Average # people per trip`)
plotNormalHistogram(parks$TpTLOG)
# Slight negative skew



