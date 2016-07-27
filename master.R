# Copyright (c) 2016 Trisha Shrum, Joseph Tuccillo
#
# Authors Comment: This model is jointly developed at the University of 
#  Colorado Earth Lab based on work by Adam ??, Joseph Tuccillo, Kelly Carney, 
#  Bill Travis, Jeffrey Tranel, Rod Sharp, and John Deering.
#
# Description: This script implements a simulation of drought adaptation
#  decisions by Western cattle ranchers. 
#
# Inputs:
#   ...
#
# Outputs:
#   ...

# Setting input values to defaults in excel file (temporary placeholder)
kHayLbs <- 22
kOthLbs <- 0
p.hay <- 100  # This should be a user input variable
p.oth <- 0  # This should be a user input variable
days.feed <- 180  # This needs to be calculated separately!!
herd <- 600 # User input variable

# Source functions
source("R/feed.R")

# Ancillary source scripts


# Main Script

# Option 1: Buy additional feed
feed.cost <- CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed, herd)  # Calculates additional costs to feed herd
