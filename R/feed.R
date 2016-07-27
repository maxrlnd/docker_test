# Description: Calculating the costs of purchasing additional feed

# Inputs:
#  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
#  p.hay = Price of hay ($/ton). User input.
#  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
#  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
#  days.feed = Number of days additional feed is needed. This needs to be calculated separately.
#  herd = Size of herd (head of cows, does not include calves)
#
# Outputs:
# feed.cost = Total feed costs for the herd over the remainder of the season


CalculateFeedCost <- function(kHayLbs,kOthLbs,p.hay,p.oth,days.feed,herd) {
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd
  return(feed.cost)
}
