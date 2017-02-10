

# Drought Adaptation Functions --------------------------------------------

CalculateDaysAction <- function(act.st.yr, act.st.m, act.end.yr, act.end.m, drought.action) {
  "
  Function: CalculateDaysAction
  Description: Calculate the number of days rancher pays for a drought adaptation action.
  NOTE: This function assumes that the actions take place only in one year.
  
  Inputs:
  act.st.yr = Year the action starts
  act.st.m = Month the action starts
  act.end.yr = Year the action ends
  act.end.m = Month the action ends
  drought.action = ??
  
  Outputs:
  days.act = Number of days drought adaptation action takes place (days)
  "
  
  # Start and End month assumes that action starts/stops on the first of the month
  
  # Error handling
  if (act.st.yr < 1) {
    stop("Action start year ", act.st.yr, "is not greater than 0")
  }
  if (act.st.yr > act.end.yr | act.st.yr == act.end.yr & act.st.m > act.end.m) {
    stop("Action end occurs before action start")
  }
  if (act.st.m < 1 | act.st.m > 12 | act.end.m < 1 | act.end.m > 12) {
    stop("Invalid action start or end month (not between 1 and 12)")
  }
  
  if (act.st.yr == act.end.yr) {
    days.act <- (act.end.m - act.st.m) * 30
  }
  
  # TEMPORARY ERROR: While the model is not equipped to handle multi-year droughts, we will not allow drought
  #  adaptation across multiple years.
  if (act.st.yr != act.end.yr) {
    stop("Model not equipped for multi-year drought adaptation. act.st.yr must equal act.end.yr")
  }
  
  days.act.vect <- days.act * drought.action #Creates a vector of days of action
  days.act.vect
}

CalculateAdaptationIntensity <- function(forage.potential, drought.adaptation.cost.factor = 1) {
  " Description: Takes forage potential and an adaptation intensity factor to 
  provide a scalar of drought action. If forage potential is above 1 (no drought), 
  then this variable goes to 0 (no adaptation). 
  Inputs: 
  adpt.intensity.factor (parameter that scales adaptation actions to reflect 
  actual adaptation behavior. Currently defaults to 1 which assumes a 
  one-to-one ratio of drops in forage percentage to need for forage 
  replacement.)
  forage.potential (the percentage of average forage produced in a year 
  based on rainfall. See forage potential functions.)
  Output: intens.adj (scales action to account for forage potential's 
  deviation from the norm.)
  Assumptions: The variable has a maximum of 1, which assumes that drought 
  actions are parameterized at full forage replacement for the full herd.
  "
  intens.adj <- ifelse(forage.potential >= 1, 0, (1 - forage.potential) * drought.adaptation.cost.factor)
  intens.adj <- ifelse(intens.adj > 1, 1, intens.adj)  # putting a ceiling of this variable at 1 (no more than 100% of drought action)
  intens.adj
}

