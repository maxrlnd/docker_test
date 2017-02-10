getAdaptCost <- function(adpt_choice, pars, days.act, current_herd, intense.adj){
  adpt_cost <- 0
  if(adpt_choice == "feed"){
    adpt_cost <- with(pars, CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.act, current_herd, intens.adj)
  }else if(adpt_choice == "rentpast"){
    cost.op.rentpast <- with(pars, CalculateRentPastCost(n.miles = n.miles,
                                              truck.cost = truck.cost,
                                              past.rent = past.rent,
                                              days.rent = days.act,
                                              oth.cost = oth.cost,
                                              max.wt = max.wt,
                                              cow.wt = cow.wt,
                                              calf.wt = calf.wt,
                                              herd = current_herd))
  }else if(adpt_choice == '')
}


