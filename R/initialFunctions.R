getInitialValues <- function(simvars){
  sim_results <- data.table(matrix(0, 1, 17))
  setnames(sim_results, c("yr","rev.calf", "rev.ins", "rev.tot", "cost.op", "cost.ins", "cost.adpt",
                          "cost.int", "cost.tot", "profit", "taxes", "aftax.inc", "cap.sales",
                          "cap.purch", "cap.taxes", "assets.cow", "assets.cash", "net.wrth", "wn.succ", "forage.potential",
                          "herd", "calf.sold") )
  sim_results[, assets.cow := CalcCowAssets(t = 1, herd = herd, p.cow = p.cow)]
  sim_results[, herd := simvars$herd]
  
}