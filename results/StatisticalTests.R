cperGameOutputs = read.csv("results/cperGameOutputs.csv")
install.packages("magrittr")
library(magrittr)

cperGameOutputs = cperGameOutputs %>% subset(yr !=0)

cperGameOutputs$Insurance = NA


ifelse(cperGameOutputs$cost.ins > 0, cperGameOutputs$Insurance == "Yes", "No")
