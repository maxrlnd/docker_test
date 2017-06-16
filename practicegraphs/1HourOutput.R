GraphOutput = read.csv("practicegraphs/practiceGameOutputs.csv")


library("plyr")
GraphOutput = plyr::rename(GraphOutput, c("herd" = "herdsize", "yr" = "year"))
names(GraphOutput)
