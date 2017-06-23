cperGameOutputs = read.csv("results/cperGameOutputs.csv")
#install.packages("magrittr")
library(magrittr)
#library(lattice)
#library(ggplot2)
library(dplyr)

cperGameOutputs <- cperGameOutputs %>% subset(yr != 0) %>% subset(yr != 2008)

#dont capitalize first 

cperGameOutputs$Insurance <- ifelse(cperGameOutputs$cost.ins > 0, 1, 0)
cperGameOutputs$adapt_choice = NULL
cperGameOutputs$cap.purch = NULL
cperGameOutputs$forage.productione = NULL
cperGameOutputs$household.exp = NULL

ttestdata = function(frame){
  #takes in results, outputs out ttest results
  cat("Follow below prompts to generate t-test results for any variable in the dataset.")
  continue = "y"
  while(TRUE){
    if (continue == tolower("y")){
      cat("Variables from said dataset")
      colnames1 = frame %>% as.data.frame() %>% colnames()
      print(colnames1)
      #readline pops up thes tring inthe console, the value you enter becomes the object you set it to. REadline outputs a character of the number given.
      variable1 = readline(prompt = "Enter the number of the dependent variable? ")
      variable2 = readline(prompt = "Enter the number of the independent/explanatory variable? ")
      index1 = as.numeric(variable1)
      index2 = as.numeric(variable2)
      vari1 = as.name(colnames1[index1])
      vari2 = as.name(colnames1[index2])
      t_test_results = t.test(frame$`vari1`, frame$`vari2`)
      print(t_test_results$p.value)
      continue = readline(prompt = "Do you want to continue? Y/N")
      if(tolower(continue) == "y"){print("Okay, starting over")}
      else{
        print(paste("Okay, returning the chart for",t_test_results))
        return(t_test_results)
    }
  }
  }
  }
  q

colnames2 = 

ggplot(cperGameOutputs, aes(herd)) +
  geom_bar(width = 5, position = "dodge")

#test2 = lm(herd ~ Insurance, data = cperGameOutputs)

xyplot(net.wrth ~ Insurance, data = cperGameOutputs)

xyplot(rangeHealth ~ herd, data = cperGameOutputs)



ttest = t.test(cperGameOutputs$Insurance, cperGameOutputs$net.wrth)

cor(cperGameOutputs$Insurance, cperGameOutputs$net.wrth)

ggplot(data = cperGameOutputs, aes(vari2,vari1)) + geom_line()

