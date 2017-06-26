cperGameOutputs = read.csv("results/cperGameOutputs.csv")
install.packages("magrittr")
library(magrittr)
library(lattice)
#library(ggplot2)
library(dplyr)

cperGameOutputs <- cperGameOutputs %>% subset(yr != 0) %>% subset(yr != 2008) %>% subset(mTurkID != "practiceRound")
cperGameOutputs$Insurance <- ifelse(cperGameOutputs$cost.ins > 0, 1, 0)
cperGameOutputs$adapt_choice = NULL
cperGameOutputs$cap.purch = NULL
cperGameOutputs$forage.productione = NULL
cperGameOutputs$household.exp = NULL



cper1 = cperGameOutputs %>% subset(mTurkID ==  2943589)
cper2 = cperGameOutputs %>% subset(mTurkID ==  2887087)
cper3 = cperGameOutputs %>% subset(mTurkID ==  2726622)
cper4 = cperGameOutputs %>% subset(mTurkID ==  2698339)
cper5 = cperGameOutputs %>% subset(mTurkID ==  2644158)
cper6 = cperGameOutputs %>% subset(mTurkID ==  2627698)
cper7 = cperGameOutputs %>% subset(mTurkID ==  2399964)
cper8 = cperGameOutputs %>% subset(mTurkID ==  2152241)
cper9 = cperGameOutputs %>% subset(mTurkID ==  2000006)
cper10 = cperGameOutputs %>% subset(mTurkID ==  1983352)
cper11 = cperGameOutputs %>% subset(mTurkID ==  1654024)
cper12 = cperGameOutputs %>% subset(mTurkID ==  1567996)
cper13 = cperGameOutputs %>% subset(mTurkID ==  1506846)
cper14 = cperGameOutputs %>% subset(mTurkID ==  1358361)
cper15 = cperGameOutputs %>% subset(mTurkID ==  1346874)
cper16 = cperGameOutputs %>% subset(mTurkID ==  1254603)

cper1o = data.frame(herd = as.numeric(mean(cper1$herd)))
cper1o$insurance = 0

cper2o = data.frame(herd = as.numeric(mean(cper2$herd)))
cper2o$insurance = 0

cper3o = data.frame(herd = as.numeric(mean(cper3$herd)))
cper3o$insurance = 1

cper4o = data.frame(herd = as.numeric(mean(cper4$herd)))
cper4o$insurance = 0

cper5o = data.frame(herd = as.numeric(mean(cper5$herd)))
cper5o$insurance = 0

cper6o = data.frame(herd = as.numeric(mean(cper6$herd)))
cper6o$insurance = 0

cper7o = data.frame(herd = as.numeric(mean(cper7$herd)))
cper7o$insurance = 1
  
cper8o = data.frame(herd = as.numeric(mean(cper8$herd)))
cper8o$insurance = 0  

cper9o = data.frame(herd = as.numeric(mean(cper9$herd)))
cper9o$insurance = 0

cper10o = data.frame(herd = as.numeric(mean(cper10$herd)))
cper10o$insurance = 1

cper11o = data.frame(herd = as.numeric(mean(cper11$herd)))
cper11o$insurance = 1

cper12o = data.frame(herd = as.numeric(mean(cper12$herd)))
cper12o$insurance = 1

cper13o = data.frame(herd = as.numeric(mean(cper13$herd)))
cper13o$insurance = 1

cper14o = data.frame(herd = as.numeric(mean(cper14$herd)))
cper14o$insurance = 1

cper15o = data.frame(herd = as.numeric(mean(cper15$herd)))
cper15o$insurance = 1

cper16o = data.frame(herd = as.numeric(mean(cper16$herd)))
cper16o$insurance = 1


cperherdoutput = rbind(cper1o, cper2o, cper3o, cper4o, cper5o, cper6o, cper7o, cper8o, cper9o, cper10o, cper11o, cper12o, cper13o, cper14o, cper15o, cper16o)


test = lm(herd ~ insurance, data = cperherdoutput)

summary(test)$coefficients

test2 = lm(rangeHealth ~ herd, data = cperGameOutputs )
test3 = lm(cows.culled ~ calves.sold, cperGameOutputs)

cperGameOutputs$rangeHealth = as.numeric(cperGameOutputs$rangeHealth)
cperGameOutputs$herd = as.numeric(cperGameOutputs$herd)


xyplot(calves.sold ~ cows.culled, cperGameOutputs)


t.test(cperGameOutputs$net.wrth ~ cperGameOutputs$Insurance)


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

