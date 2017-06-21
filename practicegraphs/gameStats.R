"
#~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~
A function to graph the ranching decision model results.

Earth Lab Risk Team 
Travis Williams
###~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~
"
setwd("C:/Users/Travis/GitHub/drought_decision_model/practicegraphs")
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)

#Precipitation Data
load("C:/Users/Travis/Google Drive/Risk Project/ranch_market_weather/data/monthlyNOAA.RData")
sprinkle = noaaMonthly[noaaMonthly$Year == 2000,]

#GameResults Data
full2 = read.csv("practiceGameOutputs2.csv")
numbers = full2%>%filter(yr>0)#na.omit()%>%
numbers$cost.ins[1:15] = 0
numbers$index = as.numeric(row.names(numbers))
numbers[order(numbers$index), ]
numbers$index[1:5] = 1
numbers$index[6:10] = 2
numbers$index[11:15] = 3
numbers$index[16:20] = 4
numbers$index[21:25] = 5
numbers$index[26:30] = 6
numbers$index[31:35] = 7
numbers$mTurkID = numbers$index
numbers$Insured = "No"
numbers$Insured[which(numbers$cost.ins > 0)] = "Yes"
numbers$Insured = factor(numbers$Insured)

gameStats = function(numbers,year1,year2){
  "
  Take in the decision model results and outputs a dataframe with a graph of a variable of choice 
  Things to do:
  add a legend for indexed, not indexed, Insured and not Insured
  add an Insured aggregate option
  somehow add precipitation?
  Which grid is CPER in?
  select appropriate years
  plot on second y-axis
  
  "
  require(dplyr)
  require(stringr)
  require(ggplot2)
  require(ggthemes)
  names(numbers) = c("Year","Adaptation_Choice","Calf_Revenue","Insurance_Revenue","Interest_Revenue", "Total_Revenue","Operating_Costs","Insurance_Costs","Adaptation_Costs","Interest_Costs","Total_Costs","Profit","Taxes","After_Tax_Income", "Captial_Sale","Capital_Purchases","Capital_Taxes","Herd_Value","Bank_Balance","Net_Worth","Weaning_Percentage","Forage_Production","Herd_Size","Calves_Sold","Cows_Sold","Forage_Potential_Change", "Grazing_Pressure","Forage_Potential","Range_Health","mTurkID", "Sim_Start_Time", "Elapsed_Time", "Household_Expenses", "Hay_Expenses","Total_Forage_Potential","Index","Insured")
  continue = "y"
  cat("Follow the prompts to generate a chart and a tibble for any variable in the dataset. Type 'q' to break out of the function")
  numbers=tbl_df(numbers)
  while (TRUE){
    if (continue == tolower("y")){  
      print(paste("Variables for the results dataset"))
      names = as.data.frame(names(numbers))
      print(names)
      variable = readline(prompt = "Enter the number for the variable that you want a time series of? ")      
      if (tolower(variable =="q")){break}
      index = as.numeric(variable)
      variable = as.character(names[index,])
      how = readline(prompt = "Aggregate or indexed?(Type the word or '1' or '2') ")        
      if(how=="indexed"|how=="2"){ 
        lwd = 1.25
        yr = "Year"
        mTurkID= "mTurkID"
        timeseries = ggplot(data = numbers,aes_string(x=yr,y=variable,group = mTurkID,color="Insured",fill="Insured"))
      }else{
        lwd = 2
        variable = as.name(variable)
        numbers2 = numbers%>%group_by(Insured,Year)%>%mutate(newvar=mean(variable))%>%distinct(newvar) 
        yr = "Year"
        timeseries = ggplot(data = numbers2,aes_string(x=yr,y="newvar",group = "Insured",color = "Insured"))
      }
      variable = gsub("_"," ",variable)
      plot = timeseries+
        geom_line(lwd=lwd)+
        #geom_smooth()+
        scale_x_continuous(breaks=seq(year1,year2,by = 1)) +
        ylab(toupper(variable))+
        xlab("YEAR")+
        #theme_fivethirtyeight()+
        #theme_economist() #+ scale_colour_economist() +
        #theme_stata() + scale_colour_stata()+
        theme_minimal()+ 
        ggtitle("Results")+      
        scale_fill_manual(values=c("orange","red"))+
        theme(
          axis.text.x = element_text( hjust = 1,face="bold"),#angle = 90,
          axis.text.y = element_text(face="bold"),
          legend.position="right",
          plot.title = element_text(hjust = 0.5,color="black", size=20, face="bold"),
          axis.title.x = element_text(color="black", size=10, face="bold"),
          axis.title.y = element_text(color="black", size=10, face="bold")
        )
      print(plot)
      answer = readline(prompt="start over(Y/N)? ")
      if(tolower(answer)=="q"){break}
      if(tolower(answer)=="y"){print("Okay, starting over")}
      else{
        print(paste("Okay, returning the chart for",variable))
        return(numbers)
      }
      continue == answer
    }
  }
} 
gameStats(numbers,1951,1955)
