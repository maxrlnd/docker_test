"
#~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~
A function to randomly select the lottery winner and their prize

Earth Lab Risk Team 
Travis Williams
###~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~
"
files = list.files()
results = read.csv(files[4])

#A few different ways, Here we randomly select an entire row, then create a subset from columns
lotteryChoice = function(results){
  row = as.data.frame(sample_n(results,1))
  questions = as.data.frame(t(row[,43:51]))
  lotteryquestion = sample_n(questions,1)
  print(lotteryquestion)
}

q = lotteryChoice(results)
