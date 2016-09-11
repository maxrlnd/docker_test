"
Basic script for simulating the 
ranch-drought model multiple times
with random starting years
"

random.starts=T # triggers random draw of starting years
runs=10

masterRunner<-function(){
  source("master.R")
  # cat(styear,"\n") # just to test
  outc=list(outcomes)
  names(outc)=styear
  return(outc)
}

# Simulate outcomes
simResults=replicate(runs,masterRunner())
