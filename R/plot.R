"
Scratchpad for generating simple plots of ranch 
drought outcomes. 
"

library(ggplot2)
library(RColorBrewer)

source("master.R") # run the ranch drought model

outcomes.plot=data.frame(outcomes,stringsAsFactors = F)
outcomes.plot=outcomes.plot[outcomes.plot$yr>0,] # exclude base year
# outcomes.plot[,c("t","aftax.inc")]=sapply(outcomes.plot[,c("t","aftax.inc")],function(X)as.numeric(as.character(X)))
outcomes.plot$opt=as.character(outcomes.plot$opt)

##Assign human-readable names
outcomes.plot$opt=with(outcomes.plot,ifelse(opt=="feed","Buy Feed",
                                  ifelse(opt=="noadpt","Drought and No Adaptation",
                                         ifelse(opt=="nodrght","No Drought",
                                                ifelse(opt=="rentpast","Rent Pasture",
                                                       ifelse(opt=="sellprs","Sell Cows/Replace","Sell Cows/Don't Replace"))))))
outcomes.plot$ins=factor(ifelse(outcomes.plot$ins==1,"With Insurance","No Insurance"))
outcomes.plot$opt=factor(outcomes.plot$opt,
                    levels=c("No Drought","Drought and No Adaptation","Buy Feed",
                             "Rent Pasture","Sell Cows/Replace","Sell Cows/Don't Replace"))
outcomes.plot$yr=outcomes.plot$yr+styear
outcomes.plot$aftax.inc=round(outcomes.plot$aftax.inc/1000,2) # truncate income text 

##Plot
ggplot(data=outcomes.plot,aes(x=yr,y=aftax.inc,colour=opt))+
  geom_hline(yintercept=0,linetype=2)+
  geom_line(size=1.2,alpha=0.8)+
  scale_colour_manual(values=brewer.pal(6,"Dark2"))+
  facet_grid(ins~opt)+
  xlab("Year")+
  ylab("After Tax Income ($1000 USD)")+
  theme(axis.text.x=element_text(angle=45))+
  labs(colour="Scenario")