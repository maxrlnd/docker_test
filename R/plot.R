library(ggplot2)
library(RColorBrewer)
outcomes=data.frame(outcomes,stringsAsFactors = F)
outcomes[,c("t","aftax.inc")]=sapply(outcomes[,c("t","aftax.inc")],function(X)as.numeric(as.character(X)))
outcomes$opt=as.character(outcomes$opt)

##Assign human-readable names
outcomes$opt=with(outcomes,ifelse(opt=="feed","Buy Feed",
                                  ifelse(opt=="noadpt","Drought and No Adaptation",
                                         ifelse(opt=="nodrght","No Drought",
                                                ifelse(opt=="rentpast","Rent Pasture",
                                                       ifelse(opt=="sellprs","Sell Cows/Replace","Sell Cows/Don't Replace"))))))
outcomes$opt=factor(outcomes$opt,
                    levels=c("No Drought","Drought and No Adaptation","Buy Feed",
                             "Rent Pasture","Sell Cows/Replace","Sell Cows/Don't Replace"))

##Plot
ggplot(data=outcomes,aes(x=t,y=aftax.inc,colour=opt))+
  geom_line(size=1.2)+
  scale_colour_manual(values=brewer.pal(6,"Dark2"))+
  facet_grid(~opt)+
  xlab("Year")+
  ylab("After Tax Income")+
  labs(colour="Scenario")