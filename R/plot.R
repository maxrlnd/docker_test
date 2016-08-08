library(ggplot2)
library(RColorBrewer)
outcomes=data.frame(outcomes,stringsAsFactors = F)
outcomes[,c("t","aftax.inc")]=sapply(outcomes[,c("t","aftax.inc")],function(X)as.numeric(as.character(X)))
ggplot(data=outcomes,aes(x=t,y=aftax.inc,group=opt,colour=opt))+
  geom_line()
  # +scale_colour_identity(brewer.pal(length(levels(outcomes$opt)),"Dark2"),values=outcomes$opt)
