yr3 <- outs[outs$yr==3,]

ggplot(yr3, aes(x = net.wrth)) + geom_density(aes(group=ins, fill = ins, alpha = .1))
ggplot(yr3, aes(x = net.wrth)) + geom_density(aes(group=opt, fill = opt, alpha = .1))

noins <- yr3[yr3$ins==1,]
ggplot(noins, aes(x = net.wrth)) + geom_density(aes(group=opt, fill = opt, alpha = .1))
ins <- yr3[yr3$ins==0,]
ggplot(ins, aes(x = net.wrth)) + geom_density(aes(group=opt, fill = opt, alpha = .1))
