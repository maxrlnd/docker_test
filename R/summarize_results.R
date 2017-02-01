#### Summary ####
# quick summary of output for final year networth by option and insurance
finalyr <- parouts[parouts$yr == 5,]
networth <- summarize(group_by(finalyr, opt, ins), avg.netwrth = mean(net.wrth))

# Expected utility, quick pass
exp_utility <- select(parouts, opt, yr, ins, aftax.inc, net.wrth, sim.index) %>%
  filter(yr > 0)
exp_utility$inc.min1 <- ifelse(exp_utility$aftax.inc <= 0, 1, exp_utility$aftax.inc) 
exp_utility$inc.util <- log(exp_utility$inc.min1)
exp_utility$wealth.util <- log(exp_utility$net.wrth)
eu <- group_by(exp_utility, ins, opt, sim.index) %>%
  summarize(exp.util.inc = mean(inc.util), exp.util.wealth = mean(wealth.util))

d <- group_by(eu, ins) %>%
  density(exp.util.wealth)

eu.noins <- filter(eu, ins==0, opt == "noadpt")
eu.ins <- filter(eu, ins==1, opt == "noadpt")
summarize(eu.noins, m = mean(exp.util.wealth))
summarize(eu.ins, m = mean(exp.util.wealth))
summarize(eu.noins, m = mean(exp.util.inc))
summarize(eu.ins, m = mean(exp.util.inc))

p <- eu.noins$exp.util.inc
d <- density(p)
plot(d)

parouts %>% filter(opt == "noadpt", yr == 5) %>% select(opt, ins, net.wrth) ->  noadpt.results
write.csv(noadpt.results, file="output/noadapt_montecarlo.csv")