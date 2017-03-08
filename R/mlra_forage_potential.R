install.packages("readr")
library(readr)

fp <- read_delim("data/co_mlra.txt", delim = "|", 
                 col_names = c("growth_curve", "description", "other", "jan", 
                               "feb", "mar", "apr", "may", "jun", "jul", "aug",
                               "sep", "oct", "nov", "dec"))

fp_67B <- filter(fp, grepl("67B", description))
fp <- summarise(fp_67B, jan = mean(jan), feb = mean(feb), mar = mean(mar), apr = mean(apr),
          may = mean(may), jun = mean(jun), jul = mean(jul), aug = mean(aug), 
          sep = mean(sep), oct = mean(oct), nov = mean(nov), dec = mean(dec))
