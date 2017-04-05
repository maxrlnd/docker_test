

library(readr)

fp <- read_delim("data/co_mlra.txt", delim = "|", 
                 col_names = c("growth_curve", "description", "other", "jan", 
                               "feb", "mar", "apr", "may", "jun", "jul", "aug",
                               "sep", "oct", "nov", "dec"))

fp_67B <- filter(fp, grepl("67B", description))
fp <- summarise(fp_67B, Jan = mean(jan), Feb = mean(feb), Mar = mean(mar), Apr = mean(apr),
          May = mean(may), Jun = mean(jun), Jul = mean(jul), Aug = mean(aug), 
          Sep = mean(sep), Oct = mean(oct), Nov = mean(nov), Dec = mean(dec))
