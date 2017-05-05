cpermlra <- c(0.0, 0.0, 0.02, 0.08,0.20,0.28,0.15,0.12,0.10,0.05,0.0,0.0) #changed sep from .11 to .10months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
cper <- matrix(cpermlra, nrow = 1, ncol = 12)
colnames(cper) <- months

