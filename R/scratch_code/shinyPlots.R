loadPlots <- function(input, output, session, i, rv){  
  ## Table of rain for each July
  output[[paste0("julyRain", i)]] <- renderTable({
    currentYear <- (startYear + i - 1)
    yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
    yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                     (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
    yprecip[, 9:12 := 0]
    ave <- station.gauge$avg
    yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
    julyRain <- round((yearAvg[1,]/yearAvg[2,]) * 100, 2)
    julyRain[, 9:12 := "?"]
    # julyRain <- station.gauge$stgg[Year == (startYear + i - 1),-1]/station.gauge$avg * 100
    # julyRain[, 7:12 := "?"]
  })
  
  
  ## Bar graph to display net worth
  output[[paste0("worthPlot", i)]] <- renderPlot({
    plotOuts <- myOuts[, c("yr", "assets.cow", "assets.cash"), with = F]
    setnames(plotOuts, c("Year", "Value of Cows", "Cash"))
    plotOuts[, Year := startYear:(startYear + nrow(plotOuts) - 1)]
    plotOuts <- melt(plotOuts, id.vars = "Year")
    setnames(plotOuts, c("Year", "Area", "Value in $"))
    plotOuts$Area <- factor(plotOuts$Area)
    plotOuts$YearNumbers <-  paste("Year", plotOuts$Year - min(plotOuts$Year) + 1)
    plotOuts$YearNumbers <- factor(plotOuts$YearNumbers, 
                                   levels = paste("Year", seq_along(unique(plotOuts$Year))))
    
    ggplot(plotOuts, aes(x = YearNumbers, y = `Value in $`, fill = Area)) + geom_bar(stat = "identity") + 
      ggtitle("Net Worth") + 
      scale_y_continuous(labels = comma) +
      geom_text(data = subset(plotOuts, `Value in $` !=0), aes(label = dollar(`Value in $`)), 
                size = 5, position = position_stack(vjust = 0.3), angle = 90) +
      theme(legend.title = element_blank(), axis.title = element_text(size = 20), text = element_text(size = 20)) +
      scale_fill_manual(values = c("#f4a460", "#85bb65"))
    
    
  })
  
  output[[paste0("RangeHealthPlot", i)]] <- renderPlot({
    PlotYear <- myOuts[, "yr", with = F]
    setnames(PlotYear, c("Year"))
    PlotYear[, Year := startYear:(startYear + nrow(PlotYear) - 1)]
    PlotYear <- melt(PlotYear, id.vars = "Year")
    PlotYear$rangeHealthList <- rangeHealthList
    PlotYear$YearNumbers <- c(paste("Year", seq(1, simLength, length.out = simLength)))
    PlotYear$YearNumbers <- factor(PlotYear$YearNumbers, 
                                   levels = paste("Year", seq_along(unique(PlotYear$Year))))
    
    ggplot(PlotYear, aes(x = YearNumbers, y = rangeHealthList)) + 
      geom_bar(stat = "identity", fill = "olivedrab") + 
      ggtitle("Range Health") + 
      labs(x = "Year" ,y = "Range Condition (%)") +
      theme(text = element_text(size = 20)) +
      geom_text(aes(label = paste(rangeHealthList, sep = "", "%")), size = 8, color =  "#ffffff", vjust = 5)
    
    
  })
  
  ## Bar graph to display rainfall
  output[[paste0("rainGraph", i)]] <- renderPlot({
    currentYear <- (startYear + i - 1)
    yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
    yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                     (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
    yprecip[, 9:12 := 0]
    ave <- station.gauge$avg
    yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
    yearAvg[, "id" := c("Actual Rain", "Average Rain")]
    yearAvg <- melt(yearAvg, id.vars = "id")
    setnames(yearAvg, c("id", "Month", "Rainfall"))
    ggplot(yearAvg, aes(x = Month, y = Rainfall, fill = id, label = Rainfall)) + 
      geom_bar(width = .9, stat = "identity", position = 'dodge') + 
      #facet_wrap(~ id) +
      #geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      theme(legend.title = element_blank(), text = element_text(size = 15)) + ggtitle("Rainfall") +
      scale_fill_manual(values = c("#00008b", "#1e90ff")) +
      ylab("Rainfall (Inches)")
    #position_dodge(width=1.3)
    #Rain color it!
    
  })
  
  ## Bar graph to display rainfall with July and August added
  output[[paste0("rainGraphSep", i)]] <- renderPlot({
    currentYear <- (startYear + i - 1)
    yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
    yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                     (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
    yprecip[, 11:12 := 0]
    ave <- station.gauge$avg
    yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
    yearAvg[, "id" := c("Actual Rain", "Average Rain")]
    yearAvg <- melt(yearAvg, id.vars = "id")
    setnames(yearAvg, c("id", "Month", "Rainfall"))
    
    #Setting output to only highlight July and August
    yprecip1 = yprecip[, (1:12)[-seq(9,10)] :=0]
    ave1 <- station.gauge$avg
    yearAvg1 <- rbindlist(list(yprecip1, ave1), use.names = T)
    yearAvg1[, "id" := c("Actual Rain", "Average Rain")]
    yearAvg1 <- melt(yearAvg1, id.vars = "id")
    setnames(yearAvg1, c("id", "Month", "Rainfall"))
    
    #ggplot for July and August
    ggplot(yearAvg, aes(x = Month, y = Rainfall, area = id)) + 
      geom_bar(width = .9, stat = "identity", position = 'dodge', alpha = .5) + 
      theme(legend.title = element_blank(), text = element_text(size = 15)) + ggtitle("Rainfall") +
      scale_fill_manual(values = c("#00008b", "#1e90ff")) +
      scale_color_manual(values = c("grey50", "grey50")) +
      ylab("Rainfall (Inches)") +
      xlab("Months") + 
      #Geom__bar only highlighting July and August
      geom_bar(data = yearAvg1, aes(x = Month, y = Rainfall, fill = id), stat = "identity", position = 'dodge')
    
  })
}