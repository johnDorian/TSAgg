hoursAgg<-function 
(data, process, multiple = 1, na.rm = FALSE, tz = "GMT") 
{
	gap <- as.numeric(difftime(data$dates,data$dates[1],tz="GMT", units = "hours"))
	agg.gap<-gap-(gap%%multiple)
	data$dates<-data$dates[1] + agg.gap * 60 * 60
	data <- timeSeries(data$dates, "%Y-%m-%d %H:%M:%S",
                     data[, 8:length(data)], tz = tz)
	result <- aggregate(data[, 8:length(data)], 
                      list(day = data$day,month = data$month, 
                           year = data$year, hour = data$hour,
                           minute=data$minute),
                      process, na.rm = na.rm)
  data2 <- data.frame(Date = strptime(
    paste(result$minute, result$hour,result$day, result$month, result$year),
    "%M %H %d %m %Y",tz = tz), result[, 6:length(result)])
  sorted <- data2[order(data2$Date), ]
  final <- data.frame(Date = as.POSIXlt(sorted$Date), 
                      sorted[, 2:length(sorted)])
  names(final) <- c("Date", names(data[8:length(data)]))
  return(final)
}