"timeSeries"<-function
(dates, dateformat, data = NULL, tz = "GMT") 
{
	dimensions<-dim(data)
	clss<-lapply(data,class)
	if (!is.null(data)){
		if (!is.null(dimensions[1])){
			if (length(dates)!=length(data[,1]))
				stop("Data and Date lengths differ")
		}else{
			if (length(dates)!=length(data))
				stop("Data and Date lengths differ")
		}
	}	
    dates <- (strptime(paste(dates), dateformat, tz = tz))
    minute <- as.numeric(format(dates, "%M"))
    hour <- as.numeric(format(dates, "%H"))
    day <- as.numeric(format(dates, "%d"))
    week <- as.numeric(format(dates, "%W"))
    month <- as.numeric(format(dates, "%m"))
    year <- as.numeric(format(dates, "%Y"))
    if (is.null(data)) {
        results <- data.frame(dates, minute, hour, day, week, 
            month, year)
    }
    else {
        results <- data.frame(dates, minute, hour, day, week, 
            month, year, data)
    }
    return(results)
}
