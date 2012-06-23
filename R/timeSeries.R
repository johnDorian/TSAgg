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
    minute <- minute(dates)
    hour <- hour(dates)
    day <- day(dates)
    week <- week(dates)
    month <- month(dates)
    year <- year(dates)
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
