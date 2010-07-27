hoursAgg<-
	function(data,
			process,
			multiple=NULL,
			na.rm=FALSE,
			tz="GMT"){

		
          if(!is.null(multiple)){
          	if(multiple>1){
            dates <- as.POSIXct(strptime(paste(data$hour,data$day,data$month,data$year),"%H %d %m %Y"),tz=tz)
            ## Get the gep between each hour.
            gap <- as.numeric(difftime(dates[2:length(dates)],dates[1:length(dates)-1]),units="hours")
            cum.gap <- gap
            for(i in 2:length(cum.gap)){
              cum.gap[i]=cum.gap[i-1]+cum.gap[i]+data$hour[1]
            }
            cum.gap <- c(data$hour[1],cum.gap)
            data$hour<-cum.gap-cum.gap%%multiple-cum.gap[1]
            #Convert the above hours from date[1] to dates.
           data$dates <- data$dates[1]+data$hour*60*60
           
             temp <- timeSeries(data$dates,"%Y-%m-%d %H:%M:%S",data[,8:length(data)],tz=tz)
            data <- temp
            }else{
            	data <- data
            	        
          }}
          
          result<-aggregate(data[,8:length(data)],list(day=data$day,month=data$month,year=data$year,hour=data$hour),process,na.rm=na.rm)

         data2 <- data.frame(Date=strptime(paste(00,result$hour,result$day,result$month,result$year),"%M %H %d %m %Y",tz=tz),result[,5:length(result)])
        sorted <- data2[order(data2$Date),]
        final <- data.frame(Date=as.POSIXlt(sorted$Date),sorted[,2:length(sorted)])
        names(final)<-c("Date",names(data[8:length(data)]))
          return(final)
        }
