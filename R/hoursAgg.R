hoursAgg<-
	function(data,
			process,
			multiple=NULL,
			na.rm=FALSE){

		
          if(!is.null(multiple)){
          	if(multiple>1){
            dates <- as.POSIXct(strptime(paste(data$hour,data$day,data$month,data$year),"%H %d %m %Y"))
            ## Get the gep between each hour.
            gap <- as.numeric(difftime(dates[2:length(dates)],dates[1:length(dates)-1]),units="hours")
            cum.gap <- gap
            for(i in 2:length(cum.gap)){
              cum.gap[i]=cum.gap[i-1]+cum.gap[i]+data$hour[1]
            }
            cum.gap <- c(data$hour[1],cum.gap)
            data$hour<-cum.gap-cum.gap%%multiple-cum.gap[1]
            #Convert the above hours from date[1] to dates.
            for(i in 1:length(data[,1])){
              data$dates[i] <- data$dates[1]+data$hour[i]*60*60
            }
            temp <- timeSeries(data$dates,"%Y-%m-%d %H:%M:%S",data$data)
            data <- temp
            }else{
            	data <- data
            	        
          }}
          
          result<-aggregate(list(data=data$data),list(day=data$day,month=data$month,year=data$year,hour=data$hour),process,na.rm=na.rm)
         data <- data.frame(Date=strptime(paste(00,result$hour,result$day,result$month,result$year),"%M %H %d %m %Y"),data=result$data)
        sorted <- data[order(data$Date),]
        final <- data.frame(Date=as.POSIXlt(sorted$Date),Data=sorted$data)
          return(final)
        }
