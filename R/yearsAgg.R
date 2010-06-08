yearsAgg<-function(data,
		    process,
	    	    multiple=NULL,
		    na.rm=FALSE,
		    from.first.obs=TRUE){
	
	if(is.null(multiple)){
		multiple=1
	}
	#Test the last gap to make sure it's complete.
	start<-min(data$year)
	end<-max(data$year)
	if((end-start)%%multiple!=0)warning("Last Gap does not contain ",multiple," years. There is only ",((end-start)%%multiple)," year(s) in this gap.",call.=FALSE)
	
	if(from.first.obs==TRUE){
	years<-(as.numeric(difftime(data$date,data$date[1],units=c("hours")))-as.numeric(difftime(data$date,data$date[1],units=c("hours")))%%8765.81277)/8765.81277
	#8765.81277 hours in each year.
	years.again<-years-years%%multiple
	data$year<-data$year[1]+years.again
	date.1<-as.Date(strptime(paste(data$day[1],data$month[1],data$year),"%d %m %Y"))
	new.1<-data.frame(date=date.1,data[,8:length(data)])
	result<-aggregate(new.1[,2:length(new.1)],list(date=new.1$date),process,na.rm=na.rm)
	names(result)<-c("Date",names(data[8:length(data)]))
	}else{
		years<-data$year-data$year[1]
		years.again<-years-years%%multiple
	data$year<-data$year[1]+years.again
	date.1<-as.Date(strptime(paste(1,1,data$year),"%d %m %Y"))
	new.1<-data.frame(date=date.1,data[,8:length(data)])
	result<-aggregate(new.1[,2:length(new.1)],list(date=new.1$date),process,na.rm=na.rm)
	names(result)<-c("Date",names(data[8:length(data)]))
		}
	

	
		#Everything that need to be done to calculate the annual agg.
	return(result)		
		}
