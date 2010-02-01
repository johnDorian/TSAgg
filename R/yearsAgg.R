yearsAgg<-function(data,
		    process,
	    	    multiple=NULL,
		    na.rm=F,
		    from.first.obs=T){
	
	if(is.null(multiple)){
		multiple=1
	}
	if(from.first.obs==T){
	years<-(as.numeric(difftime(data$date,data$date[1],units=c("hours")))-as.numeric(difftime(data$date,data$date[1],units=c("hours")))%%8765.81277)/8765.81277#8765.81277 hours in each year.
	}else{
		years<-data$year-data$year[1]
		}
	years.again<-years-years%%multiple
	data$year<-data$year[1]+years.again
	date.1<-as.Date(strptime(paste(data$day[1],data$month[1],data$year),"%d %m %Y"))
	new.1<-data.frame(date=date.1,data=data$data)
	result<-aggregate(list(data=new.1$data),list(date=new.1$date),process,na.rm=na.rm)
#Test the last gap to make sure it's complete.
if(multiple>1){
	start<-min(data$year)
	end<-max(data$year)
	if((end-start)%%multiple!=0)
	warning("Last Gap does not contain ",multiple," years. There is only ",multiple-(end-start)%%multiple," year(s) in this gap.")
	}	
	
		#Everything that need to be done to calculate the annual agg.
	return(result)		
		}
