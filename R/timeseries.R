timeseries<-
	function(dates,
			 dateformat,
			 data=NULL){
		if(class(data)=="numeric")data.length=length(data)else data.length=length(data[,1])
		if(!is.null(data)&&length(dates)!=data.length)
			stop("Lengths differ between dates (",length(dates),") and data (",data.length, ").")
		dates<-(strptime(dates,dateformat))
		minute<- as.numeric(format(dates,"%M"))
		hour<- as.numeric(format(dates,"%H"))
		day<-as.numeric(format(dates,"%d"))
		week<-as.numeric(format(dates,"%W"))
		month<-as.numeric(format(dates,"%m"))
		year<-as.numeric(format(dates,"%Y"))
		if(is.na(sum(year))){
			stop("Dates must contain atleast three fields")}
		if(is.null(data)){
			warning("NO DATA PROVIDED")
			results<-data.frame(dates,minute,hour,day,week,month,year)
			return(results)
			}
		else{
			results<-data.frame(dates,minute,hour,day,week,month,year,data)
			return(results)
		}
	}




