########################################
#####   CREATED BY JASON LESSELS   #####
########################################
##TODO: More testing....but for now it is working for all types of data (fingers crossed)
##DESIRED: ALOW THE USER TO SET THE NA.RM RATHER THAN THE CURRENT DEFAULT TRUE SETTING.

daysAgg<-function(data,process,multiple=NULL,na.rm=F){
		if(is.null(multiple)){
		warning("No multiple provided, aggregating to single days",call.=F)
		multiple=1
	}
		if(multiple==1){
		day<-aggregate(list(data=data$data),list(day=data$day,month=data$month,year=data$year),process,na.rm=na.rm)
	days<- as.Date(strptime(paste(day$day,day$month,day$year),"%d %m %Y"))
	data<-data.frame(date=days,data=day[,4:length(day)])
	return(data)
		}
	
	day<-aggregate(list(data=data$data,count=1),list(day=data$day,month=data$month,year=data$year),process,na.rm=na.rm)
	days<- as.Date(strptime(paste(day$day,day$month,day$year),"%d %m %Y"))
	data<-data.frame(date=days,data=day[,5:length(day)-1],count=day$count)
	days=paste(multiple,"days")
	all.dates<-seq.Date(as.Date(data$date[1]),as.Date(data$date[length(data[,1])]),by="day")
	dates<-data.frame(date=all.dates)
	test<-merge(dates,data,by="date",all.x=T)
	test$date<-rep(seq.Date(as.Date(data$date[1]),as.Date(data$date[length(data[,1])]),by=days),each=multiple,length=length(all.dates))
	data<-subset(test,!is.na(count))
	results<-aggregate(list(test[2:length(test)]),list(date=test$date),process,na.rm=T)
	results<-subset(results,count!=0)
	results<-results[,-length(results)]
	return(results)
}
