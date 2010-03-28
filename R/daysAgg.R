daysAgg<-function(data,process,multiple=NULL,na.rm=F){
		if(is.null(multiple)){
		multiple=1
	}
		if(multiple==1){
		day<-aggregate(data[,8:length(data)],list(day=data$day,month=data$month,year=data$year),process,na.rm=na.rm)
	days<- as.Date(strptime(paste(day$day,day$month,day$year),"%d %m %Y"))
	data2<-data.frame(date=days,data=day[,4:length(day)])
	names(data2)<-c("Date",names(data[8:length(data)]))
	return(data2)
		}
	temp<-data
	day<-aggregate(list(data[,8:length(data)],count=1),list(day=data$day,month=data$month,year=data$year),process,na.rm=na.rm)
	days<- as.Date(strptime(paste(day$day,day$month,day$year),"%d %m %Y"))
	data<-data.frame(date=days,day[,5:length(day)-1],count=day[length(day)])
	days=paste(multiple,"days")
	all.dates<-seq.Date(as.Date(data$date[1]),as.Date(data$date[length(data[,1])]),by="day")
	dates<-data.frame(date=all.dates)
	test<-merge(dates,data,by="date",all.x=T)
	test$date<-rep(seq.Date(as.Date(data$date[1]),as.Date(data$date[length(data[,1])]),by=days),each=multiple,length=length(all.dates))
#	data<-subset(test,!is.na(count)) 
	results<-aggregate(list(test[2:length(test)]),list(date=test$date),process,na.rm=T)
	results<-subset(results,count!=0)
	results<-results[,-length(results)]
	names(results)<-c("Date",names(temp[8:length(temp)]))
	return(results)
}
