monthsAgg<-function(data,
		    process,
	    	    multiple=NULL,
		    na.rm=FALSE){
	
	if(is.null(multiple)){
		multiple=1
	}	
	
	d.cols<-length(data)
	month<-aggregate(data[,8:d.cols],list(month=data$month,year=data$year),process,na.rm=na.rm)
	data.cols<-length(month)

	
	if(multiple>1){

month.gap<-month[,1]
for(i in 1:length(month[,1])){
	month.gap[i]=(month[i,2]%%month[1,2])*12+month[i,1]}
month.gap<-month.gap-month.gap%%multiple
month.gap<-month.gap%%12+1

year<-month[,2]
if(sum(month.gap)==length(month.gap)){
year<-year[1]+(year-year[1])-(year-year[1])%%(multiple/12)
}else{

for(i in 2:length(month.gap)){
	if(month.gap[i]==month.gap[i-1])
		year[i]=year[i-1]
	else
		year[i]=month[i,2]
		}
	}
		date=strptime(paste(01,month.gap,year),"%d %m %Y")
		results<-data.frame(date,data=month[,3:data.cols])
		final<-aggregate(results[,2:length(results)],list(date=results$date),process,na.rm=na.rm)
		names(final)<-c("Date",names(data[8:length(data)]))
		return(final)
}
	else{
		date=strptime(paste(01,month$month,month$year),"%d %m %Y")
		results<-data.frame(date,data=month[,3:data.cols])
		names(results)<-c("Date",names(data[8:length(data)]))
		return(results)
	}
}
