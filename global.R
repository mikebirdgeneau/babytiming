library(ggplot2)
library(data.table)
library(gridExtra)

babyTiming<-function(ExpectedDate="2016/02/29",DateofInterest="2016/02/2015",VacationDuration=14){
  DateofInterest<-as.Date(DateofInterest)
  ExpectedDate<-as.Date(ExpectedDate)
  currentDate<-Sys.Date()  
  
  #Load Probability Table
  data<-data.table(day=c("245", "246", "247", "248", "249", "250", "251", "252", "253", "254", "255", "256", "257", "258", "259", "260", "261", "262", "263", "264", "265", "266", "267", "268", "269", "270", "271", "272", "273", "274", "275", "276", "277", "278", "279", "280", "281", "282", "283", "284", "285", "286", "287", "288", "289", "290", "291", "292", "293", "294", "295", "296", "297", "298", "299", "300"),prob=c("0.0020236773954865", "0.0020236773954865", "0.00219015908250092", "0.0024047354790973", "0.00264890862005179", "0.00296707362190159", "0.00330743618201998", "0.00345541990381058", "0.00361820199778025", "0.00389937106918239", "0.00412874583795783", "0.00446910839807621", "0.00519422863485017", "0.00612652608213097", "0.006992230854606", "0.00805771365149834", "0.00928597854236034", "0.0102848686644469", "0.011165371809101", "0.0124010358860525", "0.0137846836847947", "0.0150943396226415", "0.0167665556788753", "0.0189789123196448", "0.0211098779134296", "0.0231224565297818", "0.0255345911949686", "0.0281021087680355", "0.0304698483166851", "0.0333037365889752", "0.0367295597484277", "0.0402441731409545", "0.043211246762856", "0.0458083610802812", "0.0473399926008139", "0.0475915649278579", "0.0466074731779504", "0.0455049944506105", "0.0437291897891232", "0.041509433962264", "0.03889752127266", "0.0359896411394747", "0.031934887162412", "0.0276507584165742", "0.023337032926378", "0.0189937106918238", "0.0147761746207918", "0.0113577506474287", "0.0083314835368109", "0.00574176840547536", "0.00389937106918226", "0.00266000739918615", "0.00181280059193512", "0.00141509433962406", "0.00141509433962406", "0.0020236773954865"))
  data[,day:=as.numeric(day)]
  data[,prob:=as.numeric(prob)]
  
  #Add expected date to the generic probability table. Assume expected date is 280 days    
  data[,date:=ExpectedDate-(280-day)]
  data<-data[date>=currentDate,]
  data[,scaled.prob:=prob*(1/sum(prob))]
  
  data <- merge(data.table(date=seq(from=Sys.Date(),to=max(c(ExpectedDate,DateofInterest,data$date)+VacationDuration),by=1)),data,by=c("date"),all=TRUE)
  data[is.na(prob),prob:=0,]
  data[is.na(scaled.prob),scaled.prob:=0,]
  
  
  #Report Probability On Date of Interest, Before Date and Within Vacation Period
  messages <- list(
    paste0("Based on today's date of ",currentDate,", and an expected due date of: ",ExpectedDate),
    paste0("There is a ", round(data[date==DateofInterest,scaled.prob]*100,1),"% chance the baby will be born on ",DateofInterest),
    paste0("There is a ", round(data[date<DateofInterest,sum(scaled.prob)]*100,1),"% chance the baby will be born before ",DateofInterest),
    paste0("There is a ", round(data[date<=DateofInterest& date>=(DateofInterest-VacationDuration),sum(scaled.prob)]*100,1),"% chance Mike will miss your meeting (due to baby related reasons) if it is on ",DateofInterest)
  )
  
  library(zoo)
  data[,com:=rollapply(scaled.prob,FUN = function(x){sum(x,na.rm=TRUE)}, align="right",width=VacationDuration+1,partial=TRUE,fill=NA),]
  
  data[,colour:=" "]
  data[date==DateofInterest,colour:="Date Of Interest"]
  data[date==ExpectedDate,colour:="Expected Date"]
  
  p1 <- ggplot(data,aes(x=date,y=scaled.prob))+geom_bar(stat="identity",aes(fill=colour))+ylab("Daily Probability")+theme_bw()+xlab("")+ggtitle("Baby Delivery Date Distribution")+scale_fill_manual(values=c(" "="grey30","Date Of Interest"="firebrick","Expected Date"="seagreen"),name="")    
  
  p2 <- ggplot(data,aes(x=date,y=com))+geom_bar(stat="identity",aes(fill=colour))+ylab("Daily Probability")+theme_bw()+xlab("")+ggtitle("Chance of Missing a Meeting")+scale_fill_manual(values=c(" "="grey30","Date Of Interest"="firebrick","Expected Date"="seagreen"),name="")
  
  return(list(data=data,p1=p1,p2=p2,messages=messages))
}