attraction_coordinate<-read.csv("Attraction-Coordinates.csv",head=T,sep=",")
people_movement<-read.csv("park-movement-Sun.csv",header=T,sep=",")
summary(attraction_coordinate)
summary(people_movement)
##filter the data
entry_exit<-attraction_coordinate[attraction_coordinate$CategoryNames=="Entry-Exit",]
summary(entry_exit)

people_inout<-subset(people_movement, (people_movement$X==99 & people_movement$Y==77) |(people_movement$X==63 & people_movement$Y==99) |(people_movement$X==0 & people_movement$Y==67))

people_inout<-people_inout[order(people_inout$id,people_inout$Timestamp),]

##filter out the outliers: who have just one check-out, no movement.(or just one movement)

people_inout_saved<-people_inout
people_inout_saved1<-people_inout

for (i in seq(1,nrow(people_inout),2)) if (people_inout[i,]$type =="movement") {print(i);break()}
for (i in seq(2,nrow(people_inout),2)) if (people_inout[i,]$type =="check-in") {print(i);break()}


people_inout<-rbind(people_inout[1:717-1,],people_inout[717+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:2767-1,],people_inout[2767+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:3127-1,],people_inout[3127+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:3997-1,],people_inout[3997+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:5439-1,],people_inout[5439+1:nrow(people_inout),])#!#outlier witn just one movement
people_inout<-rbind(people_inout[1:6093-1,],people_inout[6093+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:9007-1,],people_inout[9007+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:9421-1,],people_inout[9421+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:10819-1,],people_inout[10819+1:nrow(people_inout),])
people_inout<-rbind(people_inout[1:15031-1,],people_inout[15031+1:nrow(people_inout),])


plot(as.POSIXlt(people_inout[people_inout$type=="check-in",]$Timestamp),as.POSIXlt(people_inout[people_inout$type=="movement",]$Timestamp)
     ,main="Enter-Exit Clusters",xlab="Leave Time",ylab="Enter Time")












