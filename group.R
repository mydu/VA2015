attraction_coordinate<-read.csv("Attraction-Coordinates.csv",head=T,sep=",")
people_movement<-read.csv("park-movement-Sun.csv",header=T,sep=",")
summary(attraction_coordinate)
summary(people_movement)
people_checkin<-people_movement[people_movement$type=="check-in",]

attraction_coordinate<-attraction_coordinate[,2:length(attraction_coordinate)]
names(attraction_coordinate)<-c("AttractionID", "Attraction.x", "X", "Y", "ParkArea", "CategoryNames")
people_check_in<-merge(people_checkin,attraction_coordinate,by=c("X","Y"))
#people_check_in<-as.POSIXlt(people_check_in$Timestamp)
people_check_in<-people_check_in[order(people_check_in$id,people_check_in$Timestamp,people_check_in$AttractionID),]
people_check_in<-data.frame(people_check_in[,c(1,2,4,6)],time=as.POSIXlt(people_check_in$Timestamp))
people_check_in<-people_check_in[3:5]

##the data is cleaned.

people_check_in<-read.csv("people_check_in.csv",header = T,sep=',')
people_check_in<-people_check_in[,2:4]




count<-rep(0,nrow(people_check_in))
index<-c(as.numeric(people_check_in$id)[1])
for (i in 2:nrow(people_check_in)){
  
  index<-append(index,people_check_in$id[i])
  if(index[i]!=index[i-1]){
    
    count[i]<-count[i]+1
  }  
}


set_id=c(1000279)
for( i in 2:length(index)){
  if(count[i]==1){
    set_id<-append(set_id,index[i])
  }
  
}
time_check_in<- substr(as.character(people_check_in$time),12,16)
transfert_time<-function(x){
  hours<-as.numeric(substr(x,1,2))
  minutes<-as.numeric(substr(x,4,5))
  t_time<-hours*60+minutes
  return(t_time)
}

#time_check_in<-apply(matrix(time_check_in,nrow = 1),2,transfert_time)
time_check_in<-transfert_time(time_check_in)
precision_minute<-5
people_check_in<-data.frame(people_check_in,t_time=floor(time_check_in/precision_minute))

move_str<-c()
str<-c()
count<-append(count,c(1))
for (i in 1:length(count)){
  
  if(count[i]==0){
    str<-paste(str,people_check_in$AttractionID[i],people_check_in$t_time[i],sep="-")
    
  }else{
    move_str<-append(move_str,str)
    str<-c()
    str<-paste(str,people_check_in$AttractionID[i],people_check_in$t_time[i],sep="-")
  }
  
}

people_group<-data.frame(id=set_id,move_str=move_str)

people_group<-people_group[order(people_group$move_str),]


write.csv(people_group,file="people_group.csv")

###################counting...

count_group<-rep(0,nrow(people_group))
index_group<-c(as.character(people_group$move_str)[1])
for (i in 2:nrow(people_group)){
  
  index_group<-append(index_group,as.character(people_group$move_str[i]))
  if(index_group[i]!=index_group[i-1]){
    
    count_group[i]<-count_group[i]+1
  }  
}


set_group_id=c(as.character(people_group$move_str[1]))
for( i in 2:length(index_group)){
  if(count_group[i]==1){
    set_group_id<-append(set_group_id,index_group[i])
  }
}

index_group<-cumsum(count_group)+1
count_group<-append(count_group,1)
group_volume<-which(count_group == 1)




people_group_finish<-data.frame(index=index_group,move_str=set_group_id)

################adjust constraint
people_group<-read.csv('people_group.csv',header = T,sep=',')
move_str<-as.character(people_group$move_str)
set_id<-as.character(people_group$id)
trace<-list()
for (i in 1:length(move_str)){
  trace[i]<-strsplit(move_str[i],'-')
  trace[[i]]<-trace[[i]][2:length(trace[[i]])]
}

group_adjusted<-rep(1,length(set_id))

test_diff<-function(x,y,number_diff){
  diff<-length(setdiff(x,y))+length(setdiff(y,x))
  if (diff>4*number_diff){
    return(1)
  }else{
    return(0)
  }
}


num_diff<-1
for (i in 2:length(set_id)){
  group_adjusted <-c(group_adjusted[1:i-1], group_adjusted[i:length(set_id)] +test_diff(trace[[i]],trace[[i-1]],num_diff) )
  
}

people_group_adjusted<-data.frame(id=set_id,move_str=move_str,group_number_adjusted=group_adjusted)

group_adjusted

write.csv(people_group_adjusted,file='people_group_adjusted_numdiff1.csv')




