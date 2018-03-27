##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
f=flights 

## checking variable ##
glimpse(flights)


############ 3 ###########
#cancelled flights
cancelled=flights %>% 
  group_by(month,day) %>% 
  summarise(count_cancelled=sum(is.na(air_time)))

#pattern
#looking for the pattern
ggplot(cancelled,aes(x=day,y=count_cancelled,color=month))+
  geom_point()

#proportion of cancellation relation to average delay
prop=flights %>% 
  group_by(month,day) %>% 
  summarise(prop_cancel=sum(is.na(air_time))/(sum(is.na(air_time))+sum(is.na(air_time)==F)))
delay=flights%>% 
  group_by(month,day) %>% 
  summarise(del=mean(dep_delay,na.rm=T))

# determine if related or not
anova(lm(prop$prop_cancel~delay$del))
#conclusion it is related.            
  
########### 4 ########
#worst delays


flights %>% 
  group_by(carrier) %>% 
  summarise(delays=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(delays))


### challenge ##
flights %>% 
  group_by(carrier,dest) %>% 
  summarise(delays=mean(dep_delay,na.rm=T)) 
  
  

######## 5 ##########
#no of flights before first delay greater than 1
flights %>% 
  mutate(idnum=c(1:length(dep_delay))) %>% 
  group_by(month,day) %>% 
  filter(first(dep_delay>60))%>% 
  select(month,day,tailnum,dep_delay,idnum)

  
  


###### 6 #######
#worst on record
flights %>% 
  group_by(tailnum) %>% 
  summarise(delays=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(delays))

########## 7 ########
#time of day should you fly if you want to avoid delays
flights %>% 
  group_by(hour) %>% 
  summarise(delays=mean(dep_delay,na.rm=T)) %>% 
  arrange(delays)

##### 8 #########
#For each destination, compute the total minutes of delay
m=flights %>% 
  group_by(dest) %>%
  summarise(m.elays=sum(dep_delay,na.rm=T))

#For each flight, compute the proportion of the total delay for its destination.  
flights %>%
  group_by(dest) %>% 
  mutate(t.delay=sum(dep_delay,na.rm=T)) %>% 
  mutate(propdelay=dep_delay/t.delay) %>% 
  select(carrier,origin,dest,propdelay,flight) %>% 
  arrange(dest)
  


###### 9 #########
#flights that represent a potential data entry error
flights %>% 
  group_by(origin,dest) %>% 
  mutate(avg.airtime=mean(air_time,na.rm = T),fast=air_time/avg.airtime) %>% 
  select(carrier,origin,dest,flight,fast,air_time) %>% 
  arrange(fast)
  


#most delayed flight in air
flights %>% 
  mutate(air_delay=arr_delay-dep_delay, air.dist=air_delay/distance) %>% 
  select(carrier,air_delay,origin,dest,air.dist) %>% 
  arrange(desc(air.dist))




  
  





