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
            
  
########### 4 ########
#worst delays

flights %>% 
  arrange(desc(dep_delay+arr_delay)) %>% 
  select(carrier,dep_delay)


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
g=flights %>% 
  group_by(tailnum) %>% 
  summarise(n_flights=n_distinct(dep_delay>1))
#think

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
flights %>% 
  group_by(dest) %>%
  summarise(delays=sum(dep_delay,na.rm=T))

#For each flight, compute the proportion of the total delay for its destination.  
flights %>%
  group_by(dest) %>%
  summarise(delays=sum(dep_delay,na.rm=T)) %>% 
  group_by(carrier,flight) %>%
  summarise(propdel=delays/sum(dep_delay,na.rm=T))
  


###### 9 #########
#flights that represent a potential data entry error
flights %>% 
  mutate(air_delay=arr_delay-dep_delay) %>% 
  filter(air_timeair_delay)
  select(carrier,air_delay,origin,dest) %>% 
  arrange(air_delay)


  flights %>% 
    mutate(air_delay=dep_delay-arr_delay) %>% 
    filter(air_delay<0) %>% 
    select(carrier,air_delay,origin,dest) %>% 
    arrange(air_delay)
  



#air time of a flight relative to the shortest flight to that destination


#most delayed flight in air
flights %>% 
  mutate(air_delay=arr_delay-dep_delay) %>% 
  select(carrier,air_delay,origin,dest) %>% 
  arrange(desc(air_delay))




  
  





