##install packages
#Miles is a better programmer
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)


## dataset for assignment
f=flights
#a

delayed_flight_2hours=flights %>% 
  filter(arr_delay>=120)

#b

flewt0_Houston=flights %>% 
  filter(dest==c("IAH","HOU"))

#c

operated_U_A_D=flights %>% 
  filter(carrier==c("UA","AA","DL"))

#d

departed_summer=flights %>% 
  filter(month==1|month==8|month==9)

#e

arrdelay_depexact=flights %>% 
  filter(arr_delay>120 & dep_delay<=0)

#f
arrdelay_madeupflight30=flights %>% 
  filter(dep_delay>=60 & (dep_delay-arr_delay)>30)

#g
#ask for the variable
dep_midnight=flights %>% 
  filter(dep_time<=600)

#h
mostdelayed=flights %>% 
  arrange(desc(dep_delay)) %>% 

leftearly= flights %>% 
  arrange(air_delay)

#i

fastest_flights=flights %>% 
  arrange(desc(distance/time))

#j
#shortest distance flight
flights %>% 
  arrange(desc(distance))

#longest distance flight
flights %>% 
  arrange(distance)











