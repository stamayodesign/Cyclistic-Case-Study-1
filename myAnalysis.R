library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(tidyr)


getwd() #displays your working directory
setwd("/Users/soroc/Documents/_coursera/Google-Data-Analytics-Certificate/Capstone/Case Study 1/Prepared Data") 


## JANUARY
jan_2022_raw <- read_csv("202201-divvy-tripdata_Prepped.csv")


#Removing Outliers function
## Removes N/A, Trips with 0 length, and assigns the length of trip to a format accepted in R
remove_outliers = function(input_dataframe){
  tmp_df <- drop_na(input_dataframe,length_of_trip)
  tmp_df2 <- cbind(tmp_df,length_of_trip_r = as.POSIXct(tmp_df$length_of_trip,format="%H:%M:%S"))
  return <- subset(tmp_df2,length_of_trip_r > 0)
}
jan_2022_clean3imposter <- remove_outliers(jan_2022_clean)

#Removing Outliers
jan_2022_clean <- jan_2022_raw %>% drop_na(length_of_trip)

#Check Against Spreadsheet Findings and Environment
num_of_zero_01 <- sum(jan_2022_clean$length_of_trip == 0)
num_of_nonzero_01 <- sum(jan_2022_clean$length_of_trip > 0)

#Removing non 0 trips and creating time format for R
jan_2022_clean2 <- cbind(jan_2022_clean,length_of_trip_r = as.POSIXct(jan_2022_clean$length_of_trip,format="%H:%M:%S"))
jan_2022_clean3 <- subset(jan_2022_clean2,length_of_trip_r > 0)

#Creating function to filter member status and day of the week
filter_mc_dow = function(input_cleandata,input_mc,input_dow){
  return <- subset(select(input_cleandata, ride_id,rideable_type,length_of_trip,day_of_week,member_casual,length_of_trip_r),
                   member_casual==input_mc & day_of_week==input_dow)
}


jan_2022_casual1 <-filter_mc_dow(jan_2022_clean3imposter,"casual",1)
jan_2022_casual2 <-filter_mc_dow(jan_2022_clean3imposter,"casual",2)
jan_2022_casual3 <-filter_mc_dow(jan_2022_clean3imposter,"casual",3)
jan_2022_casual4 <-filter_mc_dow(jan_2022_clean3imposter,"casual",4)
jan_2022_casual5 <-filter_mc_dow(jan_2022_clean3imposter,"casual",5)
jan_2022_casual6 <-filter_mc_dow(jan_2022_clean3imposter,"casual",6)
jan_2022_casual7 <-filter_mc_dow(jan_2022_clean3imposter,"casual",7)

jan_2022_member1 <-filter_mc_dow(jan_2022_clean3imposter,"member",1)
jan_2022_member2 <-filter_mc_dow(jan_2022_clean3imposter,"member",2)
jan_2022_member3 <-filter_mc_dow(jan_2022_clean3imposter,"member",3)
jan_2022_member4 <-filter_mc_dow(jan_2022_clean3imposter,"member",4)
jan_2022_member5 <-filter_mc_dow(jan_2022_clean3imposter,"member",5)
jan_2022_member6 <-filter_mc_dow(jan_2022_clean3imposter,"member",6)
jan_2022_member7 <-filter_mc_dow(jan_2022_clean3imposter,"member",7)


#Assigning Quantile
## Doesn't need another function

quantile_01_casual1 <- quantile(jan_2022_casual1$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual2 <- quantile(jan_2022_casual2$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual3 <- quantile(jan_2022_casual3$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual4 <- quantile(jan_2022_casual4$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual5 <- quantile(jan_2022_clean3_casual5$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual6 <- quantile(jan_2022_clean3_casual6$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_casual7 <- quantile(jan_2022_clean3_casual7$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
quantile_01_member1 <- quantile(jan_2022_clean3_member1$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member2 <- quantile(jan_2022_clean3_member2$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member3 <- quantile(jan_2022_clean3_member3$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member4 <- quantile(jan_2022_clean3_member4$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member5 <- quantile(jan_2022_clean3_member5$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member6 <- quantile(jan_2022_clean3_member6$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))
quantile_01_member7 <- quantile(jan_2022_clean3_member7$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1))

# Get all Quantile data and put it into a dataframe
create_df_q_data <- function(input_quantile_data1,input_quantile_data2,input_quantile_data3,input_quantile_data4,input_quantile_data5,input_quantile_data6,input_quantile_data7){
  return <- data.frame(day_of_week = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"),
                       Q1 = c(
                         input_quantile_data1[2],
                         input_quantile_data2[2],
                         input_quantile_data3[2],
                         input_quantile_data4[2],
                         input_quantile_data5[2],
                         input_quantile_data6[2],
                         input_quantile_data7[2]
                       ),
                       Q2 = c(
                         input_quantile_data1[3],
                         input_quantile_data2[3],
                         input_quantile_data3[3],
                         input_quantile_data4[3],
                         input_quantile_data5[3],
                         input_quantile_data6[3],
                         input_quantile_data7[3]
                       ),
                       Q3 = c(
                         input_quantile_data1[4],
                         input_quantile_data2[4],
                         input_quantile_data3[4],
                         input_quantile_data4[4],
                         input_quantile_data5[4],
                         input_quantile_data6[4],
                         input_quantile_data7[4]
                       ),
                       Q4 = c(
                         input_quantile_data1[5],
                         input_quantile_data2[5],
                         input_quantile_data3[5],
                         input_quantile_data4[5],
                         input_quantile_data5[5],
                         input_quantile_data6[5],
                         input_quantile_data7[5]
                       )
                       )
}

qAll_01_casual_df <- create_df_q_data(quantile_01_casual1,quantile_01_casual2,quantile_01_casual3,quantile_01_casual4,quantile_01_casual5,quantile_01_casual6,quantile_01_casual7)
View(qAll_01_casual_df)

# Determining number of trips within 2nd quantile
num_trips_01_Q2orLower_causal_1 <- sum(jan_2022_clean3_casual1$length_of_trip_r <= quantile_01_casual1[3])
num_trips_01_Q2orLower_causal_2 <- sum(jan_2022_clean3_casual2$length_of_trip_r <= quantile_01_casual2[3])
num_trips_01_Q2orLower_causal_3 <- sum(jan_2022_clean3_casual3$length_of_trip_r <= quantile_01_casual3[3])
num_trips_01_Q2orLower_causal_4 <- sum(jan_2022_clean3_casual4$length_of_trip_r <= quantile_01_casual4[3])
num_trips_01_Q2orLower_causal_5 <- sum(jan_2022_clean3_casual5$length_of_trip_r <= quantile_01_casual5[3])
num_trips_01_Q2orLower_causal_6 <- sum(jan_2022_clean3_casual6$length_of_trip_r <= quantile_01_casual6[3])
num_trips_01_Q2orLower_causal_7 <- sum(jan_2022_clean3_casual7$length_of_trip_r <= quantile_01_casual7[3])

num_trips_01_Q2orLower_member_1 <- sum(jan_2022_clean3_member1$length_of_trip_r <= quantile_01_member1[3])
num_trips_01_Q2orLower_member_2 <- sum(jan_2022_clean3_member2$length_of_trip_r <= quantile_01_member2[3])
num_trips_01_Q2orLower_member_3 <- sum(jan_2022_clean3_member3$length_of_trip_r <= quantile_01_member3[3])
num_trips_01_Q2orLower_member_4 <- sum(jan_2022_clean3_member4$length_of_trip_r <= quantile_01_member4[3])
num_trips_01_Q2orLower_member_5 <- sum(jan_2022_clean3_member5$length_of_trip_r <= quantile_01_member5[3])
num_trips_01_Q2orLower_member_6 <- sum(jan_2022_clean3_member6$length_of_trip_r <= quantile_01_member6[3])
num_trips_01_Q2orLower_member_7 <- sum(jan_2022_clean3_member7$length_of_trip_r <= quantile_01_member7[3])

# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike

popular_bike_type_Jan_Casual <- data.frame(rideable_type = c("classic","docked","electric"), Sunday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual"& jan_2022_clean3$day_of_week == 1),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual"  & jan_2022_clean3$day_of_week == 1),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual"  & jan_2022_clean3$day_of_week == 1)),
                                           Monday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 2),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual"  & jan_2022_clean3$day_of_week == 2),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 2)),
                                           Tuesday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 3),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 3),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 3)),
                                           Wednesday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 4),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 4),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 4)),
                                           Thursday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 5),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 5),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 5)),
                                           Friday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 6),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 6),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 6)),
                                           Saturday =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 7),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 7),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual" & jan_2022_clean3$day_of_week == 7)),
                                           Total =
                                             c(sum(jan_2022_clean3$rideable_type == "classic_bike" & jan_2022_clean3$member_casual=="casual"),
                                               sum(jan_2022_clean3$rideable_type == "docked_bike" & jan_2022_clean3$member_casual=="casual"),
                                               sum(jan_2022_clean3$rideable_type == "electric_bike" & jan_2022_clean3$member_casual=="casual")
                                           ))

# Writing a function to make finding bike values faster
find_bike_data <- function(input_clean_data_rideable_type,input_clean_data_member_casual,memberOrCasual,input_clean_data_day_of_week){
  return <- data.frame(rideable_type = c("classic","docked","electric"), Sunday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 1),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual  & input_clean_data_day_of_week == 1),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual  & input_clean_data_day_of_week == 1)),
                       Monday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 2),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual  & input_clean_data_day_of_week == 2),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 2)),
                       Tuesday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 3),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 3),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 3)),
                       Wednesday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 4),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 4),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 4)),
                       Thursday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 5),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 5),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 5)),
                       Friday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 6),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 6),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 6)),
                       Saturday =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 7),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 7),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual & input_clean_data_day_of_week == 7)),
                       Total =
                         c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual),
                           sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual),
                           sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual)
                         ))
}
popular_bike_type_Jan_Casual <- find_bike_data(jan_2022_clean3$rideable_type,jan_2022_clean3$member_casual,"casual",jan_2022_clean3$day_of_week)
popular_bike_type_Jan_Member <- find_bike_data(jan_2022_clean3$rideable_type,jan_2022_clean3$member_casual,"member",jan_2022_clean3$day_of_week)

jun_2022 <- read_csv("202206-divvy-tripdata_Prepped.csv")
jun_2022_01 <- jun_2022 %>% drop_na(length_of_trip) #removed outliers from clean

