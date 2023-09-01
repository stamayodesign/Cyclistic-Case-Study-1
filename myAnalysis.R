library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(tidyr)


getwd() #displays your working directory
setwd("/Users/soroc/Documents/_coursera/Google-Data-Analytics-Certificate/Capstone/Case Study 1/Prepared Data") 

# FUNCTIONS

# Function - Removes Outliers
## Removes N/A, Trips with 0 length, and assigns the length of trip to a format accepted in R
remove_outliers = function(input_dataframe){
  tmp_df <- drop_na(input_dataframe,length_of_trip)
  tmp_df2 <- cbind(tmp_df,length_of_trip_r = as.POSIXct(tmp_df$length_of_trip,format="%H:%M:%S"))
  return <- subset(tmp_df2,length_of_trip_r > 0)
}

# Function - Filters member status and day of the week
filter_mc_dow = function(input_cleandata,input_MC,input_dow){
  return <- subset(select(input_cleandata, ride_id,rideable_type,length_of_trip,day_of_week,member_casual,length_of_trip_r),
                   member_casual==input_MC & day_of_week==input_dow)
}

# Function - Handle Quantile data
## Get all Quantile data and put it into a dataframe
create_df_q_data <- function(input_cleandata,input_MC){
  
  # Get quantile for each day
  tmp_quantile_1 <- quantile(filter_mc_dow(input_cleandata,input_MC,1)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_2 <- quantile(filter_mc_dow(input_cleandata,input_MC,2)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_3 <- quantile(filter_mc_dow(input_cleandata,input_MC,3)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_4 <- quantile(filter_mc_dow(input_cleandata,input_MC,4)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_5 <- quantile(filter_mc_dow(input_cleandata,input_MC,5)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_6 <- quantile(filter_mc_dow(input_cleandata,input_MC,6)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_7 <- quantile(filter_mc_dow(input_cleandata,input_MC,7)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  
  return <- data.frame(day_of_week = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"),
                       Q1 = c(
                         tmp_quantile_1[2],
                         tmp_quantile_2[2],
                         tmp_quantile_3[2],
                         tmp_quantile_4[2],
                         tmp_quantile_5[2],
                         tmp_quantile_6[2],
                         tmp_quantile_7[2]
                       ),
                       Q2 = c(
                         tmp_quantile_1[3],
                         tmp_quantile_2[3],
                         tmp_quantile_3[3],
                         tmp_quantile_4[3],
                         tmp_quantile_5[3],
                         tmp_quantile_6[3],
                         tmp_quantile_7[3]
                       ),
                       Q3 = c(
                         tmp_quantile_1[4],
                         tmp_quantile_2[4],
                         tmp_quantile_3[4],
                         tmp_quantile_4[4],
                         tmp_quantile_5[4],
                         tmp_quantile_6[4],
                         tmp_quantile_7[4]
                       ),
                       Q4 = c(
                         tmp_quantile_1[5],
                         tmp_quantile_2[5],
                         tmp_quantile_3[5],
                         tmp_quantile_4[5],
                         tmp_quantile_5[5],
                         tmp_quantile_6[5],
                         tmp_quantile_7[5]
                       )
                       )
}

# Function - Create table of number of trips
create_df_num_of_trips <- function(input_cleandata,input_MorC){
  return <- data.frame(day_of_week = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"),
                       num_of_trips = c(nrow(filter_mc_dow(input_cleandata,input_MorC,1)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,2)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,3)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,4)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,5)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,6)),
                                        nrow(filter_mc_dow(input_cleandata,input_MorC,7))
                       )
  )
}
# Function - Create a table of number of trips that less than Q2
create_df_num_of_trips_q <- function(input_cleandata,input_MorC,input_q){
  
  
  tmp_filter_data_MorC1 <-filter_mc_dow(input_cleandata,input_MorC,1)
  tmp_filter_data_MorC2 <-filter_mc_dow(input_cleandata,input_MorC,2)
  tmp_filter_data_MorC3 <-filter_mc_dow(input_cleandata,input_MorC,3)
  tmp_filter_data_MorC4 <-filter_mc_dow(input_cleandata,input_MorC,4)
  tmp_filter_data_MorC5 <-filter_mc_dow(input_cleandata,input_MorC,5)
  tmp_filter_data_MorC6 <-filter_mc_dow(input_cleandata,input_MorC,6)
  tmp_filter_data_MorC7 <-filter_mc_dow(input_cleandata,input_MorC,7)
  
  # Get quantile for each day
  tmp_quantile_1 <- quantile(filter_mc_dow(input_cleandata,input_MorC,1)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_2 <- quantile(filter_mc_dow(input_cleandata,input_MorC,2)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_3 <- quantile(filter_mc_dow(input_cleandata,input_MorC,3)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_4 <- quantile(filter_mc_dow(input_cleandata,input_MorC,4)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_5 <- quantile(filter_mc_dow(input_cleandata,input_MorC,5)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_6 <- quantile(filter_mc_dow(input_cleandata,input_MorC,6)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  tmp_quantile_7 <- quantile(filter_mc_dow(input_cleandata,input_MorC,7)$length_of_trip_r,probs = c(0,0.25,0.5,0.75,1)) 
  
  
  return <- data.frame(day_of_week = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"),
                       num_of_trips = c(sum(tmp_filter_data_MorC1$length_of_trip_r <= tmp_quantile_1[input_q]),
                                        sum(tmp_filter_data_MorC2$length_of_trip_r <= tmp_quantile_2[input_q]),
                                        sum(tmp_filter_data_MorC3$length_of_trip_r <= tmp_quantile_3[input_q]),
                                        sum(tmp_filter_data_MorC4$length_of_trip_r <= tmp_quantile_4[input_q]),
                                        sum(tmp_filter_data_MorC5$length_of_trip_r <= tmp_quantile_5[input_q]),
                                        sum(tmp_filter_data_MorC6$length_of_trip_r <= tmp_quantile_6[input_q]),
                                        sum(tmp_filter_data_MorC7$length_of_trip_r <= tmp_quantile_7[input_q])
                                        )
                      )
}

# Function - Find Num of Bikes Used per day per type of user
## Writing a function to make finding bike values cleaner
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


# JANUARY
jan_2022_raw <- read_csv("202201-divvy-tripdata_Prepped.csv")
jan_2022_clean <- remove_outliers(jan_2022_raw)
jan_2022_casual_df <- create_df_num_of_trips(jan_2022_clean,"casual")
jan_2022_member_df <- create_df_num_of_trips(jan_2022_clean,"member")
jan_2022_qAll_casual_df <- create_df_q_data(jan_2022_clean,"casual")
jan_2022_qAll_member_df <- create_df_q_data(jan_2022_clean,"member")
jan_2022_q2_casual_df <- create_df_num_of_trips_q(jan_2022_clean,"casual",3)
jan_2022_q2_member_df <- create_df_num_of_trips_q(jan_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
jan_2022_bike_type_Casual_df <- find_bike_data(jan_2022_clean$rideable_type,jan_2022_clean$member_casual,"casual",jan_2022_clean$day_of_week)
jan_2022_bike_type_Member_df <- find_bike_data(jan_2022_clean$rideable_type,jan_2022_clean$member_casual,"member",jan_2022_clean$day_of_week)

# FEBRUARY
feb_2022_raw <- read_csv("202202-divvy-tripdata_Prepped.csv")
feb_2022_clean <- remove_outliers(feb_2022_raw)
feb_2022_casual_df <- create_df_num_of_trips(feb_2022_clean,"casual")
feb_2022_member_df <- create_df_num_of_trips(feb_2022_clean,"member")
feb_2022_qAll_casual_df <- create_df_q_data(feb_2022_clean,"casual")
feb_2022_qAll_member_df <- create_df_q_data(feb_2022_clean,"member")
feb_2022_q2_casual_df <- create_df_num_of_trips_q(feb_2022_clean,"casual",3)
feb_2022_q2_member_df <- create_df_num_of_trips_q(feb_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
feb_2022_bike_type_Casual_df <- find_bike_data(feb_2022_clean$rideable_type,feb_2022_clean$member_casual,"casual",feb_2022_clean$day_of_week)
feb_2022_bike_type_Member_df <- find_bike_data(feb_2022_clean$rideable_type,feb_2022_clean$member_casual,"member",feb_2022_clean$day_of_week)

# JUNE
jun_2022_raw <- read_csv("202206-divvy-tripdata_Prepped.csv")
jun_2022_clean <- remove_outliers(jun_2022_raw)
jun_2022_casual_df <- create_df_num_of_trips(jun_2022_clean,"casual")
jun_2022_member_df <- create_df_num_of_trips(jun_2022_clean,"member")
jun_2022_qAll_casual_df <- create_df_q_data(jun_2022_clean,"casual")
jun_2022_qAll_member_df <- create_df_q_data(jun_2022_clean,"member")
jun_2022_q2_casual_df <- create_df_num_of_trips_q(jun_2022_clean,"casual",3)
jun_2022_q2_member_df <- create_df_num_of_trips_q(jun_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
jun_2022_bike_type_Casual_df <- find_bike_data(jun_2022_clean$rideable_type,jun_2022_clean$member_casual,"casual",jun_2022_clean$day_of_week)
jun_2022_bike_type_Member_df <- find_bike_data(jun_2022_clean$rideable_type,jun_2022_clean$member_casual,"member",jun_2022_clean$day_of_week)


