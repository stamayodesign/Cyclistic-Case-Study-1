library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(scales) #helps manage scale of data
library(tidyr)
library(hms)

#getwd() #displays your working directory
#setwd("/Users/soroc/Documents/_coursera/Google-Data-Analytics-Certificate/Capstone/Case Study 1/Prepared Data") #changes to directory with CSVs

# FUNCTIONS #

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
                       day_of_week_num = c(1:7),
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
  return <- data.frame(day_of_week = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"),
                       classic_bike = c(sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 1),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 2),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 3),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 4),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 5),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 6),
                                        sum(input_clean_data_rideable_type == "classic_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 7)
                                        ),
                       docked_bike = c(sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 1),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 2),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 3),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 4),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 5),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 6),
                                       sum(input_clean_data_rideable_type == "docked_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 7)
                                       ),
                       electric_bike = c(sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 1),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 2),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 3),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 4),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 5),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 6),
                                         sum(input_clean_data_rideable_type == "electric_bike" & input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 7)
                                       ),
                       total = c(sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 1),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 2),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 3),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 4),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 5),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 6),
                                      sum(input_clean_data_member_casual==memberOrCasual& input_clean_data_day_of_week == 7)
                                      )
  )
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


# MARCH
mar_2022_raw <- read_csv("202203-divvy-tripdata_Prepped.csv")
mar_2022_clean <- remove_outliers(mar_2022_raw)
mar_2022_casual_df <- create_df_num_of_trips(mar_2022_clean,"casual")
mar_2022_member_df <- create_df_num_of_trips(mar_2022_clean,"member")
mar_2022_qAll_casual_df <- create_df_q_data(mar_2022_clean,"casual")
mar_2022_qAll_member_df <- create_df_q_data(mar_2022_clean,"member")
mar_2022_q2_casual_df <- create_df_num_of_trips_q(mar_2022_clean,"casual",3)
mar_2022_q2_member_df <- create_df_num_of_trips_q(mar_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
mar_2022_bike_type_Casual_df <- find_bike_data(mar_2022_clean$rideable_type,mar_2022_clean$member_casual,"casual",mar_2022_clean$day_of_week)
mar_2022_bike_type_Member_df <- find_bike_data(mar_2022_clean$rideable_type,mar_2022_clean$member_casual,"member",mar_2022_clean$day_of_week)

# APRIL
apr_2022_raw <- read_csv("202204-divvy-tripdata_Prepped.csv")
apr_2022_clean <- remove_outliers(apr_2022_raw)
apr_2022_casual_df <- create_df_num_of_trips(apr_2022_clean,"casual")
apr_2022_member_df <- create_df_num_of_trips(apr_2022_clean,"member")
apr_2022_qAll_casual_df <- create_df_q_data(apr_2022_clean,"casual")
apr_2022_qAll_member_df <- create_df_q_data(apr_2022_clean,"member")
apr_2022_q2_casual_df <- create_df_num_of_trips_q(apr_2022_clean,"casual",3)
apr_2022_q2_member_df <- create_df_num_of_trips_q(apr_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
apr_2022_bike_type_Casual_df <- find_bike_data(apr_2022_clean$rideable_type,apr_2022_clean$member_casual,"casual",apr_2022_clean$day_of_week)
apr_2022_bike_type_Member_df <- find_bike_data(apr_2022_clean$rideable_type,apr_2022_clean$member_casual,"member",apr_2022_clean$day_of_week)


# MAY
may_2022_raw <- read_csv("202205-divvy-tripdata_Prepped.csv")
may_2022_clean <- remove_outliers(may_2022_raw)
may_2022_casual_df <- create_df_num_of_trips(may_2022_clean,"casual")
may_2022_member_df <- create_df_num_of_trips(may_2022_clean,"member")
may_2022_qAll_casual_df <- create_df_q_data(may_2022_clean,"casual")
may_2022_qAll_member_df <- create_df_q_data(may_2022_clean,"member")
may_2022_q2_casual_df <- create_df_num_of_trips_q(may_2022_clean,"casual",3)
may_2022_q2_member_df <- create_df_num_of_trips_q(may_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
may_2022_bike_type_Casual_df <- find_bike_data(may_2022_clean$rideable_type,may_2022_clean$member_casual,"casual",may_2022_clean$day_of_week)
may_2022_bike_type_Member_df <- find_bike_data(may_2022_clean$rideable_type,may_2022_clean$member_casual,"member",may_2022_clean$day_of_week)


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


# JULY
jul_2022_raw <- read_csv("202207-divvy-tripdata_Prepped.csv")
jul_2022_clean <- remove_outliers(jul_2022_raw)
jul_2022_casual_df <- create_df_num_of_trips(jul_2022_clean,"casual")
jul_2022_member_df <- create_df_num_of_trips(jul_2022_clean,"member")
jul_2022_qAll_casual_df <- create_df_q_data(jul_2022_clean,"casual")
jul_2022_qAll_member_df <- create_df_q_data(jul_2022_clean,"member")
jul_2022_q2_casual_df <- create_df_num_of_trips_q(jul_2022_clean,"casual",3)
jul_2022_q2_member_df <- create_df_num_of_trips_q(jul_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
jul_2022_bike_type_Casual_df <- find_bike_data(jul_2022_clean$rideable_type,jul_2022_clean$member_casual,"casual",jul_2022_clean$day_of_week)
jul_2022_bike_type_Member_df <- find_bike_data(jul_2022_clean$rideable_type,jul_2022_clean$member_casual,"member",jul_2022_clean$day_of_week)


# AUGUST
aug_2022_raw <- read_csv("202208-divvy-tripdata_Prepped.csv")
aug_2022_clean <- remove_outliers(aug_2022_raw)
aug_2022_casual_df <- create_df_num_of_trips(aug_2022_clean,"casual")
aug_2022_member_df <- create_df_num_of_trips(aug_2022_clean,"member")
aug_2022_qAll_casual_df <- create_df_q_data(aug_2022_clean,"casual")
aug_2022_qAll_member_df <- create_df_q_data(aug_2022_clean,"member")
aug_2022_q2_casual_df <- create_df_num_of_trips_q(aug_2022_clean,"casual",3)
aug_2022_q2_member_df <- create_df_num_of_trips_q(aug_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
aug_2022_bike_type_Casual_df <- find_bike_data(aug_2022_clean$rideable_type,aug_2022_clean$member_casual,"casual",aug_2022_clean$day_of_week)
aug_2022_bike_type_Member_df <- find_bike_data(aug_2022_clean$rideable_type,aug_2022_clean$member_casual,"member",aug_2022_clean$day_of_week)


# SEPTEMBER
sep_2022_raw <- read_csv("202209-divvy-tripdata_Prepped.csv")
sep_2022_clean <- remove_outliers(sep_2022_raw)
sep_2022_casual_df <- create_df_num_of_trips(sep_2022_clean,"casual")
sep_2022_member_df <- create_df_num_of_trips(sep_2022_clean,"member")
sep_2022_qAll_casual_df <- create_df_q_data(sep_2022_clean,"casual")
sep_2022_qAll_member_df <- create_df_q_data(sep_2022_clean,"member")
sep_2022_q2_casual_df <- create_df_num_of_trips_q(sep_2022_clean,"casual",3)
sep_2022_q2_member_df <- create_df_num_of_trips_q(sep_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
sep_2022_bike_type_Casual_df <- find_bike_data(sep_2022_clean$rideable_type,sep_2022_clean$member_casual,"casual",sep_2022_clean$day_of_week)
sep_2022_bike_type_Member_df <- find_bike_data(sep_2022_clean$rideable_type,sep_2022_clean$member_casual,"member",sep_2022_clean$day_of_week)


# OCTOBER
oct_2022_raw <- read_csv("202210-divvy-tripdata_Prepped.csv")
oct_2022_clean <- remove_outliers(oct_2022_raw)
oct_2022_casual_df <- create_df_num_of_trips(oct_2022_clean,"casual")
oct_2022_member_df <- create_df_num_of_trips(oct_2022_clean,"member")
oct_2022_qAll_casual_df <- create_df_q_data(oct_2022_clean,"casual")
oct_2022_qAll_member_df <- create_df_q_data(oct_2022_clean,"member")
oct_2022_q2_casual_df <- create_df_num_of_trips_q(oct_2022_clean,"casual",3)
oct_2022_q2_member_df <- create_df_num_of_trips_q(oct_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
oct_2022_bike_type_Casual_df <- find_bike_data(oct_2022_clean$rideable_type,oct_2022_clean$member_casual,"casual",oct_2022_clean$day_of_week)
oct_2022_bike_type_Member_df <- find_bike_data(oct_2022_clean$rideable_type,oct_2022_clean$member_casual,"member",oct_2022_clean$day_of_week)

# NOVEMBER
nov_2022_raw <- read_csv("202211-divvy-tripdata_Prepped.csv")
nov_2022_clean <- remove_outliers(nov_2022_raw)
nov_2022_casual_df <- create_df_num_of_trips(nov_2022_clean,"casual")
nov_2022_member_df <- create_df_num_of_trips(nov_2022_clean,"member")
nov_2022_qAll_casual_df <- create_df_q_data(nov_2022_clean,"casual")
nov_2022_qAll_member_df <- create_df_q_data(nov_2022_clean,"member")
nov_2022_q2_casual_df <- create_df_num_of_trips_q(nov_2022_clean,"casual",3)
nov_2022_q2_member_df <- create_df_num_of_trips_q(nov_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
nov_2022_bike_type_Casual_df <- find_bike_data(nov_2022_clean$rideable_type,nov_2022_clean$member_casual,"casual",nov_2022_clean$day_of_week)
nov_2022_bike_type_Member_df <- find_bike_data(nov_2022_clean$rideable_type,nov_2022_clean$member_casual,"member",nov_2022_clean$day_of_week)

# DECEMBER
dec_2022_raw <- read_csv("202212-divvy-tripdata_Prepped.csv")
dec_2022_clean <- remove_outliers(dec_2022_raw)
dec_2022_casual_df <- create_df_num_of_trips(dec_2022_clean,"casual")
dec_2022_member_df <- create_df_num_of_trips(dec_2022_clean,"member")
dec_2022_qAll_casual_df <- create_df_q_data(dec_2022_clean,"casual")
dec_2022_qAll_member_df <- create_df_q_data(dec_2022_clean,"member")
dec_2022_q2_casual_df <- create_df_num_of_trips_q(dec_2022_clean,"casual",3)
dec_2022_q2_member_df <- create_df_num_of_trips_q(dec_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
dec_2022_bike_type_Casual_df <- find_bike_data(dec_2022_clean$rideable_type,dec_2022_clean$member_casual,"casual",dec_2022_clean$day_of_week)
dec_2022_bike_type_Member_df <- find_bike_data(dec_2022_clean$rideable_type,dec_2022_clean$member_casual,"member",dec_2022_clean$day_of_week)

# 2022
year_2022_clean <- rbind(jan_2022_clean, feb_2022_clean, mar_2022_clean, apr_2022_clean, 
                         may_2022_clean, jun_2022_clean, jul_2022_clean, aug_2022_clean, 
                         sep_2022_clean, oct_2022_clean, nov_2022_clean, dec_2022_clean)
year_2022_casual_df <- create_df_num_of_trips(year_2022_clean,"casual")
year_2022_member_df <- create_df_num_of_trips(year_2022_clean,"member")
year_2022_qAll_casual_df <- create_df_q_data(year_2022_clean,"casual")
year_2022_qAll_member_df <- create_df_q_data(year_2022_clean,"member")
year_2022_q2_casual_df <- create_df_num_of_trips_q(year_2022_clean,"casual",3)
year_2022_q2_member_df <- create_df_num_of_trips_q(year_2022_clean,"member",3)
# Determining Most Popular Type of Bike
# classic_bike, docked_bike, electric_bike
year_2022_bike_type_Casual_df <- find_bike_data(year_2022_clean$rideable_type,year_2022_clean$member_casual,"casual",year_2022_clean$day_of_week)
year_2022_bike_type_Member_df <- find_bike_data(year_2022_clean$rideable_type,year_2022_clean$member_casual,"member",year_2022_clean$day_of_week)

#------
yearByMonth_2022_bike_type_Casual_df <- data.frame(
  month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  monthNum = c(1:12),
  classic_bike = c(
    sum(jan_2022_bike_type_Casual_df$classic_bike),
    sum(feb_2022_bike_type_Casual_df$classic_bike),
    sum(mar_2022_bike_type_Casual_df$classic_bike),
    sum(apr_2022_bike_type_Casual_df$classic_bike),
    sum(may_2022_bike_type_Casual_df$classic_bike),
    sum(jun_2022_bike_type_Casual_df$classic_bike),
    sum(jul_2022_bike_type_Casual_df$classic_bike),
    sum(aug_2022_bike_type_Casual_df$classic_bike),
    sum(sep_2022_bike_type_Casual_df$classic_bike),
    sum(oct_2022_bike_type_Casual_df$classic_bike),
    sum(nov_2022_bike_type_Casual_df$classic_bike),
    sum(dec_2022_bike_type_Casual_df$classic_bike)
  ) ,
  docked_bike = c(
    sum(jan_2022_bike_type_Casual_df$docked_bike),
    sum(feb_2022_bike_type_Casual_df$docked_bike),
    sum(mar_2022_bike_type_Casual_df$docked_bike),
    sum(apr_2022_bike_type_Casual_df$docked_bike),
    sum(may_2022_bike_type_Casual_df$docked_bike),
    sum(jun_2022_bike_type_Casual_df$docked_bike),
    sum(jul_2022_bike_type_Casual_df$docked_bike),
    sum(aug_2022_bike_type_Casual_df$docked_bike),
    sum(sep_2022_bike_type_Casual_df$docked_bike),
    sum(oct_2022_bike_type_Casual_df$docked_bike),
    sum(nov_2022_bike_type_Casual_df$docked_bike),
    sum(dec_2022_bike_type_Casual_df$docked_bike)
  ) ,
  electric_bike = c(
    sum(jan_2022_bike_type_Casual_df$electric_bike),
    sum(feb_2022_bike_type_Casual_df$electric_bike),
    sum(mar_2022_bike_type_Casual_df$electric_bike),
    sum(apr_2022_bike_type_Casual_df$electric_bike),
    sum(may_2022_bike_type_Casual_df$electric_bike),
    sum(jun_2022_bike_type_Casual_df$electric_bike),
    sum(jul_2022_bike_type_Casual_df$electric_bike),
    sum(aug_2022_bike_type_Casual_df$electric_bike),
    sum(sep_2022_bike_type_Casual_df$electric_bike),
    sum(oct_2022_bike_type_Casual_df$electric_bike),
    sum(nov_2022_bike_type_Casual_df$electric_bike),
    sum(dec_2022_bike_type_Casual_df$electric_bike)
  ),
  total = c( 
    sum(jan_2022_bike_type_Casual_df$classic_bike) + sum(jan_2022_bike_type_Casual_df$docked_bike) + sum(jan_2022_bike_type_Casual_df$electric_bike),
    sum(feb_2022_bike_type_Casual_df$classic_bike) + sum(feb_2022_bike_type_Casual_df$docked_bike) + sum(feb_2022_bike_type_Casual_df$electric_bike),
    sum(mar_2022_bike_type_Casual_df$classic_bike) + sum(mar_2022_bike_type_Casual_df$docked_bike) + sum(mar_2022_bike_type_Casual_df$electric_bike),
    sum(apr_2022_bike_type_Casual_df$classic_bike) + sum(apr_2022_bike_type_Casual_df$docked_bike) + sum(apr_2022_bike_type_Casual_df$electric_bike),
    sum(may_2022_bike_type_Casual_df$classic_bike) + sum(may_2022_bike_type_Casual_df$docked_bike) + sum(may_2022_bike_type_Casual_df$electric_bike),
    sum(jun_2022_bike_type_Casual_df$classic_bike) + sum(jun_2022_bike_type_Casual_df$docked_bike) + sum(jun_2022_bike_type_Casual_df$electric_bike),
    sum(jul_2022_bike_type_Casual_df$classic_bike) + sum(jul_2022_bike_type_Casual_df$docked_bike) + sum(jul_2022_bike_type_Casual_df$electric_bike),
    sum(aug_2022_bike_type_Casual_df$classic_bike) + sum(aug_2022_bike_type_Casual_df$docked_bike) + sum(aug_2022_bike_type_Casual_df$electric_bike),
    sum(sep_2022_bike_type_Casual_df$classic_bike) + sum(sep_2022_bike_type_Casual_df$docked_bike) + sum(sep_2022_bike_type_Casual_df$electric_bike),
    sum(oct_2022_bike_type_Casual_df$classic_bike) + sum(oct_2022_bike_type_Casual_df$docked_bike) + sum(oct_2022_bike_type_Casual_df$electric_bike),
    sum(nov_2022_bike_type_Casual_df$classic_bike) + sum(nov_2022_bike_type_Casual_df$docked_bike) + sum(nov_2022_bike_type_Casual_df$electric_bike),
    sum(dec_2022_bike_type_Casual_df$classic_bike) + sum(dec_2022_bike_type_Casual_df$docked_bike) + sum(dec_2022_bike_type_Casual_df$electric_bike)
  )
)

yearByMonth_2022_bike_type_Member_df <- data.frame(
  month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  monthNum = c(1:12),
  classic_bike = c(
    sum(jan_2022_bike_type_Member_df$classic_bike),
    sum(feb_2022_bike_type_Member_df$classic_bike),
    sum(mar_2022_bike_type_Member_df$classic_bike),
    sum(apr_2022_bike_type_Member_df$classic_bike),
    sum(may_2022_bike_type_Member_df$classic_bike),
    sum(jun_2022_bike_type_Member_df$classic_bike),
    sum(jul_2022_bike_type_Member_df$classic_bike),
    sum(aug_2022_bike_type_Member_df$classic_bike),
    sum(sep_2022_bike_type_Member_df$classic_bike),
    sum(oct_2022_bike_type_Member_df$classic_bike),
    sum(nov_2022_bike_type_Member_df$classic_bike),
    sum(dec_2022_bike_type_Member_df$classic_bike)
  ) ,
  docked_bike = c(
    sum(jan_2022_bike_type_Member_df$docked_bike),
    sum(feb_2022_bike_type_Member_df$docked_bike),
    sum(mar_2022_bike_type_Member_df$docked_bike),
    sum(apr_2022_bike_type_Member_df$docked_bike),
    sum(may_2022_bike_type_Member_df$docked_bike),
    sum(jun_2022_bike_type_Member_df$docked_bike),
    sum(jul_2022_bike_type_Member_df$docked_bike),
    sum(aug_2022_bike_type_Member_df$docked_bike),
    sum(sep_2022_bike_type_Member_df$docked_bike),
    sum(oct_2022_bike_type_Member_df$docked_bike),
    sum(nov_2022_bike_type_Member_df$docked_bike),
    sum(dec_2022_bike_type_Member_df$docked_bike)
  ) ,
  electric_bike = c(
    sum(jan_2022_bike_type_Member_df$electric_bike),
    sum(feb_2022_bike_type_Member_df$electric_bike),
    sum(mar_2022_bike_type_Member_df$electric_bike),
    sum(apr_2022_bike_type_Member_df$electric_bike),
    sum(may_2022_bike_type_Member_df$electric_bike),
    sum(jun_2022_bike_type_Member_df$electric_bike),
    sum(jul_2022_bike_type_Member_df$electric_bike),
    sum(aug_2022_bike_type_Member_df$electric_bike),
    sum(sep_2022_bike_type_Member_df$electric_bike),
    sum(oct_2022_bike_type_Member_df$electric_bike),
    sum(nov_2022_bike_type_Member_df$electric_bike),
    sum(dec_2022_bike_type_Member_df$electric_bike)
  ),
  total = c( 
    sum(jan_2022_bike_type_Member_df$classic_bike) + sum(jan_2022_bike_type_Member_df$docked_bike) + sum(jan_2022_bike_type_Member_df$electric_bike),
    sum(feb_2022_bike_type_Member_df$classic_bike) + sum(feb_2022_bike_type_Member_df$docked_bike) + sum(feb_2022_bike_type_Member_df$electric_bike),
    sum(mar_2022_bike_type_Member_df$classic_bike) + sum(mar_2022_bike_type_Member_df$docked_bike) + sum(mar_2022_bike_type_Member_df$electric_bike),
    sum(apr_2022_bike_type_Member_df$classic_bike) + sum(apr_2022_bike_type_Member_df$docked_bike) + sum(apr_2022_bike_type_Member_df$electric_bike),
    sum(may_2022_bike_type_Member_df$classic_bike) + sum(may_2022_bike_type_Member_df$docked_bike) + sum(may_2022_bike_type_Member_df$electric_bike),
    sum(jun_2022_bike_type_Member_df$classic_bike) + sum(jun_2022_bike_type_Member_df$docked_bike) + sum(jun_2022_bike_type_Member_df$electric_bike),
    sum(jul_2022_bike_type_Member_df$classic_bike) + sum(jul_2022_bike_type_Member_df$docked_bike) + sum(jul_2022_bike_type_Member_df$electric_bike),
    sum(aug_2022_bike_type_Member_df$classic_bike) + sum(aug_2022_bike_type_Member_df$docked_bike) + sum(aug_2022_bike_type_Member_df$electric_bike),
    sum(sep_2022_bike_type_Member_df$classic_bike) + sum(sep_2022_bike_type_Member_df$docked_bike) + sum(sep_2022_bike_type_Member_df$electric_bike),
    sum(oct_2022_bike_type_Member_df$classic_bike) + sum(oct_2022_bike_type_Member_df$docked_bike) + sum(oct_2022_bike_type_Member_df$electric_bike),
    sum(nov_2022_bike_type_Member_df$classic_bike) + sum(nov_2022_bike_type_Member_df$docked_bike) + sum(nov_2022_bike_type_Member_df$electric_bike),
    sum(dec_2022_bike_type_Member_df$classic_bike) + sum(dec_2022_bike_type_Member_df$docked_bike) + sum(dec_2022_bike_type_Member_df$electric_bike)
  )
)



# Adjust Table for Bar Plot
yearByMonth_2022_bike_type_Casual_subgrouped_df <-data.frame(biketype = rep(c("classic","docked","electric"),each=12),
                                                             monthNum = rep(yearByMonth_2022_bike_type_Casual_df$monthNum,3),
                                                             num_of_bike = c(yearByMonth_2022_bike_type_Casual_df$classic_bike,
                                                                             yearByMonth_2022_bike_type_Casual_df$docked_bike,
                                                                             yearByMonth_2022_bike_type_Casual_df$electric_bike)
                                                             )

yearByMonth_2022_bike_type_Member_subgrouped_df <-data.frame(biketype = rep(c("classic","docked","electric"),each=12),
                                                             monthNum = rep(yearByMonth_2022_bike_type_Member_df$monthNum,3),
                                                             num_of_bike = c(yearByMonth_2022_bike_type_Member_df$classic_bike,
                                                                             yearByMonth_2022_bike_type_Member_df$docked_bike,
                                                                             yearByMonth_2022_bike_type_Member_df$electric_bike)
)

# Get Year By Month but remove docked
yearByMonth_2022_bike_type_Casual_subgrouped_sansDocked_df <-data.frame(biketype = rep(c("classic","electric"),each=12),
                                                             monthNum = rep(yearByMonth_2022_bike_type_Casual_df$monthNum,2),
                                                             num_of_bike = c(yearByMonth_2022_bike_type_Casual_df$classic_bike,
                                                                             yearByMonth_2022_bike_type_Casual_df$electric_bike)
)

# Get Year By Month but remove docked
yearByMonth_2022_bike_type_Member_subgrouped_sansDocked_df <-data.frame(biketype = rep(c("classic","electric"),each=12),
                                                                        monthNum = rep(yearByMonth_2022_bike_type_Member_df$monthNum,2),
                                                                        num_of_bike = c(yearByMonth_2022_bike_type_Member_df$classic_bike,
                                                                                        yearByMonth_2022_bike_type_Member_df$electric_bike)
)

#------------


# Used to label plots
days_of_the_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                      "Thursday", "Friday", "Saturday")

# Year 2022 Bike Popularity for Casual Customers
graph_year2022casual_numOfBike_month_biketype <- ggplot(data=yearByMonth_2022_bike_type_Casual_subgrouped_df,aes(x=monthNum,y=num_of_bike, fill = biketype)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1))+
  labs(title = "Year 2022 Bike Popularity, Casual",
       subtitle = "Amount of each bike used by Casual Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption = "*K stands for 1000") +
  #geom_text(aes(label = signif(num_of_bike)), nudge_y = 3) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )

# Year 2022 Bike Popularity for Casual Customers - Sans Docked
graph_year2022casual_numOfBike_month_biketype_sansDocked <- ggplot(data=yearByMonth_2022_bike_type_Casual_subgrouped_sansDocked_df,aes(x=monthNum,y=num_of_bike, fill = biketype)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1))+
  labs(title = "Year 2022 Bike Popularity, Casual**",
       subtitle = "Amount of either classic or electic bike used by Casual Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption = "**This graph does not feature docked bikes, *K stands for 1000") +
  #geom_text(aes(label = signif(num_of_bike)), nudge_y = 3) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )+
  scale_fill_manual(values = c("#F8766D","#619CFF"))

# Individual
graph_year2022casual_numOfBike_month_classic <- ggplot(data=yearByMonth_2022_bike_type_Casual_df, aes(x=monthNum,y=classic_bike)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1), fill = "#F8766D")+
  labs(title = "Year 2022 Bike Popularity, Casual - Classic",
       subtitle = "Amount of classic bikes used by Casual Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(classic_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 

graph_year2022casual_numOfBike_month_docked <- ggplot(data=yearByMonth_2022_bike_type_Casual_df, aes(x=monthNum,y=docked_bike)) +
  geom_bar(stat = "identity",width=0.9,position=position_dodge(width=1), fill="#00BA38")+
  labs(title = "Year 2022 Bike Popularity, Casual - Docked",
       subtitle = "Amount of docked bikes used by Casual Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(docked_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 


graph_year2022casual_numOfBike_month_electric <- ggplot(data=yearByMonth_2022_bike_type_Casual_df, aes(x=monthNum,y=electric_bike)) +
  geom_bar(stat = "identity",width=0.9,position=position_dodge(width=1), fill="#619CFF")+
  labs(title = "Year 2022 Bike Popularity, Casual - Electric",
       subtitle = "Amount of electric bikes used by Casual Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(electric_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 

# Member
# Year 2022 Bike Popularity for Member Customers
graph_year2022member_numOfBike_month_biketype <- ggplot(data=yearByMonth_2022_bike_type_Member_subgrouped_df,aes(x=monthNum,y=num_of_bike, fill = biketype)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1))+
  labs(title = "Year 2022 Bike Popularity, Member",
       subtitle = "Amount of each bike used by Member Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption = "*K stands for 1000") +
  #geom_text(aes(label = signif(num_of_bike)), nudge_y = 3) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )

# Year 2022 Bike Popularity for Member Customers - Sans Docked
graph_year2022member_numOfBike_month_biketype_sansDocked <- ggplot(data=yearByMonth_2022_bike_type_Member_subgrouped_sansDocked_df,aes(x=monthNum,y=num_of_bike, fill = biketype)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1))+
  labs(title = "Year 2022 Bike Popularity, Member**",
       subtitle = "Amount of either classic or electic bike used by Member Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption = "**This graph does not feature docked bikes, *K stands for 1000") +
  #geom_text(aes(label = signif(num_of_bike)), nudge_y = 3) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )+
  scale_fill_manual(values = c("#F8766D","#619CFF"))

# Individual
graph_year2022member_numOfBike_month_classic <- ggplot(data=yearByMonth_2022_bike_type_Member_df, aes(x=monthNum,y=classic_bike)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1), fill = "#F8766D")+
  labs(title = "Year 2022 Bike Popularity, Member - Classic",
       subtitle = "Amount of classic bikes used by Member Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(classic_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 

graph_year2022member_numOfBike_month_docked <- ggplot(data=yearByMonth_2022_bike_type_Member_df, aes(x=monthNum,y=docked_bike)) +
  geom_bar(stat = "identity",width=0.9,position=position_dodge(width=1), fill="#00BA38")+
  labs(title = "Year 2022 Bike Popularity, Member - Docked",
       subtitle = "Amount of docked bikes used by Member Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(docked_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 


graph_year2022member_numOfBike_month_electric <- ggplot(data=yearByMonth_2022_bike_type_Member_df, aes(x=monthNum,y=electric_bike)) +
  geom_bar(stat = "identity",width=0.9,position=position_dodge(width=1), fill="#619CFF")+
  labs(title = "Year 2022 Bike Popularity, Member - Electric",
       subtitle = "Amount of electric bikes used by Member Customers",
       x="Month",
       y="Amount of Bikes Used (K*)",
       caption="K stands for 1000") +
  geom_text(aes(label = paste(c(floor(electric_bike/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.75,angle=0) +
  scale_y_continuous(limits = c(0,250000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 


#----------------------------------------------

# January 2022 Bike Popularity - Casual
jan_2022_bike_type_casual_subgrouped_df <- data.frame(biketype = rep(c("classic","docked","electric"),each=7),
                                                      day_of_week = rep(1:7,3),
                                                      num_of_bike = c(jan_2022_bike_type_Casual_df$classic_bike,
                                                                      jan_2022_bike_type_Casual_df$docked_bike,
                                                                      jan_2022_bike_type_Casual_df$electric_bike)
                                                      )

graph_jan_2022casual_numOfBike_day_biketype <- ggplot(data=jan_2022_bike_type_casual_subgrouped_df,aes(fill = biketype,x = day_of_week,y=num_of_bike)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1)) +
  labs(title = "January 2022 Bike Popularity - Casual",
       subtitle = "Amount of each bike used by Casual Customers",
       x="Day of the Week",
       y="Amount of Bikes Used")+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) 

# Year 2022 Amount of Rides - Casual Total
graph_year_2022casual_amountTrips_month <- ggplot(data = yearByMonth_2022_bike_type_Casual_df,aes(x=monthNum,y=total)) +
  geom_bar(stat = "identity",width=.9, fill="orange")+
  labs(title = "Year 2022 Casual Customer Bike Rides",
       subtitle = "Amount of trips by month",
       x="Month",
       y="Amount of Trips")+
  geom_text(aes(label = signif(total)), nudge_y = 9000) +
  scale_y_continuous(limits = c(0,430000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
  ) +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14),
        legend.position = "none"
  ) 


graph_year_2022casual_amountTrips_day <- ggplot(data=year_2022_bike_type_Casual_df,aes(x = c(1:7),y=total)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1),fill="violet") +
  labs(title = "Year 2022 Casual Customer Bike Rides",
       subtitle = "Amount of trips by day of the week",
       x="Day of the Week",
       y="Amount of Trips")+
  geom_text(aes(label = signif(total)), nudge_y = 15000) +
  scale_y_continuous(limits = c(0,500000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) 

#----------------------------------------------------------------

# Check Day of the Week vs Month
year_2022_month_day_Casual_df <- data.frame(monthNum = rep(as.factor(c(1:12)),each=7),
                                            dayNum = rep(c(1:7),12),
                                            num_of_bike = c(jan_2022_bike_type_Casual_df$total,
                                                            feb_2022_bike_type_Casual_df$total,
                                                            mar_2022_bike_type_Casual_df$total,
                                                            apr_2022_bike_type_Casual_df$total,
                                                            may_2022_bike_type_Casual_df$total,
                                                            jun_2022_bike_type_Casual_df$total,
                                                            jul_2022_bike_type_Casual_df$total,
                                                            aug_2022_bike_type_Casual_df$total,
                                                            sep_2022_bike_type_Casual_df$total,
                                                            oct_2022_bike_type_Casual_df$total,
                                                            nov_2022_bike_type_Casual_df$total,
                                                            dec_2022_bike_type_Casual_df$total)
                                            )

# Year 2022 Bike Popularity for Casual Customers
graph_year2022casual_numOfBike_week_month <- ggplot(data=year_2022_month_day_Casual_df,aes(x=dayNum,y=num_of_bike, fill = monthNum)) +
  geom_bar(stat = "identity",width=.9,position=position_dodge(width=1))+
  labs(title = "Year 2022 Bike Popularity - Casual",
       subtitle = "Amount of casual customer bike trips per Month",
       x="Day of the Week",
       y="Amount of Bikes Used") +
  scale_fill_discrete(name="Month",labels=c(month.name)) +
  #geom_text(aes(label = signif(num_of_bike)), nudge_y = 3) +
  scale_y_continuous(limits = c(0,100000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )

#----------------------------------------------------------------

df_customer_numOfTrip_vMonth <- data.frame(customer_type = rep(c("casual","member"),each=12),
                                           monthNum = rep(as.factor(c(1:12)),2),
                                           num_of_trips = c(sum(jan_2022_casual_df$num_of_trips),
                                             sum(feb_2022_casual_df$num_of_trips),
                                             sum(mar_2022_casual_df$num_of_trips),
                                             sum(apr_2022_casual_df$num_of_trips),
                                             sum(may_2022_casual_df$num_of_trips),
                                             sum(jun_2022_casual_df$num_of_trips),
                                             sum(jul_2022_casual_df$num_of_trips),
                                             sum(aug_2022_casual_df$num_of_trips),
                                             sum(sep_2022_casual_df$num_of_trips),
                                             sum(oct_2022_casual_df$num_of_trips),
                                             sum(nov_2022_casual_df$num_of_trips),
                                             sum(dec_2022_casual_df$num_of_trips),
                                             sum(jan_2022_member_df$num_of_trips),
                                             sum(feb_2022_member_df$num_of_trips),
                                             sum(mar_2022_member_df$num_of_trips),
                                             sum(apr_2022_member_df$num_of_trips),
                                             sum(may_2022_member_df$num_of_trips),
                                             sum(jun_2022_member_df$num_of_trips),
                                             sum(jul_2022_member_df$num_of_trips),
                                             sum(aug_2022_member_df$num_of_trips),
                                             sum(sep_2022_member_df$num_of_trips),
                                             sum(oct_2022_member_df$num_of_trips),
                                             sum(nov_2022_member_df$num_of_trips),
                                             sum(dec_2022_member_df$num_of_trips)
                                             )
                                          )

graph_year2022_tripfreq_customer <- ggplot(data=df_customer_numOfTrip_vMonth,aes(x=monthNum,y=num_of_trips, fill = customer_type)) +
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9))+
  labs(title = "Year 2022 Customers' Trip Frequency",
       subtitle = "Number of Trips per month",
       x="Per Month",
       y="Number of Trips (K*)",
       caption="K stands for 1000") +
  scale_fill_discrete(name="Customer") +
  geom_text(aes(label = paste(c(floor(num_of_trips/ 100)/10)," K",sep="")),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= 1,vjust=-.75,angle=-60) +
  scale_y_continuous(limits = c(0,500000) ,labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_x_discrete(
    breaks = seq_along(month.name), 
    labels = month.abb
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  )

#----------------------------------------------------------------

createTimeStamp = function(input_minute, input_second){
  temp_second = ifelse(input_second < 10,paste("0",input_second,sep = ""),
    as.character(input_second))
  return = paste(input_minute,temp_second,sep = ":")
}

year_2022_q2_df_fixed <- data.frame (member_casual = rep(c("casual","member"),each=7),
                                     day_of_week_num = c(year_2022_qAll_casual_df$day_of_week_num,year_2022_qAll_member_df$day_of_week_num),
                                     Q2 = c(year_2022_qAll_casual_df$Q2,year_2022_qAll_member_df$Q2),
                                     Q2seconds = minute(c(year_2022_qAll_casual_df$Q2,year_2022_qAll_member_df$Q2))*60+second(c(year_2022_qAll_casual_df$Q2,year_2022_qAll_member_df$Q2)),
                                     Q2Text = createTimeStamp(c(minute(c(year_2022_qAll_casual_df$Q2,year_2022_qAll_member_df$Q2))),
                                                              c(second(c(year_2022_qAll_casual_df$Q2,year_2022_qAll_member_df$Q2)))
                                                              )
                                     )



ggplot(data=year_2022_q2_df_fixed,aes(x=day_of_week_num,y=Q2seconds, fill=member_casual))+
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9)) +
  labs(title = "Year 2022 Q2 Bike Trip, Casual vs Member",
       subtitle = "The second quantile of casual and member rider's length of time",
       x="Day of the Week",
       y="Length of Trip (HH:MM:SS)")+
  geom_text(aes(label = Q2Text ),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.5,angle=0) +
  scale_y_time(limits = as.hms(c('00:00:00', '00:30:00')))+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) +
  scale_fill_manual(values = c("orange","violet"),name="Customer")




year_2022_q2_casual_df_fixed <- data.frame(day_of_week_num = c(year_2022_qAll_casual_df$day_of_week_num),
                                           Q2 = c(year_2022_qAll_casual_df$Q2),
                                           Q2seconds = minute(c(year_2022_qAll_casual_df$Q2))*60+second(c(year_2022_qAll_casual_df$Q2)),
                                           Q2Text = createTimeStamp(c(minute(year_2022_qAll_casual_df$Q2)),
                                                                    c(second(year_2022_qAll_casual_df$Q2))
                                           )
)

ggplot(data=year_2022_q2_casual_df_fixed,aes(x=day_of_week_num,y=Q2seconds))+
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9), fill="orange") +
  labs(title = "Year 2022 Q2 Bike Trip, Casual",
       subtitle = "The second quantile of casual rider's length of time",
       x="Day of the Week",
       y="Length of Trip (HH:MM:SS)",
       legend="Customer")+
  geom_text(aes(label = Q2Text ),
            position = position_dodge2(width = 0.9, preserve = "single"),size=4.5,hjust= .5,vjust=-.5,angle=0) +
  scale_y_time(limits = as.hms(c('00:00:00', '00:30:00')))+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) 

year_2022_q2_member_df_fixed <- data.frame(day_of_week_num = c(year_2022_qAll_member_df$day_of_week_num),
                                           Q2 = c(year_2022_qAll_member_df$Q2),
                                           Q2seconds = minute(c(year_2022_qAll_member_df$Q2))*60+second(c(year_2022_qAll_member_df$Q2)),
                                           Q2Text = createTimeStamp(c(minute(year_2022_qAll_member_df$Q2)),
                                                                    c(second(year_2022_qAll_member_df$Q2))
                                           )
)

ggplot(data=year_2022_q2_member_df_fixed,aes(x=day_of_week_num,y=Q2seconds))+
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9), fill="violet") +
  labs(title = "Year 2022 Q2 Bike Trip, Member",
       subtitle = "The second quantile of member rider's length of time",
       x="Day of the Week",
       y="Length of Trip (HH:MM:SS)",
       legend="Customer")+
  geom_text(aes(label = Q2Text ),
            position = position_dodge2(width = 0.9, preserve = "single"),size=4.5,hjust= .5,vjust=-.5,angle=0) +
  scale_y_time(limits = as.hms(c('00:00:00', '00:30:00')))+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) 

## Let's Check docked Bikes Casual

year_2022_clean_casual_docked_bike <- subset(year_2022_clean,member_casual=="casual"& rideable_type=="docked_bike")

nrow(year_2022_clean_casual_docked_bike) # 176372 
nrow(subset(year_2022_clean,member_casual=="casual")) #2319783

year_2022_qAll_casual_docked_bike_df <- create_df_q_data(year_2022_clean_casual_docked_bike,"casual")

createTimeStampWHH = function(input_hour,input_minute, input_second){
  temp_minute = ifelse(input_minute < 10,paste("0",input_minute,sep = ""),
                       as.character(input_minute))
  temp_second = ifelse(input_second < 10,paste("0",input_second,sep = ""),
                       as.character(input_second))
  return = ifelse(input_hour == 0,paste(input_minute,temp_second,sep = ":"),paste(input_hour,temp_minute,temp_second,sep = ":"))
    
}

year_2022_q2_casual_docked_bike_df <- data.frame(day_of_week_num = c(year_2022_qAll_casual_docked_bike_df$day_of_week_num),
                                           Q2 = c(year_2022_qAll_casual_docked_bike_df$Q2),
                                           Q2seconds = hour(c(year_2022_qAll_casual_docked_bike_df$Q2))*60*60+minute(c(year_2022_qAll_casual_docked_bike_df$Q2))*60+second(c(year_2022_qAll_casual_docked_bike_df$Q2)),
                                           Q2Text = createTimeStampWHH(c(hour(year_2022_qAll_casual_docked_bike_df$Q2)),
                                                                       c(minute(year_2022_qAll_casual_docked_bike_df$Q2)),
                                                                       c(second(year_2022_qAll_casual_docked_bike_df$Q2))
                                           )
)

ggplot(data=year_2022_q2_casual_docked_bike_df,aes(x=day_of_week_num,y=Q2seconds))+
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9), fill="red") +
  labs(title = "Year 2022 Q2 Bike Trip, Casual - Docked",
       subtitle = "The second quantile of casual rider's length of time using docked bikes",
       x="Day of the Week",
       y="Length of Trip (HH:MM:SS)",
       legend="Customer")+
  geom_text(aes(label = Q2Text ),
            position = position_dodge2(width = 0.9, preserve = "single"),size=4.5,hjust= .5,vjust=-.5,angle=0) +
  scale_y_time(limits = as.hms(c('00:00:00', '1:10:00')))+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) 

# Now let's show it side by side with Members
year_2022_q2_dockedcasualvsmember <- data.frame (member_casual = rep(c("casual","member"),each=7),
                                     day_of_week_num = c(year_2022_q2_casual_docked_bike_df$day_of_week_num,year_2022_qAll_member_df$day_of_week_num),
                                     Q2 = c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2),
                                     Q2seconds = hour(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2))*60*60+minute(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2))*60+second(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2)),
                                     Q2Text = createTimeStampWHH(c(hour(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2))),
                                                                 c(minute(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2))),
                                                                 c(second(c(year_2022_q2_casual_docked_bike_df$Q2,year_2022_qAll_member_df$Q2)))
                                     )
)



ggplot(data=year_2022_q2_dockedcasualvsmember,aes(x=day_of_week_num,y=Q2seconds, fill=member_casual))+
  geom_bar(stat = "identity",width=.86,position=position_dodge(width=.9)) +
  labs(title = "Year 2022 Q2 Bike Trip, Casual using Docked vs Member",
       subtitle = "The second quantile of casual rider's length of time using docked bikes vs member's",
       x="Day of the Week",
       y="Length of Trip (HH:MM:SS)")+
  geom_text(aes(label = Q2Text ),
            position = position_dodge2(width = 0.9, preserve = "single"),size=3.5,hjust= .5,vjust=-.5,angle=0) +
  scale_y_time(limits = as.hms(c('00:00:00', '01:10:00')))+
  scale_x_continuous(
    breaks = seq_along(days_of_the_week), 
    labels = days_of_the_week
  )  +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
  ) +
  scale_fill_manual(values = c("red","violet"),name="Customer")


