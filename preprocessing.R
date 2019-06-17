# !diagnostics off

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)
library(corrplot)
library(lomb)
library(signal)


source("functions.R")
labels <- readr::read_csv("data/raw/safety/labels/part-00000-e9445087-aa0a-433b-a7f6-7f4c19d78ad6-c000.csv")
labels$label <- as.factor(as.character(labels$label))
labels <- labels %>% arrange(bookingID)

all_file_names <- list.files("data/raw/safety/features/",full.names = T)
features <- do.call(rbind,lapply(all_file_names,function(x) read_csv(x)))


##Duplicates
sum(duplicated(labels$bookingID))
length(unique(labels$bookingID))
length(unique(features$bookingID)) # 20000 unique booking ids
duplicates <- labels[duplicated(labels$bookingID),]
all_duplicates <- labels[labels$bookingID %in% duplicates$bookingID,] # 18 duplicates with both the labels. 
table(all_duplicates)
sum(unique(features$bookingID) %in% all_duplicates$bookingID) # all these 18 booking IDs are available in the dataset. Remove them.
sum((features$bookingID %in% all_duplicates$bookingID)) # need to  remove 18857 observations
features <- features[! (features$bookingID %in% all_duplicates$bookingID) ,]
labels  <- labels[(labels$bookingID %in% features$bookingID),]
###

tail(sort(features$second),100)
outliers_in_second_feature <- features[features$second > 9000,c("bookingID","second")]
length(unique(outliers_in_second_feature$bookingID)) # remove 5 bookingIDs
features <-  features[! features$bookingID %in%  unique(outliers_in_second_feature$bookingID),]
labels <- labels[! labels$bookingID %in%  unique(outliers_in_second_feature$bookingID), ]

summary(features)

# Cap speed value
features$Speed[features$Speed >= 33.34] = 33.34

# Sort data
data <- features %>% group_by(bookingID) %>%
  arrange(bookingID,second) %>%
  ungroup()

# Subset data into multiple datasets so it is easier to extract  features for individual subsets.
sensor_data  <-  data[,c("bookingID","second","acceleration_x","acceleration_y","acceleration_z","gyro_x","gyro_y","gyro_z")]
speed_data <- data[,c("bookingID","second","Speed")]
gps_data <- data[,c("bookingID","second","Accuracy","Bearing")]
trip_duration_data <- data[,c("bookingID","second")]


# Compute  Energy and magnitude  for acceleration and gyroscope
sensor_data <- sensor_data %>% 
  group_by(bookingID) %>%
  mutate(a_x_energy = sum(acceleration_x^2)/length(acceleration_x)) %>%
  mutate(a_y_energy = sum(acceleration_y^2)/length(acceleration_y)) %>%
  mutate(a_z_energy = sum(acceleration_z^2)/length(acceleration_z)) %>%
  
  mutate(g_x_energy = sum(gyro_x^2)/length(gyro_x)) %>%
  mutate(g_y_energy = sum(gyro_y^2)/length(gyro_y)) %>%
  mutate(g_z_energy = sum(gyro_z^2)/length(gyro_z)) %>%
  ungroup()

sensor_data <- sensor_data %>%  
  group_by(bookingID) %>%
  mutate(a_mag  = sqrt((acceleration_x)^2+(acceleration_y)^2+(acceleration_z)^2)) %>%
  mutate(g_mag  = sqrt((gyro_x)^2+(gyro_y)^2+(gyro_z)^2)) %>%
  ungroup()


# meadian absolute deviation
sensor_data <- sensor_data %>% 
  group_by(bookingID) %>%
  mutate(a_x_mad = mad(acceleration_x)) %>%
  mutate(a_y_mad = mad(acceleration_y)) %>%
  mutate(a_z_mad = mad(acceleration_z)) %>%
  mutate(g_x_mad = mad(gyro_x)) %>%
  mutate(g_y_mad = mad(gyro_y)) %>%
  mutate(g_z_mad = mad(gyro_z)) %>%
  ungroup()

# Angles of gyro in xz and yz plane  
sensor_data <- sensor_data %>% 
  group_by(bookingID) %>%
  mutate(g_angle_xz = atan2(gyro_x,gyro_z)) %>%
  mutate(g_angle_yz = atan2(gyro_y,gyro_z)) %>%
  ungroup()

# Seperate single valued features from other sensor data so rolled up dataset can be merged.
summarized_columns <- names(sensor_data)[c(grep("_._",names(sensor_data)))]
sensor_data_summarized_columns <- sensor_data %>% 
  group_by(bookingID) %>%
  select(summarized_columns) %>%
  unique() %>%
  ungroup()


# correlation plot shows  there are correlations
cor <- cor(sensor_data[,! names(sensor_data) %in% c("bookingID",summarized_columns)])
corrplot::corrplot(cor)

# drop acceleration_z, gyro_x,gyro_y,gyro_z
sensor_data_no_corr  <- sensor_data %>%
  select(-c(acceleration_z,gyro_x,gyro_y,gyro_z))

# compute summary statistics
sensor_data_no_corr <- sensor_data_no_corr %>% group_by(bookingID) %>%
  select(- c(second,summarized_columns)) %>%
       summarise_all(list(min  = min,
                      max = max,
                      mean = mean,
                      sd = sd,
                      iqr = IQR))  %>%
      ungroup()


# Get lomb  features. lomb_scargle function is in functions.R
sensor_data_for_frequency <- sensor_data %>% 
  group_by(bookingID) %>%
  select(second,acceleration_x,acceleration_y,acceleration_z) %>%
  nest()

sensor_data_for_frequency$data <- lapply(sensor_data_for_frequency$data,function(x)lomb_scargle(x))
sensor_data_for_frequency  <- unnest(sensor_data_for_frequency)

sensor_data_for_frequency <- unique(sensor_data_for_frequency[,!names(sensor_data_for_frequency) %in% 
                                                                c("second","acceleration_x","acceleration_y","acceleration_z")])

# Summarize all sensor data
sensor_data_features <-  inner_join(sensor_data_for_frequency,sensor_data_summarized_columns,by = "bookingID")
sensor_data_features <-  inner_join(sensor_data_features,sensor_data_no_corr,by = "bookingID")


saveRDS(sensor_data_features,"data/processed/sensor_data_features")


###########################



hist(speed_data$Speed[speed_data$second == 0])
neg_speed <- unique(speed_data$bookingID[speed_data$second ==1 & speed_data$Speed < 0])
quantile(speed_data$Speed[speed_data$bookingID %in% neg_speed],c(seq(0.05,1,0.05)))


# summary stats  for speed
speed_data  <- speed_data %>%
  group_by(bookingID) %>%
  mutate(speed_min = min(Speed),
         speed_max = max(Speed),
         speed_mean = mean(Speed),
         speed_sd =  sd(Speed),
         speed_iqr = IQR(Speed)) %>%
  ungroup()
         

    
speed_data  <- speed_data %>%
  group_by(bookingID) %>%
  nest() 
  

# Create a speed vector  from speed column. FUnction in function.R
speed_data$data <- lapply(speed_data$data,function(x) { x[,"speed_vector"] = concatenate_speed(x[["Speed"]]); return(x)})

speed_data <- unnest(speed_data)
speed_data <- ungroup(speed_data)

speed_vector <- unique(speed_data[,c("bookingID","speed_vector")])
speed_vector[,"no_of_stops"] <- stri_count(speed_vector$speed_vector,regex = "S")


combn_3 <- stri_remove_empty(gsub("\\b\\w{1,2}\\b","",
                                  gsub("(.)\\1+","",unique(unlist(strsplit(
                                    gsub("(.{3})","\\1 ",speed_vector$speed_vector),
                                    split = " "))))))
combn_4 <- stri_remove_empty(
                gsub("\\b\\w{1,3}\\b","",
                     gsub("(.)\\1+","",unique(unlist(strsplit(
                       gsub("(.{4})","\\1 ",speed_vector$speed_vector),
                       split = " "))))))

for(x in  combn_3){
  speed_vector[paste0(x,"_combn_3")] = stri_count(speed_vector$speed_vector,regex = x)
}

for(x in  combn_4){
  speed_vector[paste0(x,"_combn_4")] = stri_count(speed_vector$speed_vector,regex = x)
}


speed_vector <- unnest(speed_vector)
speed_vector <- unique(speed_vector[,! names(speed_vector) %in% c("speed_vector")])

speed_data <- unique(speed_data[, !names(speed_data) %in% c("Speed","second","speed_vector")])  

speed_data_features <- inner_join(speed_data,speed_vector)
saveRDS(speed_data_features,"data/processed/speed_data_features")

##########################
trip_duration_data_features <- trip_duration_data %>%
  group_by(bookingID) %>%
  mutate(trip_duration = max(second)) %>%
  select(-second) %>%
  unique() %>%
  ungroup()

saveRDS(trip_duration_data_features,"data/processed/trip_duration_data_features")

#############################
gps_data_features <- gps_data %>% 
  group_by(bookingID) %>%
  mutate(accuracy_median = median(Accuracy),
         accuracy_sd =  sd(Accuracy),
         accuracy_iqr = IQR(Accuracy)) %>%
  
  mutate(bearing_median = median(Bearing),
         bearing_sd =  sd(Bearing),
         bearing_iqr = IQR(Bearing)) %>%
  select(-second,-Accuracy,-Bearing) %>%
  unique() %>%
  ungroup()

saveRDS(gps_data_features,"data/processed/gps_data_features")

trip_data_all_features <-  inner_join(sensor_data_features,speed_data_features,by = "bookingID")
trip_data_all_features <-  inner_join(trip_data_all_features,trip_duration_data_features,by = "bookingID")
trip_data_all_features <-  inner_join(trip_data_all_features,gps_data_features,by = "bookingID")
saveRDS(trip_data_all_features,"data/processed/trip_data_all_features")

trip_data_all_features_with_label <-  inner_join(trip_data_all_features,labels,by  = "bookingID")
saveRDS(trip_data_all_features_with_label,"data/raw/trip_data_all_features_with_label")








