
## Problem statement

Given the telematics data for each trip and the label if the trip is tagged as dangerous driving, derive a model that can detect dangerous driving trips.

### The  Analysis was performed as follows:
* **Data  understanding** : Loaded safety dataset into Rstudio, performed preliminary checks and removed a few outliers.
* **Feature engineering** : seperated safety data into the following sub-datasets  and feature engineered each separatley to be later combined  into a unified dataset:
    + sensor data
    + speed data
    + gps data 
    + trip duration data
* **Model building** : Combine all features into a training dataset. Removed observations which had minimum speed value less than 0 as test dataset. Trained a cross validated randomforest model on remaining dataset. Got an **auc of 1.0** on unseen data. Hope the auc is the same on hold-out test dataset.


## Data understanding

There were 18 bookingIDs that were duplicate and had both 0 and 1 as the label. Removed these observations. Also removed observations that had trip duration more than 9000 seconds. There were some bookings which showed a massive number in the speed column.The maximum speed was limited to 33.34 mt/s around 120 kmph. There were a few drivers going at 240 kmph!!(noisy data)

## Feature engineering

From the accelerometer and gyroscope data: energy, median absolute deviation, angles in xz and yz planes were created. Since applying a fast-fourier transformation on irregularly sampled data leads to noise, upon median scaling, a lomb-scargle periodgram, robust to irregular data such as the speed columns was applied on the accelerometer data to extract the peak and the first 10 dominating frequencies for each of these. standard features such as minimum, maximum, standard deviation, and inter quantile range were created.

From the speed data, a speed vector was constructed that represents the entire trip as a string of event changes. This string was used to subset features of length three and four that shows the counts of these features during the trip. For example *ADCS* features gives counts of number of times the driver accelerated, then decelrated until a stop. All possibble combinations of patterns present in the data were extracted. trip duration was calculated. One interesting thing to note is that speeds for about 5000 or more bookings is negative. These could be eliminated but I chose to compute a speed_min feature and let the model decide if it is required or not. A better decision could have been taken if I understand how this data has been collected. Another alarming finding is that at second 0 or 1 marks, the speeds are shockingly high. There are multiple bookings where the speeds are over 40 kmph (upon conversion). This could mean either the driver started the trip after the passenger is seated or the values are erroneous. For now, I chose to use this data. Also sudden break related features could have been extracted from this column. 

median, standard deviation and iqr were calculated from the Accuracy and Bearing data. Bearing could have been used to compute rate of change of direction features. Bearing column combined with speed column could have been used together to extract features related to sudden turns.

## Model building

Four correlated features were removed and random forest model was built. If only the data was continuous and captured the entire trip, it would be interesting to build a cnn model with values at each timestep as features. This model would automatically extract all the relevant features. 

## Steps to test the model

Please run the test_preperation.R script to first preprocess the  data. The preprocessed test data will be in the output folder. (Preprocessing takes a lot of time) Run the prediction.R script to predict on the test dataset.

## Conclusion

It is really a great dataset. Overall, I am happy that I spent most of my time feature engineering and but unhappy that I could not do all the things I wanted to do with the modeling part. It was still a great learning experience and will continue to work on this data. 
