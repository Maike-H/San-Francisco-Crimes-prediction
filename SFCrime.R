# Predicting crimes in San Francisco using machine learning
# Overview
# 1. Introduction and summary (in the rmd-Report-file)
# 2. Pre-processing
# 2.1. Downloading the data and libraries
# 2.2. Excursion robberies
# 2.2.1. Observation
# 2.2.2. Visualization
# 2.3. Create the final data set
# 2.3.1. Observation
# 2.3.2. Visualization
# 3. Methods and analysis 
# 3.1 Modelling
# 4. Results of the training 
# 5. Conclusion and using best performing model for final prediction


# 2.1 Loading and installing the libraries needed to read and wrangle the data

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(raster)) install.packages("raster") 
if(!require(foreign)) install.packages("foreign")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(bigreadr)) install.packages("bigreadr")
if(!require(ggplot2)) install.packages("ggplot2")

library(raster)
library(foreign)
library(tidyverse)
library(lubridate)
library(dplyr)
library(caret)
library(bigreadr)
library(ggplot2)


# Download the file from the GeoDa Data and Lab platform. The source of the data is 
# the San Francisco Police Department Crime Incident Reporting System.
sf <- tempfile()
download.file("https://geodacenter.github.io/data-and-lab//data/SFCrime_July_Dec2012.zip", sf)

# unzip and observe the containing data to choose the files needed
unzipped <- unzip(sf)
unzipped

# Store the data frames for the general blocks data and the four different kind of crimes
cartheft <- read.dbf("./SFCrime_July_Dec2012 2/Crime Events/sf_cartheft.dbf")
drugs <- read.dbf("./SFCrime_July_Dec2012 2/Crime Events/sf_drugs.dbf")
vandalism <- read.dbf("./SFCrime_July_Dec2012 2/Crime Events/sf_vandalism.dbf")
robbery <- read.dbf("./SFCrime_July_Dec2012 2/Crime Events/sf_robbery.dbf")

# Store the map of San Francisco
block_map <- shapefile("./SFCrime_July_Dec2012 2/SF PD Plots/SFCrime_blocks.shp")


# 2.2  First take a look at only the robbery data
# Observe the robbery data, 2761 incidents of 45 different categories of description
head(robbery)
dim(robbery)
length(unique(robbery$Descript))

# Sort by number of most happened incidents
rob_arranged <- robbery %>% 
  group_by(Descript) %>% 
  summarise(Count = length(Descript)) %>% 
  arrange(desc(Count))
head(rob_arranged, 10)

# See which incidents happens only less. Gun robberies of banks or in houses
rob_less <- robbery %>% 
  group_by(Descript) %>% 
  summarise(Count = length(Descript)) %>% 
  arrange(Count)
head(rob_less, 10)

# Filter for all incidents with guns using the string detect function "grep"
guns <- rob_arranged$Descript[grep("GUN", rob_arranged$Descript)]
gun_rob <- rob_arranged %>% filter(Descript %in% guns)

# In 20 percent of the cases of robbery a gun was involved 
sum((gun_rob$Count) / sum(rob_arranged$Count))*100


# 5 Locations of the most happening robberies in descending order
rob_location <- robbery %>% 
  group_by (Location) %>% 
  summarise(Count = length(Location)) %>% 
  arrange(desc(Count))
head(rob_location, 5)

# 5 Safest places in San Francisco regarding robberies
rob_location_safe <- robbery %>% 
  group_by (Location) %>% 
  summarise(Count = length(Location)) %>% 
  arrange(Count)
head(rob_location_safe, 5)

# Plotting the coordinates of the 100 most happened robberies, so the unsafest places
most_robberies <- head(rob_location, 100) %>%
  left_join(robbery, by = "Location")

most_robberies %>% ggplot(aes(X, Y))+
  geom_point() +
  labs(title = "Locations of most robberies")


# 2.3 Create a united table
# Join all crimes in one table
crimes <- robbery %>%
  full_join(cartheft)
crimes <- crimes %>%
  full_join(drugs)
crimes <- crimes %>%
  full_join(vandalism)


# Plot all districts with their number of crimes
crimes %>% group_by(PdDistrict) %>%
  summarize(Count = length(PdDistrict)) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x = reorder(PdDistrict, Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(x ="Districts", y = "Number of crimes", title = "Crimes in San Francisco") +
  coord_flip()


# Very large number of different types of crimes
length(unique(crimes$Descript))

# Cleaning the Descripts of crimes
crime_clean<-crimes %>%
  mutate(Crime = fct_recode(Descript,
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, VANDALISM",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, GRAFFITI",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, VANDALISM OF VEHICLES",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, TIRE SLASHING",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, BREAKING WINDOWS",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, BREAKING WINDOWS WITH BB GUN",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, STREET CARS/BUSES",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, FICTITIOUS PHONE CALLS",
                     "Malicious Mischief" = "MALICIOUS MISCHIEF, BUILDING UNDER CONSTRUCTION",
                     "Robbery with bodily force" = "ROBBERY ON THE STREET, STRONGARM",
                     "Robbery with bodily force" = "ROBBERY, BODILY FORCE",
                     "Robbery with bodily force" = "ROBBERY OF A RESIDENCE WITH BODILY FORCE",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY CHAIN STORE WITH BODILY FORCE",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY COMM. ESTAB. WITH BODILY FORCE",
                     "Robbery with bodily force" = "ROBBERY OF A COMMERCIAL ESTABLISHMENT, STRONGARM",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY WITH BODILY FORCE",
                     "Robbery with bodily force" = "ROBBERY OF A CHAIN STORE WITH BODILY FORCE",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY OF A BANK WITH BODILY FORCE",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY RESIDENCE WITH BODILY FORCE",
                     "Robbery with bodily force" = "ROBBERY OF A SERVICE STATION WITH BODILY FORCE",
                     "Robbery with bodily force" = "ROBBERY OF A BANK WITH BODILY FORCE",
                     "Robbery with bodily force" = "ATTEMPTED ROBBERY ON THE STREET WITH BODILY FORCE",
                     "Robbery with a Knife" = "ROBBERY, ARMED WITH A KNIFE",
                     "Robbery with a Knife" = "ROBBERY OF A RESIDENCE WITH A KNIFE",
                     "Robbery with a Knife" = "ROBBERY ON THE STREET WITH A KNIFE",
                     "Robbery with a Knife" = "ROBBERY OF A COMMERCIAL ESTABLISHMENT W/ A KNIFE",
                     "Robbery with a Knife" = "ATTEMPTED ROBBERY ON THE STREET WITH A KNIFE",
                     "Robbery with a Knife" = "ATTEMPTED ROBBERY WITH A KNIFE",
                     "Robbery with a Knife" = "ROBBERY OF A CHAIN STORE WITH A KNIFE",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY RESIDENCE WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY OF A RESIDENCE WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY, ARMED WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY ON THE STREET WITH A GUN",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY ON THE STREET W/DEADLY WEAPON",
                     "Robbery with a weapon" = "ROBBERY, ARMED WITH A DANGEROUS WEAPON",
                     "Robbery with a weapon" = "ROBBERY ON THE STREET WITH A DANGEROUS WEAPON",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY WITH A DEADLY WEAPON",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY ON THE STREET WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY OF A CHAIN STORE WITH A DANGEROUS WEAPON",
                     "Robbery with a weapon" = "ROBBERY OF A COMMERCIAL ESTABLISHMENT WITH A GUN",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY OF A SERVICE STATION WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY OF A CHAIN STORE WITH A GUN",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY CHAIN STORE WITH DEADLY WEAPON",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY COMM. ESTABLISHMENT WITH A GUN",
                     "Robbery with a weapon" = "ROBBERY OF A RESIDENCE WITH A DANGEROUS WEAPON",
                     "Robbery with a weapon" = "ROBBERY OF A COMMERCIAL ESTABLISHMENT W/ WEAPON",
                     "Robbery with a weapon" = "ROBBERY OF A BANK WITH A DANGEROUS WEAPON",
                     "Robbery with a weapon" = "ROBBERY OF A BANK WITH A GUN",
                     "Robbery with a weapon" = "ATTEMPTED ROBBERY OF A BANK WITH A GUN",
                     "Carjacking with bodily force" = "CARJACKING WITH BODILY FORCE",
                     "Carjacking with a knife" = "CARJACKING WITH A KNIFE",
                     "Carjacking with a weapon" = "CARJACKING WITH A DANGEROUS WEAPON",
                     "Carjacking with a weapon" = "CARJACKING WITH A GUN",
                     "Stolen vehicle" = "ATTEMPTED STOLEN VEHICLE",
                     "Stolen vehicle" = "STOLEN TRUCK",
                     "Stolen vehicle" = "STOLEN AND RECOVERED VEHICLE",
                     "Stolen vehicle" = "STOLEN AUTOMOBILE",
                     "Stolen vehicle" = "STOLEN MOTORCYCLE",
                     "Stolen vehicle" = "STOLEN MISCELLANEOUS VEHICLE",
                     "Stolen vehicle" = "STOLEN TRAILER",
                     "Possesion of drugs" = "POSSESSION OF MARIJUANA",
                     "Possesion of drugs" = "POSSESSION OF COCAINE",
                     "Possesion of drugs" = "POSSESSION OF HEROIN",
                     "Possesion of drugs" = "POSSESSION OF CONTROLLED SUBSTANCE",
                     "Possesion of drugs" = "UNDER INFLUENCE OF DRUGS IN A PUBLIC PLACE",
                     "Possesion of drugs" = "POSSESSION OF BASE/ROCK COCAINE",
                     "Possesion of drugs" = "POSSESSION OF NARCOTICS PARAPHERNALIA",
                     "Possesion of drugs" = "POSSESSION OF METH-AMPHETAMINE",
                     "Possesion of drugs" = "POSSESSION OF METHADONE",
                     "Possesion of drugs" = "POSSESSION OF OPIATES",
                     "Possesion of drugs" = "POSSESSION OF AMPHETAMINE",
                     "Possesion of drugs" = "POSSESSION OF BARBITUATES",
                     "Possesion of drugs" = "POSSESSION OF HALLUCINOGENIC",
                     "Possesion of drugs" = "POSSESSION OF OPIUM DERIVATIVE",
                     "Possesion of drugs" = "FURNISHING MARIJUANA",
                     "Possesion of drugs" = "PLANTING/CULTIVATING MARIJUANA",
                     "Possesion of drugs" = "POSSESSION OF OPIUM",
                     "Encouraging minor to use marijuana" = "ENCOURAGING MINOR TO USE MARIJUANA",
                     "Sale of drugs" = "POSSESSION OF METHADONE FOR SALES",
                     "Sale of drugs" = "POSSESSION OF METH-AMPHETAMINE FOR SALE",
                     "Sale of drugs" = "POSSESSION OF HALLUCINOGENIC FOR SALES",
                     "Sale of drugs" = "POSSESSION OF CONTROLLED SUBSTANCE FOR SALE",
                     "Sale of drugs" = "POSSESSION OF BASE/ROCK COCAINE FOR SALE",
                     "Sale of drugs" = "POSSESSION OF MARIJUANA FOR SALES",
                     "Sale of drugs" = "SALE OF METH-AMPHETAMINE",
                     "Sale of drugs" = "POSSESSION OF COCAINE FOR SALES",
                     "Sale of drugs" = "POSSESSION OF OPIATES FOR SALES",
                     "Sale of drugs" = "POSSESSION OF HEROIN FOR SALES",
                     "Sale of drugs" = "SALE OF MARIJUANA",
                     "Sale of drugs" = "POSSESSION OF AMPHETAMINE FOR SALES",
                     "Sale of drugs" = "SALE OF HEROIN",
                     "Sale of drugs" = "SALE OF METHADONE",
                     "Sale of drugs" = "SALE OF OPIATES",
                     "Sale of drugs" = "SALE OF METHADONE",
                     "Sale of drugs" = "SALE OF OPIATES",
                     "Sale of drugs" = "SALE OF COCAINE",
                     "Sale of drugs" = "SALE OF BASE/ROCK COCAINE",
                     "Sale of drugs" = "SALE OF CONTROLLED SUBSTANCE",
                     "Transportation of drugs" = "TRANSPORTATION OF COCAINE",
                     "Transportation of drugs" = "TRANSPORTATION OF MARIJUANA",
                     "Transportation of drugs" = "TRANSPORTATION OF METH-AMPHETAMINE",
                     "Transportation of drugs" = "TRANSPORTATION OF OPIATES",
                     "Transportation of drugs" = "TRANSPORTAION OF CONTROLLED SUBSTANCE",
                     "Damage" = "DAMAGE TO FIRE ALARM APPARATUS",
                     "Damage" = "DAMAGE/DESTRUCTION OF MAIL",
                     "Damage" = "DAMAGE TO MAIL BOX",
                     "Forge or alter prescription" = "FORGE OR ALTER PRESCRIPTION"))

unique(crime_clean$Crime)

# Plot the crimes
crime_clean %>%
  group_by(Crime) %>%
  summarize(Acts = length(Crime)) %>%
  arrange(desc(Acts)) %>%
  ggplot(aes(x = reorder(Crime, Acts), y = Acts)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x ="Type of act", y ="Number of acts, log-scale", title = "Type of crimes in San Francisco") +
  coord_flip()


# Reunite with the crimes table to get all columns back
sf_crimes <- crime_clean %>%
  left_join(crimes)

# Distribution months
sf_crimes <- sf_crimes %>%
  mutate(Month = month(Date, label=TRUE, abbr=TRUE))

sf_crimes %>% group_by(Month) %>%
  summarize(Count = length(Month)) %>%
  ggplot(aes(Month, Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Months", y = "Number of crimes", title = "Crime distribution July - December")

# Distribution weekdays with ordered days
sf_crimes %>% 
  group_by(DayOfWeek) %>%
  summarize(Count = length(DayOfWeek)) %>%
  ggplot(aes(ordered(DayOfWeek, 
                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
                                "Saturday", "Sunday")), Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Weekdays", y = "Number of crimes", 
       title = "Crime distribution over days of week")

# Crimes per day
sf_crimes %>%
  group_by(Crime, DayOfWeek) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = Crime, 
             y = ordered(DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", 
                                               "Saturday", "Sunday")))) +
  geom_tile(aes(fill = count)) +
  labs(x = "Crime", y = "Day of week", title = "Drug offenses are more often around Wednesdays, 
       robbery on Fridays and vandalism on weekends") +
  scale_fill_viridis_c("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()


# San Francisco map
plot(block_map)

# Filter only crimes on Friday, 12th of October
Friday <- sf_crimes %>%
  filter(Date == "2012-10-12")

#Convert the spatial points matrix of the map into a data frame to use it with ggplot
#source of the function("https://raw.githubusercontent.com/tidyverse/ggplot2/master/R/fortify-spatial.r")
fortify.SpatialPolygons <- function(model, data, ...) {rbind_df(lapply(model@polygons, fortify))}

block_map_df<-fortify.SpatialPolygons(block_map)


# Show in all crimes on the map that happened on Friday, 12th of October
ggplot(block_map_df, aes(x = long, y = lat))+
  geom_path(aes(group = group))+
  geom_point(data = Friday, aes(x = X, y = Y, color = Crime), pch = 10, size = 5, show.legend = TRUE)+
  labs(title = "Crimes in San Francisco on Friday, October 12, 2012")



# In some Police Districts happen more crimes than in others
sf_crimes %>% group_by(PdDistrict) %>%
  summarise(Count = length(PdDistrict)) %>%
  ggplot(aes(x = reorder(PdDistrict, Count), Count))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Police District", title = "Distribution of crimes by Police District")


# For being able to count the resolution as simply resolved yes or no we modify the 13 levels of Resolution
unique(sf_crimes$Resolution)

sf_crimes <- sf_crimes %>%
  mutate(Resolved = fct_recode(Resolution,
                            "N" = "NONE",
                            "Y" = "ARREST, BOOKED",
                            "Y" = "JUVENILE BOOKED",
                            "N" = "UNFOUNDED",
                            "Y" = "JUVENILE ADMONISHED",
                            "N" = "DISTRICT ATTORNEY REFUSES TO PROSECUTE",
                            "Y" = "ARREST, CITED",
                            "Y" = "JUVENILE CITED",
                            "Y" = "EXCEPTIONAL CLEARANCE",
                            "Y" = "PSYCHOPATHIC CASE",
                            "N" = "NOT PROSECUTED",
                            "N" = "COMPLAINANT REFUSES TO PROSECUTE",
                            "Y" = "CLEARED-CONTACT JUVENILE FOR MORE INFO"))


# Group data to count the crimes
grouped_crimes <- group_by(sf_crimes, Descript, Resolved, PdDistrict, 
                           Date, X ,Y, Time, DayOfWeek, Month, Crime) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Create a data set with a combination of Date and Police Districts
# The number of rows will be the product of Date and Districts
sf_crimes %>%
  summarize(count_PdDistrict = n_distinct(PdDistrict),
            count_date = n_distinct(Date))

# Unique Police Districts 
PdDis <- sf_crimes %>%
  distinct(PdDistrict) %>%
  arrange(PdDistrict)
PdDis

# Unique dates
dates <- sf_crimes %>%
  distinct(Date) %>%
  arrange(Date)

# Create a data frame with every possible combination of Districts and Dates
Dist_Date <- expand_grid(PdDis, dates) %>%
  arrange(PdDistrict)
head(Dist_Date)

# Use the grouped crime data (with Counts) to group by District and Date 
# to find the total number of crimes per day and District
crimes_Dist_Date <- grouped_crimes %>%
  group_by(PdDistrict, Date) %>%
  summarize(Count = sum(Count, na.rm = TRUE),
            Resolved = sum(as.numeric(Resolved)-1, na.rm = TRUE),
            Month = Month, DayOfWeek = DayOfWeek, Crime = Crime)
crimes_Dist_Date
# Put both data sets together to get the modeling data set
dataset <- Dist_Date %>%
  left_join(crimes_Dist_Date, keep=TRUE)


# Remove double columns
dataset <- dataset %>%
  mutate(Date = Date.y, PdDistrict = PdDistrict.y) %>%
  dplyr::select(-PdDistrict.x, -PdDistrict.y, -Date.x, -Date.y)


# 34 empty rows were produced
sum(is.na(dataset$DayOfWeek))
which(is.na(dataset$Month))
Show_empty_rows <- dataset[c(316, 1279, 2013, 4606, 5258, 5783, 6075, 6087, 6317, 6369, 6454, 6517, 
                 6605, 6671, 6692, 6702, 6761, 6767, 6781, 6951, 7000, 7001, 7046, 8638, 
                 9144, 9184, 9223, 9230, 9265, 9281, 9300, 9410, 9464, 9959),]
Show_empty_rows

#Remove these empty rows from the dataset
dataset <- dataset[-c(316, 1279, 2013, 4606, 5258, 5783, 6075, 6087, 6317, 6369, 6454, 6517, 
                      6605, 6671, 6692, 6702, 6761, 6767, 6781, 6951, 7000, 7001, 7046, 8638, 
                      9144, 9184, 9223, 9230, 9265, 9281, 9300, 9410, 9464, 9959),]

# The resolution rate of San Francisco police it at around 36%
(sum(dataset$Resolved)/sum(dataset$Count))*100



# We want to predict the Count of crimes
# Make a data partition for validation set (10%)
set.seed(7, sample.kind = "Rounding")

validation_index <- createDataPartition(dataset$Count, times = 1, p = 0.1, list = FALSE)
training_set <- dataset %>% slice(-validation_index)
validation_set <- dataset %>% slice(validation_index)

# Splitting a test set from the training_set
set.seed(7, sample.kind = "Rounding")

test_index <- createDataPartition(training_set$Count, times = 1, p = 0.2, list = FALSE)
test_set <- training_set %>% slice(test_index)
train_set <- training_set %>% slice(-test_index)


# 3. Methods and TRAINING SECTION (5 approaches)

# 3.1. First model: Just the average number of crimes happening in San Francisco per day
Count_avg <- mean(train_set$Count)


# 3.2. Second model: Vector bias which has an effect on the count of crimes
# Effect of police districts
Pd_effect <- train_set %>% group_by(PdDistrict) %>%
  summarize(Pd_effect = mean(Count - Count_avg))

# Effect of weekdays
Day_effect <- train_set %>% group_by(DayOfWeek) %>%
  summarize(Day_effect = mean(Count - Count_avg))

#Effect of crimes as some crimes happen more often than others
Crime_effect <- train_set %>% group_by(Crime) %>%
  summarize(Crime_effect = mean(Count - Count_avg))

# Month effect
Month_effect <- train_set %>% group_by(Month) %>%
  summarize(Month_effect = mean(Count - Count_avg))

# Predict the count of crimes regarding the bias effects on the test set
predictions <- test_set %>%
  left_join(Pd_effect, by = "PdDistrict") %>%
  left_join(Day_effect, by = "DayOfWeek") %>%
  left_join(Crime_effect, by = "Crime") %>%
  left_join(Month_effect, by = "Month") %>%
  mutate(predictions = Count_avg + Pd_effect + Day_effect + Crime_effect + Month_effect)
fit_bias <- predictions$predictions


# 3.3. Cross validation on 4 different machine learning models with 10 folds and 3 repeats as configuration

# prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# GLM
set.seed(7, sample.kind = "Rounding")
fit_glm <- train(Count ~ ., method = "glm", data = train_set, trControl = control)

# CART
set.seed(7, sample.kind = "Rounding")
fit_cart <- train(Count ~ ., method = "rpart", data = train_set, trControl = control)

# SVM (takes several minutes to run, 4.2 MB)
set.seed(7, sample.kind = "Rounding")
fit_svm <- train(Count ~ ., method = "svmRadial", data = train_set, trControl = control)

# kNN
set.seed(7, sample.kind = "Rounding")
fit_knn <- train(Count ~ ., method = "knn", data = train_set, trControl = control)

# collect resamples
results <- resamples(list(GLM = fit_glm, 
                          CART = fit_cart, SVM = fit_svm, 
                          KNN = fit_knn))
results

# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)



# 3.4. An ensemble model 
models <- c("glm", "rpart", "svmRadial", "knn")

set.seed(1, sample.kind = "Rounding")
fit_ensemble <- lapply(models, function(model){ 
  print(model)
  train(Count ~ ., method = model, data = train_set)
}) 
names(fit_ensemble) <- models


# 3.5. Random Forest with tuning values of 80 trees and an mtry of 100
set.seed(1, sample.kind = "Rounding")
# This codes takes about 10 minutes
fit_rf <- train(Count ~ .,
                  method = "rf",
                  data = train_set,
                  ntree = 80,
                  tuneGrid = expand.grid(.mtry = 100))


# 4. RESULTS

# Making predictions with the trained models

# Average
rmse_avg <- RMSE(Count_avg, test_set$Count)

# Bias
rmse_bias <- RMSE(test_set$Count, predictions$predictions)

# GLM
pred_glm <- predict(fit_glm, test_set)
rmse_glm <- RMSE(test_set$Count, pred_glm)

# CART
pred_cart <- predict(fit_cart, test_set)
rmse_cart <- RMSE(test_set$Count, pred_cart)

# SVM
pred_svm <- predict(fit_svm, test_set)
rmse_svm <- RMSE(test_set$Count, pred_svm)

# KNN
pred_knn <- predict(fit_knn, test_set)
rmse_knn <- RMSE(test_set$Count, pred_knn)

# Ensemble
pred_ensemble <- sapply(fit_ensemble, function(object) 
  predict(object, newdata = test_set))
rmse_ensemble <- RMSE(test_set$Count, pred_ensemble)

# RF
pred_rf <- predict(fit_rf, test_set)
rmse_rf <- RMSE(test_set$Count, pred_rf)



# result table for RMSEs
RMSE_results <- data_frame(method = c("Average", "Bias", "Regression trees", "Linear regression", 
                                      "Svm", "K-Nearest-Neighbors", "Ensemble", "Random Forest"), 
                           RMSE = c(rmse_avg, rmse_bias, rmse_cart, rmse_glm, rmse_svm, rmse_knn,
                                    rmse_ensemble, rmse_rf))
RMSE_results


# 5. CONCLUSION

# Test the best model on the validation set
Validation_pred <- predict(fit_rf, validation_set)
Final_result <- RMSE(validation_set$Count, Validation_pred)
Final_result

# See the maximum count of crimes per day and district
max <- max(validation_set$Count)
max
