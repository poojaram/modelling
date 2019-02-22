library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)


# Reads in the data files
dengue_features_train <- read.csv("data/dengue_features_train.csv", stringsAsFactors = FALSE)

dengue_features_test <- read.csv("data/dengue_features_test.csv", stringsAsFactors = FALSE)

dengue_labels_train <- read.csv("data/dengue_labels_train.csv", stringsAsFactors = FALSE)

#na_count <- data.frame(colSums(is.na(dengue_features_train)))


# Setting all NA values to the mean
for(i in 1:ncol(dengue_features_train)){
  dengue_features_train[is.na(dengue_features_train[,i]), i] <- mean(dengue_features_train[,i], na.rm = TRUE)
}

for(i in 1:ncol(dengue_labels_train)){
  dengue_labels_train[is.na(dengue_labels_train[,i]), i] <- mean(dengue_labels_train[,i], na.rm = TRUE)
}


# Filter of data by city  
sj_labels_train <- filter(dengue_labels_train, city == 'sj')
sj_features_train <- filter(dengue_features_train, city == 'sj')

iq_labels_train <- filter(dengue_labels_train, city == 'iq')
iq_features_train <- filter(dengue_features_train, city == 'iq')

# Joining the labels and features data frames
# Mutating coumns for 4 weeks and 8 weeks of lag
sj_joined <- left_join(sj_labels_train, sj_features_train, by = c("city", "year", "weekofyear")) %>%
  mutate(four_lag_cases = lag(total_cases, 4), eight_lag_cases = lag(total_cases, 8))
for(i in 1:ncol(sj_joined)){ # replacing NA with mean values
  sj_joined[is.na(sj_joined[,i]), i] <- mean(sj_joined[,i], na.rm = TRUE)
}

iq_joined <- left_join(iq_labels_train, iq_features_train, by = c("city", "year", "weekofyear")) %>%
  mutate(four_lag_cases = lag(total_cases, 4), eight_lag_cases = lag(total_cases, 8))
for(i in 1:ncol(iq_joined)){
  iq_joined[is.na(iq_joined[,i]), i] <- mean(iq_joined[,i], na.rm = TRUE) 
}


############################### HUMIDITY  HUMIDITY HUMIDITY ################################################

sj_humidity <- select(sj_joined, total_cases, four_lag_cases,reanalysis_relative_humidity_percent)

sj_humidity_plots <-  ggplot() +
  geom_smooth(data = sj_humidity, aes(x = reanalysis_relative_humidity_percent, y = total_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=sj_humidity, aes(x = reanalysis_relative_humidity_percent, y = four_lag_cases), fill="red",
              colour="red", size=1)


#########################################################################################################

#vegitation mean mutated
sj_vegitation_mean <-  mutate(sj_joined, ndvi_mean = ((ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw) / 4))


#########################################################################################################
#########################################################################################################
# Does Diurnal temperature range in relation with average temperature effect the number of cases
sj_temp_range_cases_hightemp <- select(sj_joined, total_cases, station_avg_temp_c, station_diur_temp_rng_c, four_lag_cases, eight_lag_cases) %>%
  filter(station_avg_temp_c >= 26)
sj_temp_range_cases_lowtemp <- select(sj_joined, total_cases, station_avg_temp_c, station_diur_temp_rng_c, four_lag_cases, eight_lag_cases) %>%
  filter(station_avg_temp_c < 26)

# Plot for how high average temperatures and a high diurnal temperature range lead to decreased DENV transmission
sj_temp_range_cases_plots <-  ggplot() +
  geom_smooth(data = sj_temp_range_cases_hightemp, aes(x = station_diur_temp_rng_c, y = total_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=sj_temp_range_cases_lowtemp, aes(x = station_diur_temp_rng_c, y = total_cases), fill="red",
              colour="red", size=1)

sj_temp_range_cases_plots_fourlag <-    ggplot() +
  geom_smooth(data = sj_temp_range_cases_hightemp, aes(x = station_diur_temp_rng_c, y = four_lag_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=sj_temp_range_cases_lowtemp, aes(x = station_diur_temp_rng_c, y = four_lag_cases), fill="red",
              colour="red", size=1)


# We use a lag of four weeks because there is typically a lag of weeks to months 
# between changes in weather and associated dengue incidence.
# We can see in the second plot that with highter temperatures and a high diurnal temperature range leads
# to decreased DENV transmission(BLUE), where as with lower average temperature a high diurnal temperature range
# leads to increaded DENV transmission(RED)
plots <- grid.arrange(sj_temp_range_cases_plots, sj_temp_range_cases_plots_fourlag, ncol=2)
#########################################################################################################
#########################################################################################################

# same as above for IQ
#########################################################################################################
#########################################################################################################
# Does Diurnal temperature range in relation with average temperature effect the number of cases
iq_temp_range_cases_hightemp <- select(iq_joined, total_cases, station_avg_temp_c, station_diur_temp_rng_c, four_lag_cases, eight_lag_cases) %>%
  filter(station_avg_temp_c >= 26)
iq_temp_range_cases_lowtemp <- select(iq_joined, total_cases, station_avg_temp_c, station_diur_temp_rng_c, four_lag_cases, eight_lag_cases) %>%
  filter(station_avg_temp_c < 26)

# Plot for how high average temperatures and a high diurnal temperature range lead to decreased DENV transmission
iq_temp_range_cases_plots <-  ggplot() +
  geom_smooth(data = iq_temp_range_cases_hightemp, aes(x = station_diur_temp_rng_c, y = total_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=iq_temp_range_cases_lowtemp, aes(x = station_diur_temp_rng_c, y = total_cases), fill="red",
              colour="red", size=1)

iq_temp_range_cases_plots_fourlag <-    ggplot() +
  geom_smooth(data = iq_temp_range_cases_hightemp, aes(x = station_diur_temp_rng_c, y = four_lag_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=iq_temp_range_cases_lowtemp, aes(x = station_diur_temp_rng_c, y = four_lag_cases), fill="red",
              colour="red", size=1)


# We use a lag of four weeks because there is typically a lag of weeks to months 
# between changes in weather and associated dengue incidence.
# We can see in the second plot that with highter temperatures and a high diurnal temperature range leads
# to decreased DENV transmission(BLUE), where as with lower average temperature a high diurnal temperature range
# leads to increaded DENV transmission(RED)
plots <- grid.arrange(iq_temp_range_cases_plots, iq_temp_range_cases_plots_fourlag, ncol=2)
#########################################################################################################
#########################################################################################################

# Comparing the total cases of dengue in dry and wet seasons for SJ and IQ

sj_total_cases_dry <- filter(sj_labels_train, weekofyear %in% c(49:52, 1:13)) %>% # dry season
  summarise(total_dry_cases = sum(total_cases))
sj_total_cases_wet <- filter(sj_labels_train, weekofyear %in% 14:48) %>% # wet seson
  summarise(total_wet_cases = sum(total_cases))
sj_compare_seasons <- data.frame(sj_total_cases_dry, sj_total_cases_wet)

iq_total_cases_dry <- filter(iq_labels_train, weekofyear %in% 23:39) %>% # dry season
  summarise(total_dry_cases = sum(total_cases))
iq_total_cases_wet <- filter(iq_labels_train, weekofyear %in% c(40:52, 1:22)) %>% # wet season
  summarise(total_wet_cases = sum(total_cases))
iq_compare_seasons <- data.frame(iq_total_cases_dry, iq_total_cases_wet) # More cases in wet seasons


dry_to_wet <- data.frame(sj_compare_seasons, iq_compare_seasons)
dry <- data.frame(total_dry_cases = c(dry_to_wet[,"total_dry_cases"], dry_to_wet[,"total_dry_cases.1"]))
wet <- data.frame(total_wet_cases = c(dry_to_wet[,"total_wet_cases"], dry_to_wet[,"total_wet_cases.1"]))
dry_to_wet <- data.frame(city = c("SJ", "IQ"), dry, wet)
dry_to_wet <- melt(dry_to_wet)  #the function melt reshapes it from wide to long

#graph of how casualties change from dry to wet seasons
ggplot(dry_to_wet, aes(variable, value, group=factor(city))) + geom_line(aes(color=factor(city)))

  


## DISTURBUTIONS
sj_dist <- group_by(sj_labels_train, year) %>%
  summarize(mean_of_cases = mean(total_cases))
sj_dist_plot <- ggplot(sj_dist) +
  geom_bar(mapping = aes(x = year, y = mean_of_cases), stat = "identity") +
  geom_smooth(data=sj_dist, aes(x = year, y = mean_of_cases), fill = NA,
              colour="red", size=1)

iq_dist <- group_by(iq_labels_train, year) %>%
  summarize(mean_of_cases = mean(total_cases))
iq_dist_plot <- ggplot(iq_dist) +
  geom_bar(mapping = aes(x = year, y = mean_of_cases), stat = "identity") 


  
  
# Plot for cases over weeks for different years in SJ
sj_cases_over_weeks <- ggplot(data = sj_joined) +
  geom_line(mapping = aes(x = weekofyear, y = four_lag_cases)) +
  facet_wrap(~year)

# Plot for cases over weeks for different years in IQ
iq_cases_over_weeks <- ggplot(data = iq_joined) +
  geom_line(mapping = aes(x = weekofyear, y = four_lag_cases)) +
  facet_wrap(~year)
