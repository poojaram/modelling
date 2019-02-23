library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
library("ggpubr")


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


#Temperature plot 
temp_vs_totcases_sj <- select(sj_joined, total_cases, four_lag_cases, reanalysis_avg_temp_k)
temp_vs_totcases_sj_p <- ggplot() + geom_point(data=temp_vs_totcases_sj, aes(x = reanalysis_avg_temp_k, y = total_cases), fill=NA,
                                               colour="dark orange", size=1, alpha = 0.3) +
  geom_smooth(data=temp_vs_totcases_sj, aes(x = reanalysis_avg_temp_k, y = total_cases), fill=NA,
                                     colour="red", size=1) +
                      labs(
                        title = "San Juan - Total DNV cases vs Temp", # plot title
                        x = "Average air temperature in K", # x-axis label (with units!)
                        y = "Total count of dengue fever cases", # y-axis label (with units!)
                        color = NA
                      )


temp_vs_totcases_iq <- select(iq_joined, total_cases, four_lag_cases, reanalysis_avg_temp_k)
temp_vs_totcases_iq_p <- ggplot() + geom_point(data=temp_vs_totcases_iq, aes(x = reanalysis_avg_temp_k, y = total_cases), fill=NA,
                                               colour="dark green", size=1, alpha = 0.3) +
  geom_smooth(data=temp_vs_totcases_iq, aes(x = reanalysis_avg_temp_k, y = total_cases), fill=NA,
                                                colour="blue", size=1) +
  labs(
    title = "Iquitos - Total DNV cases vs Temp", # plot title
    x = "Average air temperature in K", # x-axis label (with units!)
    y = "Total count of dengue fever cases", # y-axis label (with units!)
    color = NA
  )

temp_vs_totcases_plots <- grid.arrange(temp_vs_totcases_sj_p, temp_vs_totcases_iq_p, ncol = 2)

###Precipitatioon graphs

sj_info <- select(sj_joined, year, weekofyear, total_cases, four_lag_cases, eight_lag_cases,reanalysis_relative_humidity_percent,
                      reanalysis_sat_precip_amt_mm)

iq_info <- select(iq_joined, year, weekofyear, total_cases, four_lag_cases, eight_lag_cases,reanalysis_relative_humidity_percent,
                  reanalysis_sat_precip_amt_mm)


sj_box_df <- select(sj_info, weekofyear, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm)
sj_box_df$weekofyear <- as.factor(sj_box_df$weekofyear)
  

sj_box_p <- ggplot(sj_box_df, aes(x=weekofyear, y=reanalysis_sat_precip_amt_mm)) + 
  geom_boxplot()   +
  labs(
    title = "San Juan: Total Precipitation by week of year", 
    x = "Week Of The Year", 
    y = "Total precipitation amount in mm",
    color = NA
  )
  
sj_prec_curve <- group_by(sj_info, weekofyear) %>%
  summarize(mean_of_prec = mean(reanalysis_sat_precip_amt_mm))


sj_prec_curve_p <- ggplot(sj_prec_curve) +
  geom_bar(mapping = aes(x = weekofyear, y = mean_of_prec), stat = "identity") 
  

iq_box_df <- select(iq_info, weekofyear, reanalysis_relative_humidity_percent, reanalysis_sat_precip_amt_mm)
iq_box_df$weekofyear <- as.factor(iq_box_df$weekofyear)


iq_box_p <- ggplot(iq_box_df, aes(x=weekofyear, y=reanalysis_sat_precip_amt_mm)) + 
  geom_boxplot()  +
  labs(
    title = "Iquitos: Total Precipitation by week of year", 
    x = "Week Of The Year", 
    y = "Total precipitation amount in mm",
    color = NA
  )


####
iq_prec_curve <- group_by(iq_info, weekofyear) %>%
  summarize(mean_of_prec = mean(reanalysis_sat_precip_amt_mm))

iq_prec_curve_p <- ggplot(iq_prec_curve) +
  geom_bar(mapping = aes(x = weekofyear, y = mean_of_prec), stat = "identity") 




sj_humidity_plots <-  ggplot() +
  geom_smooth(data = sj_info, aes(x = reanalysis_relative_humidity_percent, y = total_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=sj_info, aes(x = reanalysis_relative_humidity_percent, y = four_lag_cases), fill="red",
              colour="red", size=1)

sj_prec_plots <-  ggplot() +
  geom_smooth(data = sj_info, aes(x = reanalysis_sat_precip_amt_mm, y = total_cases), fill="blue",
              colour="darkblue", size=1) +
  geom_smooth(data=sj_info, aes(x = reanalysis_sat_precip_amt_mm, y = eight_lag_cases), fill="red",
              colour="red", size=1)

####Vegitation

#SJ vegitation over NDVI mean
sj_vegitation_mean <-  mutate(sj_joined, ndvi_mean = ((ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw) / 4)) %>%
  select(ndvi_mean, ndvi_mean, four_lag_cases, total_cases, reanalysis_sat_precip_amt_mm)

sj_vegitation_prec_plot <-  ggplot() + 
  geom_point(data=sj_vegitation_mean, aes(x = ndvi_mean, y = reanalysis_sat_precip_amt_mm), fill=NA,
                                                 colour="green4", size=1, alpha = 0.3) +
  labs(
    title = "San Juan - Precipitation by NDVI mean", 
    x = "Normalized difference vegetation index mean", 
    y = "Total precipitation in mm", 
    color = NA
  )

#IQ Vegitation over NDVI mean
iq_vegitation_mean <-  mutate(iq_joined, ndvi_mean = ((ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw) / 4)) %>%
  select(ndvi_mean, ndvi_mean, four_lag_cases, total_cases, reanalysis_sat_precip_amt_mm)

iq_vegitation_prec_plot <-  ggplot() + 
  geom_point(data=iq_vegitation_mean, aes(x = ndvi_mean, y = reanalysis_sat_precip_amt_mm), fill=NA,
             colour="green4", size=1, alpha = 0.3) +
  labs(
    title = "Iquitos - Precipitation by NDVI mean", 
    x = "Normalized difference vegetation index mean", 
    y = "Total precipitation in mm", 
    color = NA
  )

prec_veg_plots <- grid.arrange(sj_vegitation_prec_plot, iq_vegitation_prec_plot, ncol = 2)


# Does Diurnal temperature range in relation with average temperature effect the number of cases
sj_temp_range_cases_hightemp <- select(sj_joined, total_cases, reanalysis_avg_temp_k, reanalysis_tdtr_k, four_lag_cases, eight_lag_cases) %>%
  filter(reanalysis_avg_temp_k >= 299.15)
sj_temp_range_cases_lowtemp <- select(sj_joined, total_cases, reanalysis_avg_temp_k, reanalysis_tdtr_k, four_lag_cases, eight_lag_cases) %>%
  filter(reanalysis_avg_temp_k < 299.15)

# Plot for how high average temperatures and a high diurnal temperature range lead to decreased DENV transmission

sj_temp_range_cases_plots_fourlag <- ggplot() +
  geom_smooth(data = sj_temp_range_cases_hightemp, aes(x = reanalysis_tdtr_k, y = total_cases, colour="Average air temperature >= 299.15"),fill="red",
               size=1) +
  geom_smooth(data=sj_temp_range_cases_lowtemp, aes(x = reanalysis_tdtr_k, y = total_cases, colour="Average air temperature < 299.15"),fill="blue",
               size=1)  +
  scale_colour_manual(name="legend",breaks = c("Average air temperature >= 299.15", "Average air temperature < 299.15"), values=c("blue", "red")) +
  labs(
    title = "San Juan - Total DNV cases by Diurnal temp range", 
    x = "Diurnal temperature range in Kelvin", 
    y = "Total cases of DF", 
    color = NA
  )


# Does Diurnal temperature range in relation with average temperature effect the number of cases for IQ
iq_temp_range_cases_hightemp <- select(iq_joined, total_cases, reanalysis_avg_temp_k, reanalysis_tdtr_k, four_lag_cases, eight_lag_cases) %>%
  filter(reanalysis_avg_temp_k >= 299.15)
iq_temp_range_cases_lowtemp <- select(iq_joined, total_cases, reanalysis_avg_temp_k, reanalysis_tdtr_k, four_lag_cases, eight_lag_cases) %>%
  filter(reanalysis_avg_temp_k < 299.15)

# Plot for how high average temperatures and a high diurnal temperature range lead to decreased DENV transmission
iq_temp_range_cases_plots <-  ggplot() +
  geom_smooth(data = iq_temp_range_cases_hightemp, aes(x = reanalysis_tdtr_k, y = total_cases), fill="red",
              colour="red", size=1) +
  geom_smooth(data=iq_temp_range_cases_lowtemp, aes(x = reanalysis_tdtr_k, y = total_cases), fill="blue",
              colour="blue", size=1)

iq_temp_range_cases_plots_fourlag <-  ggplot() +
  geom_smooth(data = iq_temp_range_cases_hightemp, aes(x = reanalysis_tdtr_k, y = total_cases, colour="Average air temperature >= 299.15"), fill="red",
              size=1) +
  geom_smooth(data=iq_temp_range_cases_lowtemp, aes(x = reanalysis_tdtr_k, y = total_cases, colour="Average air temperature < 299.15"), fill="blue",
              size=1)  +
  scale_colour_manual(name="legend",breaks = c("Average air temperature >= 299.15", "Average air temperature < 299.15"), values=c("blue", "red")) +
  labs(
    title = "Iquitos - Total DNV cases by Diurnal temp range", 
    x = "Diurnal temperature range in Kelvin", 
    y = "Total cases of DF",
    color = NA
  )


# We use a lag of four weeks because there is typically a lag of weeks to months 
# between changes in weather and associated dengue incidence.
# We can see in the second plot that with highter temperatures and a high diurnal temperature range leads
# to decreased DENV transmission(BLUE), where as with lower average temperature a high diurnal temperature range
# leads to increaded DENV transmission(RED)
diur_plots <- grid.arrange(iq_temp_range_cases_plots_fourlag, sj_temp_range_cases_plots_fourlag, ncol=2)

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
dry_to_wet <- data.frame(city = c("San Juan", "Iquitos"), dry, wet)
dry_to_wet <- melt(dry_to_wet)  #the function melt reshapes it from wide to long

# graph of how casualties change from dry to wet seasons
dry_to_wet_plot <- ggplot(dry_to_wet, aes(variable, value, group=factor(city))) + geom_line(aes(color=factor(city))) +
  labs(
    title = "Change in DF cases from dry to wet seasons", 
    x = "Season", 
    y = "Total cases of DF",
    color = "City"
  )



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
#########
  
  
# Plot for cases over weeks for different years in SJ
sj_cases_over_weeks <- ggplot(data = sj_joined) +
  geom_line(mapping = aes(x = weekofyear, y = total_cases)) +
  facet_wrap(~year) +
  labs(
    title = "San Juan - cases of DF over the years", 
    x = "Week Of The Year", 
    y = "Total cases of DF",
    color = NA
  )


sj_table_elnino <- group_by(sj_info, year) %>%
  summarize(mean_of_prec = mean(reanalysis_sat_precip_amt_mm))
sj_table_elnino_p <- ggplot(sj_table_elnino) +
  geom_bar(mapping = aes(x = year, y = mean_of_prec), stat = "identity")

# Plot for cases over weeks for different years in IQ
iq_cases_over_weeks <- ggplot(data = iq_joined) +
  geom_line(mapping = aes(x = weekofyear, y = total_cases)) +
  facet_wrap(~year) +
  labs(
    title = "Iquitos - cases of DF over the years", 
    x = "Week Of The Year", 
    y = "Total cases of DF",
    color = NA
  )


# Prec plotfo low medium and  high temperatures
trial_sj <- select(sj_joined, total_cases, four_lag_cases, eight_lag_cases, reanalysis_avg_temp_k, reanalysis_sat_precip_amt_mm, city) %>%
  arrange(reanalysis_avg_temp_k)

trial_sj$Rad <- cut(trial_sj$reanalysis_avg_temp_k, 3, labels=c('Low', 'Medium', 'High')) 

trial_low <- filter(trial_sj, Rad == 'Low') %>% select(-reanalysis_avg_temp_k, -Rad)
low_plot_sj <-  ggplot(data = trial_low) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "yellow3", fill = "yellow3") +
  labs(
    title = "San Juan low temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  ) + theme(plot.title = element_text(size=14))


trial_med <- filter(trial_sj, Rad == 'Medium') %>% select(-reanalysis_avg_temp_k, -Rad)
med_plot_sj <-  ggplot(data = trial_med) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "orange", fill = "orange") +
  labs(
    title = "San Juan medium temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  ) + theme(plot.title = element_text(size=14))


trial_high <- filter(trial_sj, Rad == 'High') %>% select(-reanalysis_avg_temp_k, -Rad)
high_plot_sj <-  ggplot(data = trial_high) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "red", fill = "red") +
  labs(
    title = "San Juan high temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  ) + theme(plot.title = element_text(size=14))


high_low_med_p_sj <- grid.arrange(low_plot_sj, med_plot_sj, high_plot_sj, ncol=3)
#######
trial_iq <- select(iq_joined, total_cases, four_lag_cases, eight_lag_cases, reanalysis_avg_temp_k, reanalysis_sat_precip_amt_mm, city) %>%
  arrange(reanalysis_avg_temp_k)

trial_iq$Rad <- cut(trial_iq$reanalysis_avg_temp_k, 3, labels=c('Low', 'Medium', 'High')) 

trial_low <- filter(trial_iq, Rad == 'Low') %>% select(-reanalysis_avg_temp_k, -Rad)
low_plot_iq <-  ggplot(data = trial_low) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "yellow3", fill = "yellow3") +
  labs(
    title = "Iquitos low temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  )


trial_med <- filter(trial_iq, Rad == 'Medium') %>% select(-reanalysis_avg_temp_k, -Rad)
med_plot_iq <-  ggplot(data = trial_med) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "orange", fill = "orange") +
  labs(
    title = "Iquitos medium temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  )


trial_high <- filter(trial_iq, Rad == 'High') %>% select(-reanalysis_avg_temp_k, -Rad)
high_plot_iq <-  ggplot(data = trial_high) +
  geom_smooth(mapping = aes(x = reanalysis_sat_precip_amt_mm, y = four_lag_cases), colour = "red", fill = "red") +
  labs(
    title = "Iquitos high temperatues", 
    x = "Total precipitation amount in mm", 
    y = "Total cases of DF",
    color = NA
  )


high_low_med_p_iq <- grid.arrange(low_plot_iq, med_plot_iq, high_plot_iq, ncol=3)

