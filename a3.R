library(ggplot2)
library(dplyr)

# Reads in the data files
dengue_features_train <- read.csv("data/dengue_features_train.csv", stringsAsFactors = FALSE)

dengue_features_test <- read.csv("data/dengue_features_test.csv", stringsAsFactors = FALSE)

dengue_labels_train <- read.csv("data/dengue_labels_train.csv", stringsAsFactors = FALSE)


# Filter of data by city  
sj_labels_train <- filter(dengue_labels_train, city == 'sj')
sj_features_train <- filter(dengue_features_train, city == 'sj')

iq__labels_train <- filter(dengue_labels_train, city == 'iq')
iq_features_train <- filter(dengue_features_train, city == 'iq')

# Joining the labels and features data frames

sj_joines