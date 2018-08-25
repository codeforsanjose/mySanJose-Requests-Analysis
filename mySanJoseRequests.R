# My San Jose Requests
# Code for San Jose
# Aakash Desai

# Install libraries
library("ggplot2")
library("dplyr")
library("reshape2")

# Get the file and read into a dataframe
source <- read.csv("MySanJose App Request data by Census block group.csv")

# Shape the dataset for general statistics
mySJ <- select(source, pop, income, age)
mySJ <- mySJ[!is.na(mySJ$income),]
mySJ[,'income'] <- round(mySJ[,'income'], digits = -2)

# Plot Income across MySJ Requests
ggplot(mySJ, aes(x=income)) +
  geom_histogram() +
  xlab("Income") +
  ylab("Count") +
  ggtitle("Income across MySJ Requests")

# Plot Age across MySJ Requests
ggplot(mySJ, aes(x=age)) +
  geom_histogram() +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Age across MySJ Requests")

# Plot Income across Population in MySJ Requests
ggplot(mySJ, aes(x=income, y=pop)) +
  geom_point() +
  xlab("Income") +
  ylab("Population") +
  ggtitle("Income across Population in MySJ Requests")

# Plot LM for Income across Age in MySJ Requests
ggplot(mySJ, aes(x=age, y=income)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlab("Average Age") +
  ylab("Income") +
  ggtitle("Age across Income in MySJ Requests")