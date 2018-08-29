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
  stat_smooth(method="auto") +
  stat_bin_2d() +
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

# Requests Analyses

# Shape the dataset
myAnalysisSJ <- select(source, pop, income, age, Abandoned.Vehicle, Streetlight.Outage, General.Request, Illegal.Dumping, Graffiti, Pothole)
myAnalysisSJ <- myAnalysisSJ[!is.na(myAnalysisSJ$income),]
myAnalysisSJ[,'income'] <- round(myAnalysisSJ[,'income'], digits = -2)

# Basic Trend Lines over Income
ggplot(myAnalysisSJ, aes(x=income)) +
  stat_smooth(aes(y=Abandoned.Vehicle, colour="Abandoned Vehicle")) +
  stat_smooth(aes(y=Streetlight.Outage, colour="Streetlight Outage")) +
  stat_smooth(aes(y=General.Request, colour="General")) +
  stat_smooth(aes(y=Illegal.Dumping, colour="Illegal Dumpings")) +
  stat_smooth(aes(y=Graffiti, colour="Graffiti")) +
  stat_smooth(aes(y=Pothole, colour="Potholes")) +
  scale_colour_manual(name=c("Abandoned Vehicle","Streetlight Outrage","General","Illegal Dumpings","Graffiti","Potholes"), values=c("red","blue","green","orange","yellow","pink")) +
  xlab("Income") +
  ylab("mySJ Requests") +
  ggtitle("Income across MySJ Requests")

# Basic Trend Lines over Pop
ggplot(myAnalysisSJ, aes(x=pop)) +
  stat_smooth(aes(y=Abandoned.Vehicle, colour="Abandoned Vehicle")) +
  stat_smooth(aes(y=Streetlight.Outage, colour="Streetlight Outage")) +
  stat_smooth(aes(y=General.Request, colour="General")) +
  stat_smooth(aes(y=Illegal.Dumping, colour="Illegal Dumpings")) +
  stat_smooth(aes(y=Graffiti, colour="Graffiti")) +
  stat_smooth(aes(y=Pothole, colour="Potholes")) +
  scale_colour_manual(name=c("Abandoned Vehicle","Streetlight Outrage","General","Illegal Dumpings","Graffiti","Potholes"), values=c("red","blue","green","orange","yellow","pink")) +
  xlab("Population per GeoID") +
  ylab("mySJ Requests") +
  ggtitle("Population across MySJ Requests")

# Basic Trend Lines over Age
ggplot(myAnalysisSJ, aes(x=age)) +
  stat_smooth(aes(y=Abandoned.Vehicle, colour="Abandoned Vehicle")) +
  stat_smooth(aes(y=Streetlight.Outage, colour="Streetlight Outage")) +
  stat_smooth(aes(y=General.Request, colour="General")) +
  stat_smooth(aes(y=Illegal.Dumping, colour="Illegal Dumpings")) +
  stat_smooth(aes(y=Graffiti, colour="Graffiti")) +
  stat_smooth(aes(y=Pothole, colour="Potholes")) +
  scale_colour_manual(name=c("Abandoned Vehicle","Streetlight Outrage","General","Illegal Dumpings","Graffiti","Potholes"), values=c("red","blue","green","orange","yellow","pink")) +
  xlab("Average Age") +
  ylab("mySJ Requests") +
  ggtitle("Average Age across MySJ Requests")
