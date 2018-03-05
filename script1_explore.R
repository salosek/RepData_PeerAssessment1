## Reproducible Research Project 1

## Scott Loseke

## read in the data

setwd("C:/rProjects/repResearch/RepData_PeerAssessment1")

dat1 <- read.csv("activity.csv")
dat1$date <- as.Date(dat1$date, "%Y-%m-%d")
str(dat1)

