#load libraries
library(tidyverse)
library(DT)
library(ggplot2)
library(knitr)
library(plotly)
library(matrixStats)
library(shiny)
library(stats)
library(randomForest)
library(caret)
library(shinycssloaders)

#set working directory
#setwd("C:/Users/Owner/Desktop/ST558-Project3")
#read in data from csv files
readData1<-read_csv('Washington_State_HDMA-2016-Part1.csv')
readData2<-read_csv('Washington_State_HDMA-2016-Part2.csv')
readData3<-read_csv('Washington_State_HDMA-2016-Part3.csv')
readData<-dplyr::bind_rows(readData1,readData2,readData3)
#read in csv that contains field definitions 
fieldDef<-read_csv('field definitions.csv')

#add variable sexRevised to reclass Info not provided to N/A
#add variable raceRevised to reclass Info not provided to N/A
#add variable application_result to reclass action taken to 1 or 0
modelData<-readData%>%mutate(sexRevised = ifelse(applicant_sex_name=="Information not provided by applicant in mail, Internet, or telephone application","Not applicable",as.character(applicant_sex_name)))%>%mutate(raceRevised = ifelse(applicant_race_name_1=="Information not provided by applicant in mail, Internet, or telephone application","Not applicable",as.character(applicant_race_name_1)))%>%mutate(application_result=ifelse(action_taken_name=="Loan originated",1,0))

#convert application_result to a factor 
modelData$application_result <- as.factor(modelData$application_result)

#data used for box plot which excludes race N/A
boxData<-modelData%>%filter(raceRevised!="Not applicable")

#data used for clustering. Select loan amount and income and drops records with NAs. A sample is taken due to computer limitations when running the clustering analysis.
clusData<-modelData%>%select(loan_amount_000s,applicant_income_000s)%>%drop_na()%>%sample_n(100, replace = FALSE)
#scaling the clustering data
clusData<-scale(clusData)
#computing distanct between points 
d<-dist(clusData, method = "euclidean")

#function to get six number summaries for income and loan amount 
getNumericSummaries <- function(sex, action) {
  dataSubset<-modelData%>%filter(modelData$sexRevised==sex & modelData$action_taken_name==action)%>%select(applicant_income_000s,loan_amount_000s)%>%drop_na()
  Min.<-sapply(dataSubset, min)
  Qu.1st<-colQuantiles(as.matrix(dataSubset), prob =.25)
  Mean<-sapply(dataSubset, mean)
  Median<-sapply(dataSubset, median)
  Qu.3rd<-colQuantiles(as.matrix(dataSubset), prob =.75)
  Max.<-sapply(dataSubset, max)
  #combine results
  rbind(Min.,"1st Qu." = Qu.1st,Mean,Median,"3rd Qu." = Qu.3rd,Max.)
}

