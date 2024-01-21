#Apify for #tiktok
#Load data
getwd()
setwd(dir = "/Users/katgood/desktop")
library(readxl)
tt1x <- read_excel("22hash.xlsx")
str(tt1x)

#Change time to readable date
tt1x$createTimeISO <- as.Date(tt1x$createTimeISO)

#What dates is this data from?
range(tt1x$createTimeISO)
#Let's delete anything pre 2021 or post 2022
install.packages("dplyr")
library("dplyr")
install.packages("data.table")
library("data.table")
arrange(tt1x,tt1x$createTimeISO)
tt1x<-tt1x[- grep("2016", tt1x$createTimeISO),]
tt1x<-tt1x[- grep("2017", tt1x$createTimeISO),]
tt1x<-tt1x[- grep("2018", tt1x$createTimeISO),]
tt1x<-tt1x[- grep("2019", tt1x$createTimeISO),]
tt1x<-tt1x[- grep("2020", tt1x$createTimeISO),]
tt1x<-tt1x[- grep("2023", tt1x$createTimeISO),]
#Let's delete any other dates that are unneccessary
tt1x<-tt1x[- grep("2021", tt1x$createTimeISO),] #for top 2022
tt1x<-tt1x[- grep("2021", tt1x$createTimeISO),] #for top 2021
#check that we have the correct data only
range(tt1x$createTimeISO)

#Visualize playCount
hist(tt1x$playCount, main="# Playcount 2021-2022", xlab="play count", breaks=10)
summary(tt1x)
boxplot(tt1x$playCount)

#Discretize predictors
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
#Change verified to logical
tt1x$`authorMeta/verified`<-as.logical(tt1x$`authorMeta/verified`)
#Change all predictors to factors
tt1x$`authorMeta/fans`<-discretize(tt1x$`authorMeta/fans`, method = "interval", breaks=10)
tt1x$`authorMeta/heart`<-discretize(tt1x$`authorMeta/heart`, method = "interval", breaks=10)
tt1x$`authorMeta/id`<-as.factor(tt1x$`authorMeta/id`)
tt1x$`authorMeta/name`<-as.factor(tt1x$`authorMeta/name`)
tt1x$`authorMeta/video`<-discretize(tt1x$`authorMeta/video`, method = "interval", breaks=10)
tt1x$commentCount<-discretize(tt1x$commentCount, method = "interval", breaks=10)
tt1x$createTimeISO<-as.factor(tt1x$createTimeISO)
tt1x$diggCount<-discretize(tt1x$diggCount, method = "interval", breaks=10)
tt1x$`hashtags/0/name`<-as.factor(tt1x$`hashtags/0/name`)
tt1x$`hashtags/1/name`<-as.factor(tt1x$`hashtags/1/name`)
tt1x$`hashtags/2/name`<-as.factor(tt1x$`hashtags/2/name`)
tt1x$`hashtags/3/name`<-as.factor(tt1x$`hashtags/3/name`)
tt1x$`hashtags/4/name`<-as.factor(tt1x$`hashtags/4/name`)
tt1x$`hashtags/5/name`<-as.factor(tt1x$`hashtags/5/name`)
tt1x$`hashtags/6/name`<-as.factor(tt1x$`hashtags/6/name`)
tt1x$`hashtags/7/name`<-as.factor(tt1x$`hashtags/7/name`)
tt1x$`hashtags/8/name`<-as.factor(tt1x$`hashtags/8/name`)
tt1x$`hashtags/9/name`<-as.factor(tt1x$`hashtags/9/name`)
tt1x$`hashtags/10/name`<-as.factor(tt1x$`hashtags/10/name`)
tt1x$shareCount<-discretize(tt1x$shareCount, method = "interval", breaks=10)
tt1x$`videoMeta/duration`<-discretize(tt1x$`videoMeta/duration`, method = "interval", breaks=10)
#Find cutoff for viral/not viral playCount outcome
quantile(tt1x$playCount)
#discretize playCount outcome using breaks=c(min,3rd quartile, max)
tt1x$playCount <- cut(tt1x$playCount, breaks=c(0,547875,71500000), labels=c('not_viral', 'viral'))
#check to make sure everything is logical or factor format
str(tt1x)

#Association Rule Mining
rules <- apriori(data=tt1x,parameter = list(minlen=2,supp=0.2,conf=0.6),appearance = list(rhs = c("playCount=not_viral","playCount=viral"),default="lhs"), control = list(verbose=F))
summary(rules)
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)

#Decision Tree
#Not functional on my computer due to issues locating Java 
install.packages("rJava")
library("rJava")
install.packages("RWeka")
library("RWeka")
install.packages("party")
library("party")
tree<-J48(playCount~., data=tt1x)
