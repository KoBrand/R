library(Hmisc)
library(ggplot2)
library(reshape2)
##################################################################################
# Load relevant Data
##################################################################################

#clear workspace
rm(list = ls())

# Set working directory to csv file location
folder_location <- "/home/koni/Desktop/HiWi/R/Janto"

# load .csv file
file <- "data.csv"

file_to_use <- paste(folder_location,file,sep="/")

# read csv file into workspace
mydata <- read.csv2(file_to_use, header=TRUE, sep=",", dec=".")

# Only take data of interst
# mydata2 <- mydata[,c("QD", "Ppl", "Task")]
# remove missing values
# mydata <- na.omit(mydata2) 


# import names
# objects of the database can be accessed by simply using their names
attach(mydata)
##################################################################################
# Some basic stuff
##################################################################################

# Check names
#names(mydata)

# box plot to visualize the data
#boxplot(QD ~ Task, xlab="Task", ylab="Question", col="bisque")

# See
#View(mydata)

##################################################################################
# Error bar plot with 5% confidence intervall for small samples
##################################################################################
# calculate the mean
meanOfValues <-aggregate(QD~Task, FUN=mean, data=mydata)
# standard deviation
standardDeviationOfValues <-aggregate(QD~Task, FUN=sd, data=mydata)
n <- length(QD)  # num of samples
degeeOfFreedom <-length(meanOfValues[,1])-1  #degrees of freedomn


# errorbar calculation
# 95% abweichung, confidense interval for big samples:
#error <- qnorm(0.975)*d[,-1]/sqrt(n) 
# 95% abweichung, confidence interval for small samples
error <-qt(0.975,degeeOfFreedom)*standardDeviationOfValues[,-1]/sqrt(n) 

errbar(meanOfValues[,-2],meanOfValues[,-1], meanOfValues[,-1]+error, meanOfValues[,-1]-error,
       ylim=range(0:5), xlab='Task', ylab = 'Value', xaxt='n')
# ajust axis
axis(side =1, at =1:4)

##################################################################################
# multiple error bar Plott
##################################################################################
# Get the data you need
dataPpl0 <- mydata[! mydata$Ppl %in% c(5,15,10),]
dataPpl5 <- mydata[! mydata$Ppl %in% c(0,15,10),]
dataPpl10 <- mydata[! mydata$Ppl %in% c(5,15,0),]
dataPpl15 <- mydata[! mydata$Ppl %in% c(5,0,10),]

##################################################################################
# pairwise test
##################################################################################


##################################################################################
# 1 Anova  standard R
##################################################################################

# aov (dependent~ independent)
model <- aov(QD~factor(Task)*factor(Ppl), mydata)
summary(model)

##################################################################################
# 2 Anova like SPSS
##################################################################################
library(car)
library(stats)

# Set the variables to factors
detach(mydata) # to change some details
mydata$Task <- as.factor(mydata$Task)
mydata$Ppl <- as.factor(mydata$Ppl)
attach(mydata)

#look what factors are
#levels(fTask)
#levels(fPpl)
#table(fTask)
#table(fPpl)


linear_model <- lm(QD~Task*Ppl)
# summary (linear_model)

# Change options to SPSS standard
options(contrasts = c("contr.helmert", "contr.poly"))

# Anova typ 3 of the Car paket
Anova(linear_model, type=3)

detach(mydata)