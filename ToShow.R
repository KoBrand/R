##################################################################################
# Load relevant packages
##################################################################################
# Install packages first
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("reshape2")

# If it depends on other packages uese
#install.packages("car", dependencies = TRUE)

# load packages if aleady installed
library(Hmisc)
library(ggplot2)
library(reshape2)

##################################################################################
# Load relevant data
##################################################################################

# clear workspace.
# R does not do this for different projeckts. 
# If you have the same variable in different projects it might cause errors
rm(list = ls())

# Set working directory to csv file location
folder_location <- "/home/koni/Desktop/HiWi/R/Janto"
file <- "data.csv"
file_to_use <- paste(folder_location, file, sep="/")  # combines two strings

# read csv file into workspace
mydata <- read.csv2(file_to_use, header=TRUE, sep=",", dec=".")  
# you can also use 'read.csv' instead of 'read.csv2', 
# but the file handling is slightly diffrent. See '?read.table' for more information.

# Only take data of interst
#mydata2 <- mydata[,c("QD", "Ppl", "Task")]
# remove missing values
#mydata <- na.omit(mydata2) 


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

# Some important stuff:
# Help can be optained by applying a '?' before the function and entering it in the terminal.

# Make sure to understand what the 'c' funtion is (combine function)
# http://www.r-tutor.com/r-introduction/vector/combining-vectors

# There are different wayt to call data:
#mydata$QA1 
#mydata[,8] or mydata[8] (row vector or column vector)
#if you apply attach(mydata) in beforehand you can right away use QA1
#sometimes you can also find QA1, data=mydata in some functions

##################################################################################
# 1 Error bar plot with 5% confidence intervall for small samples
##################################################################################
# mean
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
# 2 Error bar plot with 5% confidence intervall for small samples
##################################################################################
# all stepps in one
myDataErrorBar <- aggregate(mydata$QA1, 
                    by = list(Task = mydata$Task),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(QA1)))

# not sure why but to acces data from myDataErrorBar we need to call this:
myData2 <- do.call(data.frame, myDataErrorBar)
# set column names of myData2
colnames(myData2) <- c("Task", "mean", "sd", "n")
# a nother way to add colums to a table:
myData2$DegreeOfFreedomn <- length(myData2$mean)-1
myData2$Error <- qt(0.975, myData2$DegreeOfFreedomn)*myData2$sd/sqrt(myData2$n)

# plot
# Set window margins
par(mar = c(5, 6, 4, 5) + 0.1)
# If you change them once you will change them for all future plots. 
# So make shure to change them back with 
#par(mar = c(4, 4, 4, 4))

# Set maximum of Y axis
plotTop <- 6

# calculate bar plot 
barCenters <- barplot(height = myData2$mean,
                      names.arg = myData2$Task,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "Name of plot",
                      ylab = "y achsis name",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = myData2$Task, xpd = TRUE)

segments(barCenters, myData2$mean - myData2$Error, barCenters,
         myData2$mean + myData2$Error, lwd = 1.5)

# Draw 5% confidence arrows with 90° angle
arrows(barCenters, myData2$mean - myData2$Error, barCenters,
       myData2$mean + myData2$Error, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

##################################################################################
# 1 multiple error bar Plott
##################################################################################
# Get the data you need
dataPpl0 <- mydata[! mydata$Ppl %in% c(5,15,10),]

par(mar = c(4, 4, 4, 4))  # set marginals back to standard
meanOfValues <-aggregate(QD~Task, FUN=mean, data=dataPpl0)
standardDeviationOfValues <-aggregate(QD~Task, FUN=sd, data=dataPpl0)
n <- length(dataPpl0$QD)  # num of samples
degeeOfFreedom <-length(meanOfValues[,1])-1  #degrees of freedomn
error <-qt(0.975,degeeOfFreedom)*standardDeviationOfValues[,-1]/sqrt(n) 
errbar(meanOfValues[,-2],meanOfValues[,-1], meanOfValues[,-1]+error, meanOfValues[,-1]-error,
       ylim=range(0:6), xlab='Task', ylab = 'Value', xaxt='n')
# ajust axis
axis(side =1, at =1:4)

########################
# combine plots
par(new=T)   # is the same as 'Hold an' from matlab
########################
dataPpl5 <- mydata[! mydata$Ppl %in% c(0,15,10),]

meanOfValues <-aggregate(QD~Task, FUN=mean, data=dataPpl5)
standardDeviationOfValues <-aggregate(QD~Task, FUN=sd, data=dataPpl5)
n <- length(dataPpl0$QD)  # num of samples
degeeOfFreedom <-length(meanOfValues[,1])-1  #degrees of freedomn
error <-qt(0.975,degeeOfFreedom)*standardDeviationOfValues[,-1]/sqrt(n) 
errbar(meanOfValues[,-2],meanOfValues[,-1], meanOfValues[,-1]+error, meanOfValues[,-1]-error,
       ylim=range(0:6), xlab='Task', ylab = 'Value', xaxt='n')
# ajust axis
axis(side =1, at =1:4)


par(new=F)  # 'holf off'

##################################################################################
# 2 multiple error bar Plott
##################################################################################
# Gather data you need
myDataErrorBar <- aggregate(mydata$QA1, 
                            by = list(Task = mydata$Task, Ppl = mydata$Ppl),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
myData2 <- do.call(data.frame, myDataErrorBar)
colnames(myData2) <- c("Task", "Ppl", "mean", "sd", "n")
myData2$DegreeOfFreedomn <- length(myData2$mean)-1
myData2$Error <- qt(0.975, myData2$DegreeOfFreedomn)*myData2$sd/sqrt(myData2$n)


# merge relevant data into a matrix wich can easily be plotted
tapply(myData2$mean, list(myData2$Task, myData2$Ppl),
       function(x) c(x = x))

tabbedMeans <- tapply(myData2$mean, list(myData2$Task,
                                         myData2$Ppl),
                      function(x) c(x = x))

tabbedSE <- tapply(myData2$Error, list(myData2$Task,
                                       myData2$Ppl),
                   function(x) c(x = x))

########################
# multiple plot Ppl oder Task
par(mar = c(5, 6, 4, 5) + 0.1)
plotTop <- 6

barCenters <- barplot(height = tabbedMeans,
                      beside = TRUE, las = 1,
                      ylim = c(0, plotTop),
                      cex.names = 0.75,
                      main = "Plot name",
                      ylab = "Y - Axis",
                      xlab = "No. Gears",
                      border = "black", axes = TRUE,
                      legend.text = FALSE,
                      args.legend = list(title = "Ppl", 
                                         x = "topright",
                                         cex = .7))

segments(barCenters, tabbedMeans - tabbedSE, barCenters,
         tabbedMeans + tabbedSE, lwd = 1.5)

arrows(barCenters, tabbedMeans - tabbedSE, barCenters,
       tabbedMeans + tabbedSE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

########################
#multiple plot Task oder Ppl

tapply(myData2$mean, list(myData2$Ppl, myData2$Task),
       function(x) c(x = x))

tabbedMeans <- tapply(myData2$mean, list(myData2$Ppl, myData2$Task),
                      function(x) c(x = x))

tabbedSE <- tapply(myData2$Error, list(myData2$Ppl, myData2$Task),
                   function(x) c(x = x))

barCenters <- barplot(height = tabbedMeans,
                      beside = TRUE, las = 1,
                      ylim = c(0, plotTop),
                      cex.names = 0.75,
                      main = "Plot name",
                      ylab = "Y - Axis",
                      xlab = "No. Gears",
                      border = "black", axes = TRUE,
                      legend.text = FALSE,
                      args.legend = list(title = "Ppl", 
                                         x = "topright",
                                         cex = .7))

segments(barCenters, tabbedMeans - tabbedSE, barCenters,
         tabbedMeans + tabbedSE, lwd = 1.5)

arrows(barCenters, tabbedMeans - tabbedSE, barCenters,
       tabbedMeans + tabbedSE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)



##################################################################################
# pairwise test
##################################################################################

model <- aov(QD~factor(Task)*factor(Ppl), data=mydata)
summary(model)

TukeyHSD(model)

##################################################################################
# Anova Conditions  
##################################################################################
# Normality of residuals: Shapiro-Wilk on the residuals of the models (applied for a small number of samples): p-value > 0.05, so normality assumption is valid
fit <- lm(QA1~factor(Task)+factor(Ppl), data=mydata)
summary(fit)
# Homogeneity: the variance in each experimental condition need to be fairly similar
homogenity <- bartlett.test(QA1~Task, data=mydata)
homogenity2 <- bartlett.test(QA1~factor(Ppl), data=mydata)
summary(homogenity)
summary(homogenity2)
# Homogeneity of Variance Plot
library(HH)
hov(QA1~factor(Ppl), data=mydata)
hovPlot(QA1~factor(Ppl),data=mydata) 
# Independence of observations 

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
#summary(linear_model)

# Change options to SPSS standard
options(contrasts = c("contr.helmert", "contr.poly"))

# Anova typ 3 of the Car paket
Anova(linear_model, type=3)

# Change options Back to standard
# check options:
#options("contrast")
options(contrasts = c("contr.treatment", "contr.poly"))

detach(mydata) # Detach ddata at the end because R does not do this on its own