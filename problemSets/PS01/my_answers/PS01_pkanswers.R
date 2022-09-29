#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects

rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
#setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")  already have directory from github


#####################
# Problem 1
#####################

#part 1)

library(tidyverse)  #loading in tidyverse for graphing


y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98) #vector containing the iQs of the sample 25 students#


#In this section I graph the iQs in a bar chart, its helpful for me to have a rough idea what the data looks like
initial_groups<- cut(y,breaks = 7)    # binning the IQs for bar chart, I chose 7 groups arbitrarily
as.data.frame(table(initial_group))   #https://stackoverflow.com/questions/32292542/binning-values-in-a-vector  

ggplot() +    #graphing the binned data
  geom_bar(mapping = aes(x = initial_groups))+
  ggtitle("iQs of the sample 25 students")+
  xlab("groups of student iQs") +
  ylab("Frequancy")
  

# calculating the mean, sum of errors, (sum of errors)**2, variance, standard deviation,  turns out all this is unneccisary since sd() function exists

y_bar = sum(y)/length(y)  #mean
y_bar
y_bar_2 = mean(y)

sum_errors <- NULL   #sum of errors
for(i in 1:length(y))
{
  sum_errors[i] <- y[i] - mean(y)
}
sum_errors

sum_error_sq <- sum_errors^2   #sum of errors squared
sum_error_sq

variance <- (sum(sum_error_sq))/(length(y)-1)  #variance
variance

st_dev <- sqrt(variance)   #standard deviation
sd(y)



# generating a normal distribution using the data in y[]
dist = dnorm(y, mean(y), sd(y))


#plotting the normal distribution for the sample iQs
ggplot() +
  geom_line(mapping = aes(y, dist)) +
  ggtitle("normal distribution of sample y") +
  xlab("iQ") +
  ylab("probability")


#calculating the confidence interval
conf_int <- 0.9  # assigning conf int value
margin <- qt((1-(1-conf_int)/2), df = length(y) -1)*sd(y)/sqrt(length(y)) #calculating the margin
#margin


#finding the upper and lower intervals
lower_int = mean(y) - margin
upper_int = mean(y) + margin



# getting a bell curve for the mean value since no matter the underlying curve the mean curve is always a normal distribution
samples =  100000   #number of samples taken
mean_curve <- c()  #initialising empty vector which will contain the mean values of random samples from the curve
for (u in 1:samples)
  {
  x <- rnorm(25, mean(y), sd(y)) #random samples of size 25 from the curve
  mean_curve[u] = mean(x)  #mean of the samples of 25

}

#plotting the mean values of the samples from x curve to compare our confidence intervals to
# first as a bar chart with frequency, then as a normal distribution using the data

mean_x_groups<- cut(mean_curve,breaks = 25)    #I chose 25 at random
x_dist = dnorm(mean_curve, mean(mean_curve), sd(mean_curve))

ggplot() +    #graphing bar chart
  geom_bar(mapping = aes(x = mean_x_groups))+
  ggtitle("bar chart with the frequancy of mean iQs gotten from random samples of the normal distribution of y")+
  xlab("mean iQ") +
  ylab("Frequancy") 

  ggplot() +  # graphing normal distribution
  geom_line(mapping = aes(mean_curve, x_dist)) +
  ggtitle("sample mean values of y with 90% confidence interval lines") +
  xlab("mean iQ") +
  ylab("probability")+
  geom_vline(xintercept = lower_int) +
  geom_vline(xintercept = upper_int)

#looks good

  
#Part 2
  
#Hypothesis is that the average iQ of the students in the school is higher than the average(100)
#this requires a one tail hypothesis test

qqnorm(y)   #testing if we can use a normal distribution or t-curve
qqline(y)
#we should use a t-curve and not a noraml distribution

#stating our null and alternative hypothesis
#H_0 mu <= 100
#H_a mu > 100

t_stat <- (mean(y)-100)/(sd(y)/sqrt(length(y))) #getting the t-stat of the data to test the alternative hypothesis

# now to make a sample distribution t-curve
P_value <- pt(abs(t_stat), df = length(y)-1, lower.tail = FALSE) # it is one sided so it is not multiplied by 2
# we are also testing the alternative so we need lower.tail = TRUE since that would represent the p value for a mean smaller than 100

#we can test our answer with the piece of code t.test()
t_test <- t.test(y, mu = 100, alternative = '')





#####################
# Problem 2
#####################
  
rm(list=ls())  # clears enviroment from part 1
  
#install.packages("Rcpp")   # I needed to intall these packages to do part 1
#install.packages("GGally")
  
library(tidyverse)  #installing libraries
library("GGally")  

#Part 1)
#here I simply read the data and graph all the colums with numerical non region data against eachother, this is done with ggpairs which is uses GGally and ggplot
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", 
                          header=T)
ggpairs(expenditure[,2:5], labels = c("Y", "X1", "X2", "X3"), main = "All colums plotted against eachotehr with the corresponding correlation")


#Part 2)
ggplot(data = expenditure) + #graphing Y against region
  geom_point(mapping = aes(y = Y, x = Region))+
  ggtitle("per capita spending on housing assistance grouped by region")

for(i in 1:4)  #creating objects containing the section of the dataset which are from the same region
  { 
  nam <- paste("Region_", i, sep = "")
  assign(nam, expenditure[expenditure$Region == i,])
  }
mean(Region_1$Y)  #printing the mean of Y
mean(Region_2$Y)
mean(Region_3$Y)
mean(Region_4$Y)


#Part 3)
ggplot(data = expenditure) +  #its a simple plot
  geom_point(mapping = aes(y = Y, x = X1, colour = as.factor(Region), shape = as.factor(Region))) + 
  labs(colour="Region", shape = "Region") +
  ggtitle("per capita spending on housing assistance vs per capita income")



##########################
#some workings
#########################



# #x <- vector(length = 1000)
# dist = dnorm(y, mean(y), sd(y))
# dist


# for (i in 1:4)
# {
#   for (j in 1:4)
#   {
#     if (i < j)
#     {}
#     else
#     {
#       plot(expenditure[,i], expenditure[,j]) +
#         xlab("title_vector[i]") +
#         ylab("title_vector[j]")
#     }
#   }
# }
# title_vector

# Y <- as.numeric(expenditure[,2 ])
# X1 <- as.numeric(expenditure[,3 ])
# X2 <- as.numeric(expenditure[,4 ])
# X3 <- as.numeric(expenditure[,5 ])
# title_vector <-  c("per capita expenditure on shelters/housing assistance in state","per capita personal income in state", "Number of residents per 100,000 that are 'financially insecure'in state","Number of people per thousand residing in urban areas in state")
# data_vector <- c(Y,X1,X2,X3)
# data_vector[2]
# plot(expenditure[,1], expenditure[,2])
# plot.ts(expenditure)

# expenditure$Y
# u[1]$Y
# u_1 <- u[1]$Y
# u_1
# mean(u_1$Y)
# u[1]$Y

# Region_1 <- group_by(expenditure, Region)
# Region_1
# ##view(Region_1)
# u_1 <- split(expenditure, f = expenditure$Region == 1)

# mean_vect = c()
# for(i in 1:4)
# { 
#   mean_vect[i] = mean(paste("Region_", i, sep =""))
#   
# }
# v_1 <- expenditure[expenditure$Region == 3,]
# mean(v_1$Y)

#scale_shape_identity(guide = "legend") + 
#scale_colour_identity(guide = "legend") 

#mean_curve
#print(i)
#t_stat
#t_test
#alpha = 0.05
#P-value < alpha
# thus there is evidence that mean(y) != 100
# to see if it is in the upper or lower tail we need to check again
# however we know from the t_stat being negative that it is in the lower tail
#t_test <- t.test(y, mu = 100, alternative = 'greater')