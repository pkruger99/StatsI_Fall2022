rm(list=ls())

library(tidyverse)
library(ggplot2)

dat <- read.csv("C:incumbents_subset.csv", stringsAsFactors=FALSE,na.strings=c(""))


###############
#Question 1
###############
#part 1
summary(lm(dat$voteshare~dat$difflog)) #running regression summary

#Part 2
ggplot(dat, mapping = aes(difflog,voteshare))+  #graph
  geom_point(size = 0.5) + geom_smooth(method='lm', formula= y~x, size = 0.5)+
  ggtitle("regression for the impact of difflog on voteshare")

ggsave("Plot 1.2.png", width = 6, height = 4, units = 'in', dpi = 300) #save graph

#Part 3
res1 <- summary(lm(dat$voteshare~dat$difflog))$residual #saving the residuals to an object

#Part 4
lm(dat$voteshare~dat$difflog) #getting the slope and intercept for projection.


##############
#Question 2
##############
#part 1
summary(lm(dat$presvote~dat$difflog)) #running regression summary

#Part 2
ggplot(dat, mapping = aes(difflog,presvote))+  #graph
  geom_point(size = 0.5) + geom_smooth(method='lm', formula= y~x, size = 0.5)+
  ggtitle("regression for the impact of difflog on presvote")

ggsave("Plot 2.2.png", width = 6, height = 4, units = 'in', dpi = 300) #save graph

#Part 3
res2 <- summary(lm(dat$presvote~dat$difflog))$residual #saving the residuals to an object

#Part 4
lm(dat$presvote~dat$difflog) #getting the slope and intercept for projection.

##############
#Question 3
##############
#part 1
summary(lm(dat$voteshare~dat$presvote)) #running regression summary

#Part 2
ggplot(dat, mapping = aes(presvote,voteshare))+  #graph
  geom_point(size = 0.5) + geom_smooth(method='lm', formula= y~x, size = 0.5)+
  ggtitle("regression for the impact of presvote on voteshare")

ggsave("Plot 3.2.png", width = 6, height = 4, units = 'in', dpi = 300) #save graph

#Part 3
lm(dat$voteshare~dat$presvote) #getting the slope and intercept for projection.


################
#Question 4
################
#part 1
summary(lm(res1~res2)) #running regression summary

#Part 2
ggplot(dat, mapping = aes(res2,res1))+  #graph
  geom_point(size = 0.5) + geom_smooth(method='lm', formula= y~x, size = 0.5)+
  ggtitle("regression for the impact of residuals of voteshare on residuals of presvote")

ggsave("Plot 4.2.png", width = 6, height = 4, units = 'in', dpi = 300) #save graph

#Part 3
lm(res1~res2) #getting the slope and intercept for projection.


################
#Question 5
################
#Part 1
summary(lm(dat$voteshare~cbind(dat$difflog,dat$presvote))) #Getting the regression

#Part 2
lm(dat$voteshare~cbind(dat$difflog,dat$presvote)) #getting terms for prediction equation





