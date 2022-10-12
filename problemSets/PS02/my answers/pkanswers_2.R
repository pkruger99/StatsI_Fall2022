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

########################
#Question 1
########################

library(tidyverse)  #loading in tidyverse for graphing

x <- matrix(c(14,7,6,7,7,1),ncol = 3) #initialising a matrix with the values from the table
x

#calculating x_e for all values in x
x_e <- matrix(data = NA,ncol = 3, nrow =2) # creating empty matrix to put the expected values in
x_e
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    x_e[i,j] <- (sum(x[i,])*sum(x[,j]))/sum(x) #going through all the slots in the matrix and putting in the expected value
  }}

x_chi <- matrix(data = NA,ncol = 3, nrow =2) # creating empty matrix to put the chi squared values in
for(i in 1:nrow(x_chi)){
  for(j in 1:ncol(x_chi)){
    x_chi[i,j] <- ((x[i,j]-x_e[i,j])**2)/x_e[i,j] #going through all the slots in the matrix and putting in the chi squared contribution of that slot
  }}
x_chi
chi_squared <- sum(x_chi) #summing terms in x_chi to get chi squared term
chi_squared

pchisq(chi_squared, df = (ncol(x)-1)*(nrow(x)-1),  lower.tail=FALSE) #gets p-value from chi squared test statistic

#with alpha = 0.1 we have insufficient evidence to exclude that the variables are not independent.

x_sr <-  matrix(data = NA,ncol = 3, nrow =2) # creating empty matrix to put the standardized residuals in
for(i in 1:nrow(x_sr)){
  for(j in 1:ncol(x_sr)){
    x_sr[i,j] <- (x[i,j]-x_e[i,j])/(x_e[i,j]*(1-(sum(x[i,])/sum(x)))*(1-(sum(x[,j])/sum(x)))) #going through all the slots in the matrix and putting in the expected value
    }}
x_sr


#######################
#Question 2
#######################

rm(list=ls())

dat <- read.table("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T, sep = ",")

#view(dat)
#looking at the data there is two villages per GP and the second villiage is always in the even column, furthermore they are numbered in ascending order
#with 300 lines this can be verified relatively easily by scanning through it, however I am writing two short functions to check this, just so the code is generalseable 
#to larger datasets

increase.function <- function(a){
  for(i in 2:length(a[,1])){
    if(a[i,1] < a[(i-1),1]){print("Error in line ")
      print(i)}
    else{}
  }}

two_gp.function <- function(a){
  for(i in 2:length(a[,1])){
    if(i %% 2 == 0 & a[(i-1),1] != a[i,1]){print("Error in line ")
      print(i)}
    else{}
  }}
  

increase.function(dat)  
two_gp.function(dat)     
#they both run without the error in line print, thus the dataset can be manipulated using the following functions  

#for each GP we need the total drinking water projects in GP and whether they had female leader

total_water.function <- function(a){
  for(i in 1:length(a[,6])){
    if(i %% 2 == 1){
      a[i,6] <- a[i,6]+a[(i+1),6]}
    else{}

  }
  return(a)
}

remove_even.function <- function(a){
  remove_vector <- c() #no idea why i have to do it this way removing lines in the if loop gets the incorrect answer
  for(i in 1:length(a[,6])){
    if(i %% 2 == 0){print(i)
      remove_vector <- append(remove_vector, i)
      }
    else{}
  }
  print(remove_vector)
  a <- a[-c(remove_vector),]
  return(a)
}
dat_2 <-remove_even.function(total_water.function(dat))

boxplot(water~female,data = remove_even.function(total_water.function(dat)), main ="per capita spending on housing assistance grouped by region", ylim(c(0,150))) #graphing boxplot
 # 
dat_2 <- total_water.function(dat) 
dat_2 <- remove_even.function(dat_2)
view(dat_2)
# for(i in 1:length(dat[,6])){#print(i)
#   if(i %/% 2 == 0){print(i)#dat <- dat[-c(i),]
#     }
#   else{}
# }
# for(i in 1:length(dat[,6])){
#   if(i %% 2 == 1){print(i)
#     dat[i,6] <- dat[i,6]+dat[(i+1),6]}
#   else{}
#   #return(a)
# }







# print(x[i,j])
# print(x_e[i,j])
# print(sum(x[i,]))
# print(sum(x))
# print(sum(x[,j]))   
