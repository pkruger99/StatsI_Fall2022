install.packages(car)
library(car)
data(Prestige)
help(Prestige)

###################
#Question 1
###################

#Part 1
Prestige$professional <- vector(mode="integer", length=nrow(Prestige)) #adding dummy column

for (i in 1:nrow(Prestige)){
  ifelse(Prestige[i,6] == "prof", Prestige[i,7] <- 1, Prestige[i,7] <- 0)
} #filling dummy column

#removing entries with NA for type
Prestige2 <- Prestige

for (i in 1:nrow(Prestige2)){
  if (is.na(Prestige2[i,6]) == TRUE){
    Prestige2 <- Prestige2[-c(i),]}
}
Prestige2   
   
#Part 2
summary(lm(Prestige2$prestige ~ Prestige2$income +Prestige2$professional + Prestige2$income:Prestige2$professional)) #getting the coefficients


interact_reg <- lm(Prestige2$prestige ~ Prestige2$income +Prestige2$professional + Prestige2$income:Prestige2$professional) #assigning to object so that the cose from lecture 11 can be used to graph the results
interact_reg
plot(Prestige2$income, Prestige2$prestige, type = "n")+
  abline(interact_reg$coefficients[1],interact_reg$coefficients[2], col = 2)+
  abline(interact_reg$coefficients[1]+interact_reg$coefficients[3],interact_reg$coefficients[2]+interact_reg$coefficients[4], col =4, lty = 2)



