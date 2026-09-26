
# Homework 1

#Question 3
set.seed(123)
x<- rnorm(100)
e<- rnorm(100)
y<- x+e
lm3<- lm(x~y)
summary(lm3)
plot(y,x)
  
#Question 6

library(rosdata)
data(health)
head(health)

# With the US 
lmfit<- lm(lifespan~ spending, data = health)
summary(lmfit)


plot(health$spending, health$lifespan,
     main = "Life Expectancy vs Health Expenditure", 
     xlab ="Health Expenditure (Total health expenditure/capita)", 
     ylab= "Life Expectancy (Years)")

text(health$spending, health$lifespan, 
     labels = health$country, col = "black", cex=0.5)

abline(lmfit, col = "red", lwd =2)



#Without the US 
health_no_us <- subset(health, country != "USA")

lmfit_no_us<- lm(lifespan~ spending, data = health_no_us)
summary(lmfit_no_us)


plot(health_no_us$spending, health_no_us$lifespan,
     main = "Life Expectancy vs Health Expenditure (Excluding USA) ", 
     xlab ="Health Expenditure (Total health expenditure/capita)", 
     ylab= "Life Expectancy (Years)")

text(health_no_us$spending, health_no_us$lifespan, 
     labels = health$country, col = "black", cex=0.5)

abline(lmfit_no_us, col = "blue", lwd =2)
