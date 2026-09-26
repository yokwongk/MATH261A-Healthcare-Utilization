#Homework 2 MATH 261A 

#Question 3 
set.seed(123)
  
 n_sim <- 1000
 b0 <- 2
 b1 <- 3
 
 
 #Replicaiton over 1000 simulations
 
 simulation1 <- replicate(n_sim, {
   n<- 100
   X <- runif(n, min = 0, max =10)
   e <- rnorm(n,mean=0,sd=1)
   Y <- b0 + b1*X + e
   lm_fit <- lm(Y~X)
   summary(lm_fit)
   
   b1_hat <- coef(summary(lm_fit))["X", "Estimate"]
   se_b1 <- coef(summary(lm_fit))["X", "Std. Error"]
   
  lower_bound <- b1_hat - 2*se_b1
  upper_bound <- b1_hat + 2*se_b1
  
  return ((b1>= lower_bound) && (b1<= upper_bound))
  })
 
summary(simulation1)
 

#Error is uniform (-3,3)


simulation2 <- replicate(n_sim, {
  n<- 100
  X <- runif(n, min = 0, max =10)
  e <- runif(n,min = -3, max =3)
  Y <- b0 + b1*X + e
  lm_fit <- lm(Y~X)
  summary(lm_fit)
  
  b1_hat <- coef(summary(lm_fit))["X", "Estimate"]
  se_b1 <- coef(summary(lm_fit))["X", "Std. Error"]
  
  lower_bound <- b1_hat - 2*se_b1
  upper_bound <- b1_hat + 2*se_b1
  
  return ((b1>= lower_bound) && (b1<= upper_bound))
})

summary(simulation2)

#simulation 3

simulation3 <- replicate(n_sim, {
  n<- 15
  X <- runif(n, min = 0, max =10)
  e <- rnorm(n,mean=0,sd=1)
  Y <- b0 + b1*X + e
  lm_fit <- lm(Y~X)
  summary(lm_fit)
  
  b1_hat <- coef(summary(lm_fit))["X", "Estimate"]
  se_b1 <- coef(summary(lm_fit))["X", "Std. Error"]
  
  lower_bound <- b1_hat - 2*se_b1
  upper_bound <- b1_hat + 2*se_b1
  
  return ((b1>= lower_bound) && (b1<= upper_bound))
})

summary(simulation3)


#Question 4 
library(palmerpenguins)
data(penguins)

adelie_females <-  subset (penguins, sex == "female" & species == "Adelie")
lm_penguin1 <- lm(body_mass_g ~ bill_length_mm,
   data = adelie_females)
summary(lm_penguin1)

plot(lm_penguin1$fitted.values, lm_penguin1$residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted Values")

predict(lm_penguin1, 
        newdata= data.frame(bill_length_mm = 40), 
        interval = "confidence",
        level = 0.95)


predict(lm_penguin1, 
        newdata= data.frame(bill_length_mm = 40), 
        interval = "predict",
        level = 0.95)


#Question 5
library(lubridate)

judges_appointments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv')
judges_appointments <- subset(judges_appointments, court_name =="Supreme Court of the United States")


judges_appointments$nomination_date <- mdy(judges_appointments$nomination_date)
judges_appointments$commission_date <- mdy(judges_appointments$commission_date)

judges_appointments$nomination_year <- year(judges_appointments$nomination_date) 


judges_appointments$confirmation_length <- as.numeric(
  difftime(judges_appointments$commission_date,
           judges_appointments$nomination_date, units = "days"))

judges_appointments <- subset(judges_appointments, !is.na(nomination_year) & !is.na(confirmation_length))

lm_judge <- lm(confirmation_length ~ nomination_year,data = judges_appointments)
summary(lm_judge)
confint(lm_judge, level =.95)

#Plot nomination vs confirmation
plot(judges_appointments$nomination_year, judges_appointments$confirmation_length, 
     xlab = "Year of nomination",
     ylab = " Confirmation length (days)", 
     main = "Confirmation length vs Year",
     pch= 19)

abline(lm_judge, col= "blue", lwd= 2)

#Plot residuals

plot(judges_appointments$nomination_year, lm_judge$residuals, 
     xlab = "Year of nomination",
     ylab = " Residuals", 
     main = "Residuals vs Predictor Plot",
     pch= 19)
abline(h=0, col= "red", lwd= 2)




#QQ plot residuals to test normality of errors
qqnorm(lm_judge$residuals)
qqline(lm_judge$residuals)

