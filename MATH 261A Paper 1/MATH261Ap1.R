library(tidyverse)
library(dplyr)
library(ggplot2)
url <- "https://data.chhs.ca.gov/dataset/aea55da9-a322-49e9-b187-a216dc4e460d/resource/ea58d9aa-e4e6-46d6-8815-3442960873c0/download/services_rpt_data-service-category.csv"
df <- read.csv(url)
mmhi <- read.csv("C:/Users/aroon/OneDrive - sjsu.edu/MATH 261 A/mmhi.csv")

# Subset service count data for 2023 for each county 
county_summary <- df %>%
  filter(reporting_year== 2023) %>%
  group_by(county_name) %>%
  summarise(
    com_service_count = sum(service_count[payer_type == "Commercial"], na.rm =TRUE), 
    total_service_count = sum(service_count, na.rm= TRUE), 
    com_percent = (com_service_count/total_service_count) * 100
  )

#Clean and Join with Median income by county 2020-2024 

mmhi_cleaned <- mmhi%>%
  mutate(
    county_name = str_remove(county_name, " County.*"),
    median_income = parse_number(median_income)
    )
 data<- county_summary %>%
  left_join(mmhi_cleaned, by = "county_name")
 
#Regression
lm_fit <- lm(data$com_percent ~ data$median_income)
summary(lm_fit)

#Plot
plot(data$median_income, data$com_percent, 
     xlab = "Household Median Income", 
     ylab = "Commercial Insurance Percentage", 
    main = "Commercial Insurance Utilization vs Median Household Income", 
   )
abline(lm_fit,col = "darkblue", lwd = 1)


#Residuals vs Predictor 

plot(data$median_income, lm_fit$residuals, 
     xlab = "Median Household income", 
     ylab = "Residuals", 
     main = "Residuals vs Predictor"
     )
abline(h=0)

#QQ plot
qqnorm(lm_fit$residuals)
qqline(lm_fit$residuals)

#Suppression ID = Y
suppressed_summary <- df %>%
  filter(reporting_year == 2023, suppression_ind == "Y") %>%
  group_by(payer_type) %>%
  summarise(suppressed_rows = n(), .groups = "drop") %>% 
  mutate(sup_percent = (suppressed_rows / sum(suppressed_rows)) * 100)
  
#PLOT 
ggplot(suppressed_summary, aes(x = payer_type, y = sup_percent)) +
  geom_col(width = 0.4, fill = "black") +
  geom_text(aes(label = paste0(round(sup_percent, 1), "%")), 
            vjust = -0.6, 
            size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Share of Suppressed Rows by Payer Type",
       x = "Payer Type",
       y = "Percentage (%)") +
  theme_classic() 


