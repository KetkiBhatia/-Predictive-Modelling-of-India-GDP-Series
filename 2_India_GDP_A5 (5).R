# Load required libraries
install.packages("forecast")
install.packages("fpp2")
install.packages("TTR")
install.packages("car")
library(forecast)
library(ggplot2)
library(readxl)
library(fpp2)
library(car)
library(TTR)
library(lmtest)
library(dplyr)

my_data <- read_excel("C:/Users/Ketki/Downloads/India GDP Series.xlsx")  #Upload the data
filtered_data<-na.omit(my_data)
#Plot the GDP data over years 
ggplot(data = filtered_data, aes(x =Year , y = `GDP India (Const 2015 $)`))+
  geom_point() +
  labs(title = " GDP India (Const 2015 $) over Year",
       x = "Year",
       y = "GDP India (Const 2015 $)")
ggplot(filtered_data, aes(x =Year , y = `GDP India (Const 2015 $)`)) +
  geom_line() +
  labs(title = " GDP India (Const 2015 $) over Year",
       x = "Year",
       y = "GDP India (Const 2015 $)")
#Plot  GDP over time

library(ggplot2)
ggplot(filtered_data, aes(x =Time , y = `GDP India (Const 2015 $)`)) +
  geom_line() +
  labs(title = " `GDP India (Const 2015 $)`) over time",
       x = "Time",
       y = "GDP India (Const 2015 $)")

ggplot(data = filtered_data, aes(x =Time , y = `GDP India (Const 2015 $)`))+
  geom_point() +
  labs(title = " `GDP India (Const 2015 $)`) over time",
       x = "Time",
       y = "GDP India (Const 2015 $)")





### Check whether the GDP series has any seasonality, cycles and/or secular trends ###
#secular

#"The analysis of the GDP series reveals the presence of a significant secular trend.The examination of the GDP series reveals a notable and consistent upward secular trend. This suggests a sustained and positive movement in the GDP values over the analyzed time period.
### Write down the time series model that forecasts the GDP
#Model chosen is the ARIMA (AutoRegressive Integrated Moving Average) model.
#The ARIMA model is denoted as ARIMA(p, d, q), where:p (AR order): Number of autoregressive lags.d (Integration order): Degree of differencing needed to make the series stationary.q (MA order): Number of moving average lags.

#The data is non stationary therefore we are choosing Arima model
# Run the ARIMA model with automatic p, d, q calculation
arima_model <- auto.arima(my_data$`GDP India (Const 2015 $)`)
summary(arima_model)

#Removing time column
my_data <- filtered_data[,-2]
 str(my_data)
 
acf(my_data$`GDP India (Const 2015 $)`)
pacf(my_data$`GDP India (Const 2015 $)`)
# Explain ACF and PACF correlation with p and q values
#The spike in ACF occurs at 0 value and then it declines, this corresponds to the p value that is 0.
#The spike in PACF occurs at 1 as the graph for PACF starts from 1 and this corresponds with the q value that is 1

# Predict GDP for the years 2023 and 2024
forecast_values <- forecast(arima_model, h = 2)
predicted_gdp_2023 <- forecast_values$mean[1]
predicted_gdp_2024 <- forecast_values$mean[2]

# Print predicted GDP for 2023 and 2024
cat("Predicted GDP for 2023:", predicted_gdp_2023, "\n")
cat("Predicted GDP for 2024:", predicted_gdp_2024, "\n")

# Plot the predicted GDP for the years 2023 and 2024
forecast_values <- forecast(arima_model, h = 2*12)
plot(forecast_values)
lines(forecast_values$fitted,col="green") 
predicted_gdp_2023 <- forecast_values$mean[1]
predicted_gdp_2024 <- forecast_values$mean[2]

# The predicted_gdp_2023 value is 3074.22708925851
# The predicted_gdp_2024 value is 3193.47651548415

