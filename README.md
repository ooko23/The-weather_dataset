# The-weather_dataset
Advanced exploratory data analysis of weather dateset
# Load the dataset
library(readxl)
weather_data <- read_excel("C:/Users/admin/Downloads/weather data.xlsx")

View(weather_data)
Temperature trends over time
library(ggplot2)

ggplot(weather_data, aes(x = as.POSIXct(`Date/Time`, format = "%m/%d/%Y %H:%M"), y = Temp_C)) +
  geom_line() +
  xlab("Date/Time") +
  ylab("Temperature (C)") 
## Warning: Removed 3456 rows containing missing values (`geom_line()`).

Analyze the relationship between wind speed and temperature
ggplot(weather_data, aes(x = `Wind Speed_km/h`, y = Temp_C)) +
  geom_point() +
  xlab("Wind Speed (km/h)") +
  ylab("Temperature (C)") 

correlation <- cor(weather_data$`Wind Speed_km/h`, weather_data$Temp_C)
correlation
## [1] -0.061876
Forecasting temperature using ARIMA
library(forecast)
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
# Assuming the temperature data is in a column named "Temp_C"
temperature <- ts(weather_data$Temp_C)

# Fit ARIMA model
model <- auto.arima(temperature)

# Forecast next 10 time points
forecast <- forecast(model, h = 10)

# Plot the forecast
plot(forecast, main="")

Detecting outliers or anomalies
boxplot(weather_data$Temp_C, main = "")
 ## Decompose the data

library(stats)
# Convert 'Years' column to a date format
weather_data$`Date/Time` = as.POSIXct(weather_data$`Date/Time`, format = "%m/%d/%Y %H:%M")


# Remove rows with null values
clean_data <- weather_data[complete.cases(weather_data), ]
weather_data = clean_data

# Convert the data to a time series object
ts_data <- ts(weather_data$Temp_C, frequency = 6, start = c(min(weather_data$`Date/Time`)))

#decompose the ts
decomposed_data<-decompose(ts_data)

plot(decomposed_data)
 ## Calculate the correlation matrix

# Select only the numeric columns
numeric_columns <- weather_data[, sapply(weather_data, is.numeric)]

# Calculate the correlation matrix for numeric columns
cor_matrix <- cor(numeric_columns)

# Print the correlation matrix
print(cor_matrix)
##                       Temp_C Dew Point Temp_C   Rel Hum_% Wind Speed_km/h
## Temp_C            1.00000000       0.93597968 -0.27173231     -0.05446374
## Dew Point Temp_C  0.93597968       1.00000000  0.07766300     -0.09274411
## Rel Hum_%        -0.27173231       0.07766300  1.00000000     -0.09696754
## Wind Speed_km/h  -0.05446374      -0.09274411 -0.09696754      1.00000000
## Visibility_km     0.27644938       0.05915253 -0.64312185      0.01595147
## Press_kPa        -0.23662440      -0.33248766 -0.25391316     -0.37859951
##                  Visibility_km  Press_kPa
## Temp_C              0.27644938 -0.2366244
## Dew Point Temp_C    0.05915253 -0.3324877
## Rel Hum_%          -0.64312185 -0.2539132
## Wind Speed_km/h     0.01595147 -0.3785995
## Visibility_km       1.00000000  0.2532171
## Press_kPa           0.25321707  1.0000000
library(ggplot2)

# Convert correlation matrix to long format
cor_df <- reshape2::melt(cor_matrix)

# Plot the heatmap using ggplot2
ggplot(data = cor_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme_minimal()

# Add color legend
heat_colors <- c("blue", "white", "red")
breaks <- seq(-1, 1, by = 0.2)
labels <- c("Low Correlation", "", "", "", "", "High Correlation")
