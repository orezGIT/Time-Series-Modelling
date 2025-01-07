#load packges 


library(TTR) 
library(forecast)
library(forecast)
library(readxl)


#load the dataset 

# create a variable for the file path
file_path <- "C:/Users/Orezime Isaac/Desktop/Applied Statitistics Assessment/Task 3 - Time Series Modelling/UK Vital Statistics/Vital statistics in the UK.xlsx"

# Read the Excel file and skip the first 4 rows
uk_data <- read_excel(file_path, sheet = 3, skip = 4) %>%
  dplyr::select(1:3) # Select the first three columns

#display the data
uk_data

# Set the fifth row as the header
colnames(uk_data) <- uk_data[1, ] # Use the first row as column names

# Remove the header row from the data
uk_data <- uk_data[-1, ]           

# Reset row indexing
rownames(uk_data) <- NULL

# Reorder uk_data by Year in ascending order
uk_data <- uk_data[order(uk_data$Year), ]

#disaplay the structure 
str(uk_data)

# display few rows
head(uk_data)


#convert the data types to numerical column
uk_data$Year <- as.numeric(uk_data$Year)
uk_data$`Number of deaths: United Kingdom` <- as.numeric(uk_data$`Number of deaths: United Kingdom`)
uk_data$`Number of deaths: England and Wales` <- as.numeric(uk_data$`Number of deaths: England and Wales`)

#check if it has converted to numerical
str(uk_data)


# Set the y-axis limits to cover the range of both columns
y_range <- range(c(uk_data$`Number of deaths: United Kingdom`, 
                      uk_data$`Number of deaths: England and Wales`), na.rm = TRUE)

# Plot the first line
plot(uk_data$Year, uk_data$`Number of deaths: United Kingdom`, 
     type = "l", col = "blue",
     xlab = "Year", 
     ylab = "Number of Deaths", 
     main = "Death Rate: UK vs England & Wales",
     ylim = y_range)  # Adjust y-axis limits

# Add the second line
lines(uk_data$Year, uk_data$`Number of deaths: England and Wales`, 
      col = "red")

# Add a legend for clarity
legend("bottomright", 
       legend = c("United Kingdom", "England & Wales"), 
       col = c("blue", "red"), 
       lwd = 2)


# Replace missing values in the UK column with values from England & Wales
uk_data$`Number of deaths: United Kingdom`[is.na(uk_data$`Number of deaths: United Kingdom`)] <- 
  uk_data$`Number of deaths: England and Wales`[is.na(uk_data$`Number of deaths: United Kingdom`)]

#display result
str(uk_data)

#extract uk year and death columns
uk_death <- uk_data %>% dplyr::select(Year, `Number of deaths: United Kingdom`)
uk_death


# Store data for uk_death as a time series
uk_death_ts <- ts(
  uk_death$`Number of deaths: United Kingdom`,
  start = min(uk_death$Year), #minimum starting year 
  frequency = 1               #annual combine data
)

head(uk_death_ts)


#create variable for england & wales
england_wales <- uk_data %>% dplyr::select(Year, `Number of deaths: England and Wales`)
england_wales


#store data of england & wales as time series
eng_wales_ts <- ts(england_wales$`Number of deaths: England and Wales`, 
                   start = min(england_wales$Year), 
                   frequency = 1)
               
head(eng_wales_ts)                


#Plotting the time series uk
plot.ts(uk_death_ts)

#plot time series for england $ wales 
plot.ts(eng_wales_ts)

#decompose the uk death time series using simple moving average 
uk_death_ts_sma3 <- SMA(uk_death_ts,n=3) 

# plot the decompose data
plot(uk_death_ts_sma3)


#decompose the england & wales time series using simple moving average 
eng_wales_ts_sma3 <- SMA(eng_wales_ts, n=3) 

# Plot the decompose data
plot(eng_wales_ts_sma3)



# Using Holt-Winters Exponential Smoothing (trend and no seasonality) to make forecast 

#use Holt’s exponential smoothing to fit a predictive model for number of deaths in the UK 
uk_death_forecast <- HoltWinters(uk_death_ts, gamma = FALSE) 
uk_death_forecast

#plot the original series and the forecast 
plot(uk_death_forecast)



#use Holt’s exponential smoothing to fit a predictive model for number of deaths in England and Wales 
england_wales_forecast <- HoltWinters(eng_wales_ts, gamma = FALSE) 
england_wales_forecast

#plot the original series and the forecast 
plot(england_wales_forecast)


#make a forecast of 10 years ( 2022-2032) for Death in the UK 
uk_death_forcast2 = forecast(uk_death_ts, h=10)

#plot the forecast for death in the UK 
plot(uk_death_forcast2)


#make a forecast of 10 years ( 2022-2032) for Death in England & Wales
england_wales_forecast2 = forecast(eng_wales_ts, h=10)

#plot the forecast for death in the UK 
plot(england_wales_forecast2)



#check autocorrelation in the residuals 

#hypothesis Testing 
#Null Hypothesis (H₀):
#There is no autocorrelation in the residuals up to the specified lag.

#Alternative Hypothesis (H₁):
#There is autocorrelation in the residuals up to at least one of the lags. 

#Death in the UK time series model
#perform ACF plot on residuals
acf(na.omit(uk_death_forcast2$residuals), lag.max=20) 

#perform Box-Ljung test on residuals
Box.test(uk_death_forcast2$residuals, lag=20, type="Ljung-Box")


#Death in England & Wales
#perform ACF plot on residuals
acf(na.omit(england_wales_forecast2$residuals), lag.max=20) 

#perform Box-Ljung test on residuals
Box.test(england_wales_forecast2$residuals, lag=20, type="Ljung-Box")

#define a function 
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors: 
  mybinsize <- IQR(forecasterrors)/4  
  mysd <- sd(forecasterrors) 
  mymin <- min(forecasterrors) - mysd*5  
  mymax <- max(forecasterrors) + mysd*3 
  
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd) 
  mymin2 <- min(mynorm) 
  mymax2 <- max(mynorm) 
  
  if (mymin2 < mymin) { mymin <- mymin2 } 
  if (mymax2 > mymax) { mymax <- mymax2 } 
  
  # make a red histogram of the forecast errors, with the normally distributed data overlaid
  mybins <- seq(mymin, mymax, mybinsize) 
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins) 
  
  # freq=FALSE ensures the area under the histogram = 1 
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins) 
  
  # plot the normal curve as a blue line on top of the histogram of forecast errors 
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2) 
}

#display forecast errors for uk death model
plot.ts(uk_death_forcast2$residuals) # make time series plot

uk_death_forcast2$residuals <- na.omit(uk_death_forcast2$residuals)  # Removing NA values
plotForecastErrors(uk_death_forcast2$residuals)  # Make a histogram


#display forecast errors for England & Wales death model
plot.ts(england_wales_forecast2$residuals) # make time series plot

england_wales_forecast2$residuals <- na.omit(england_wales_forecast2$residuals)  # Removing NA values
plotForecastErrors(england_wales_forecast2$residuals)  # Make a histogram



#USING ARIMA TIME SERIES MODEL

#difference the time series for deaths in the uk 
uk_deathdiff <- diff(uk_death_ts, differences=1) 

plot.ts(uk_deathdiff)


#difference the time series for deaths in England & Wales 
engwales_deathdiff <- diff(eng_wales_ts, differences=1) 

plot.ts(engwales_deathdiff)


#acquire the value of p and q from uk_deathdiff 
#obtaing q values
acf(uk_deathdiff, lag.max=20) 

# plot a correlogram 
acf(uk_deathdiff, lag.max=20, plot=FALSE) # get the autocorrelation values 


#obtaing p values
pacf(uk_deathdiff, lag.max=20) # plot a partial correlogram 

# plot a correlogram 
pacf(uk_deathdiff, lag.max=20, plot=FALSE) # get the partial autocorrelation vakues 



#acquire the value of p and q from engwales_deathdiff 
#obtaing q values
acf(engwales_deathdiff, lag.max=20) 

# plot a correlogram 
acf(engwales_deathdiff, lag.max=20, plot=FALSE) # get the autocorrelation values 


#obtaing p values
pacf(engwales_deathdiff, lag.max=20) # plot a partial correlogram 

# plot a correlogram 
pacf(engwales_deathdiff, lag.max=20, plot=FALSE) # get the partial autocorrelation vakues 



#fit ARIMA model to the uk time series with ARIMA(0,1,1)
uk_deatharima <- arima(uk_death_ts, order=c(0,1,1)) # fit an ARIMA(0,1,1) model 
uk_deatharima



#fit ARIMA model to the England & wales time series with ARIMA(0,1,1)
engwales_deatharima <- arima(eng_wales_ts, order=c(0,1,1)) # fit an ARIMA(0,1,1) model 
engwales_deatharima


# make a forecast of 5 years for uk_deatharima
uk_arimaforecast <- forecast(uk_deatharima, h=5)
uk_arimaforecast


# make a forecast of 5 years for engwales_deatharima
engwales_arimaforecast <- forecast(engwales_deatharima, h=5)
engwales_arimaforecast 

#plot the uk forecast 
plot(uk_arimaforecast) 

#plot the England & Wales forecast 
plot(engwales_arimaforecast) 


#check for correlations between consecutive forecast in the uk
acf(uk_arimaforecast$residuals, lag.max=20) 
Box.test(uk_arimaforecast$residuals, lag=20, type="Ljung-Box")


#check for correlations between consecutive forecast in the uk
acf(engwales_arimaforecast$residuals, lag.max=20) 
Box.test(engwales_arimaforecast$residuals, lag=20, type="Ljung-Box")


#check if the forecast errors are normally distributed for uk_arimaforecast
plot.ts(uk_arimaforecast$residuals) 

# time plot forecast error 
plotForecastErrors(uk_arimaforecast$residuals) # make a histogram 


#check if the forecast errors are normally distributed for engwales_arimaforecast
plot.ts(engwales_arimaforecast$residuals) 

# time plot forecast error 
plotForecastErrors(engwales_arimaforecast$residuals) # make a histogram 


