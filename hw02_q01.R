data_path = "06_Data_from_Professor/Data pack 1/Crimerate.txt"
crimerate_df <- read.table(data_path, header=TRUE)

#1b:
crime_rate_model <- lm(Crime ~ Diplomapct, data = crimerate_df)
summary(crime_rate_model)

#1c:
#sigma() extracts the residual standard error, which we square to
#get the estimate of the error variance:
sigma(crime_rate_model)^2

#1f:
#Filename for upcoming plot:
png("hw02_q01f_crime_rate_data_and_fit.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(crimerate_df$Diplomapct, #x-variable
     crimerate_df$Crime, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 0.5, #size of pch
     col = "black", #color of pch
     main = "Scatter plot", #title of plot
     xlab = "Percentage in County with at least an HS Diploma", #x-axis label
     ylab = "Crimes Reported (per 100,000 County Residents) in a Year") #y-axis label
#Add the fitted regression line to the plot and color it red:
abline(crime_rate_model, col = "red")
dev.off() #Close the graphics device.

#1g:
qt(0.995, 82)
