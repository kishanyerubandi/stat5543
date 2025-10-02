yerubandi_stat_5543_hw_02p5_dataset <- read.csv("yerubandi_stat_5543_hw_02p5_dataset.csv", header=TRUE)

yerubandi_model <- lm(RDO_NET_P ~ GiniCoeff, data = yerubandi_stat_5543_hw_02p5_dataset)
summary(yerubandi_model)

#Filename for upcoming plot:
png("hw02p5_data_and_fit.png",
    width = 800, height = 600, units = "px")

#Scatter plot of the two variables:
plot(yerubandi_stat_5543_hw_02p5_dataset$GiniCoeff, #x-variable
     yerubandi_stat_5543_hw_02p5_dataset$RDO_NET_P, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 0.5, #size of pch
     col = "blue", #color of pch
     main = "RDO_NET_P vs. GiniCoeff Scatter Plot with Regression Line", #title of plot
     xlab = "Gini Coefficient (A Measure of Income-Inequality)", #x-axis label
     ylab = "RDO_NET_P (A Measure of Trump's Comparative Outperformance)") #y-axis label

#Add the fitted regression line to the plot and color it red:
abline(yerubandi_model, col = "red")

#Adding a dashed gray line at y=0 to help the reader notice
#that the data is spread across positive and negative y-values:
abline(h = 0, lty = 2, lwd = 1.2, col = "gray50")

#Adding a legend to help the reader know what the main two
#items on the plot are:
legend("bottomleft", 
       legend = c("Data Points", "RDO_NET_P as predicted by GiniCoeff"),
       col = c("blue", "red"),
       pch = 16,
       cex = 0.8 #size of legend text
)
dev.off() #Close the graphics device.
