#2a:
set.seed(123)
n <- 200 #Sample size.
x <- rnorm(n,mean=0,sd=1)
error <- rnorm(n,mean=0,sd=2)
y <- 2 + 3*x^3 + error

my_df <- data.frame(x,y)

#Filename for upcoming plot:
png("hw03_q02a_scatter_x_y.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x, #x-variable
     my_df$y, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "blue", #color of pch
     main = "Scatter Plot of y vs. x", #title of plot
     xlab = "x", #x-axis label
     ylab = "y") #y-axis label
dev.off() #Close the graphics device.

#2b:
my_model <- lm(y ~ x, data = my_df)
summary(my_model)

#Filename for upcoming plot:
png("hw03_q02b_scatter_with_fit.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x, #x-variable
     my_df$y, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "blue", #color of pch
     main = "Scatter Plot of y vs. x", #title of plot
     xlab = "x", #x-axis label
     ylab = "y") #y-axis label
#Add the fitted regression line to the plot and color it red:
abline(my_model, col = "red")
dev.off() #Close the graphics device.

#2c:
#Filename for upcoming plot:
png("hw03_q02c_residual_vs_x.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x, #x-variable
     my_model$residuals, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "darkred", #color of pch
     main = "Residuals vs. x", #title of plot
     xlab = "x", #x-axis label
     ylab = "Residuals") #y-axis label
#Add a horizontal line at zero, for visual reference:
abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q02c_qqplot.png",
    width = 800, height = 600, units = "px")
#Normal QQ-plot:
qqnorm(residuals(my_model),
       pch = 16, col = "orange",
       main = "QQ-plot of Residuals",
       cex.main= 1.0)
qqline(residuals(my_model), col = "black") #no departure from normality
dev.off() #Close the graphics device.

#2d:
#Add new column to dataframe:
my_df$x_1 <- x^3 #Polynomial transformation as remedy.

new_model <- lm(y ~ x_1, data = my_df)
summary(new_model)

#Filename for upcoming plot:
png("hw03_q02d_scatter_with_fit.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x_1, #x-variable
     my_df$y, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "blue", #color of pch
     main = "Scatter Plot of y vs. x_1", #title of plot
     xlab = "x_1", #x_1-axis label
     ylab = "y") #y-axis label
#Add the fitted regression line to the plot and color it red:
abline(new_model, col = "red")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q02d_residual_vs_x_1.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x_1, #x-variable
     new_model$residuals, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "darkred", #color of pch
     main = "Residuals of New Model vs. x_1", #title of plot
     xlab = "x_1", #x-ax_1is label
     ylab = "Residuals of New Model") #y-axis label
#Add a horizontal line at zero, for visual reference:
abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q02d_qqplot.png",
    width = 800, height = 600, units = "px")
#Normal QQ-plot:
qqnorm(residuals(new_model),
       pch = 16, col = "orange",
       main = "QQ-plot of Residuals of New Model",
       cex.main= 1.0)
qqline(residuals(new_model), col = "black") #no departure from normality
dev.off() #Close the graphics device.
