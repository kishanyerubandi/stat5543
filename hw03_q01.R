#1a:
input_path <- "06_Data_from_Professor/Data pack 1/ProductionLot.txt"
my_df <- read.table(input_path, header = FALSE)
names(my_df)[1] <- "Duration"
names(my_df)[2] <- "Size"

my_model <- lm(Duration ~ Size, data = my_df)
summary(my_model)

#Filename for upcoming plot:
png("hw03_q01a_scatter_x_y.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$Size, #x-variable
     my_df$Duration, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "blue", #color of pch
     main = "Scatter Plot of Duration vs. Size", #title of plot
     xlab = "Size of Lot Produced in Run (integer)", #x-axis label
     ylab = "Duration of Run (hours)") #y-axis label
#Add the fitted regression line to the plot and color it red:
abline(my_model, col = "red")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q01a_residual_vs_x.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$Size, #x-variable
     my_model$residuals, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "darkred", #color of pch
     main = "Residuals vs. Size", #title of plot
     xlab = "Size of Lot Produced in Run (integer)", #x-axis label
     ylab = "Residuals") #y-axis label
#Add a horizontal line at zero, for visual reference:
abline(h=0)
dev.off() #Close the graphics device.

#1b:
#Add new column to our dataframe:
my_df$x_1 <- sqrt(my_df$Size)

#Fit the model with this different predictor:
sqrt_model <- lm(Duration ~ x_1, data = my_df)
summary(sqrt_model)

#Filename for upcoming plot:
png("hw03_q01b_scatter_sqrtx_y.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x_1, #x-variable
     my_df$Duration, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "blue", #color of pch
     main = "Scatter Plot of Duration vs. Sqrt(Size)", #title of plot
     xlab = "Sqrt(Size) of Lot Produced in Run", #x-axis label
     ylab = "Duration of Run (hours)") #y-axis label
#Add the fitted regression line to the plot and color it red:
abline(sqrt_model, col = "red")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q01b_residual_vs_sqrtx.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(my_df$x_1, #x-variable
     sqrt_model$residuals, #y-variable
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     col = "darkred", #color of pch
     main = "Residuals of Sqrt Model vs. Sqrt(Size)", #title of plot
     xlab = "Sqrt(Size) of Lot Produced in Run", #x-axis label
     ylab = "Residuals of Sqrt Model") #y-axis label
#Add a horizontal line at zero, for visual reference:
abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q01b_qqplot_sqrt.png",
    width = 800, height = 600, units = "px")
#Normal QQ-plot:
qqnorm(residuals(sqrt_model),
       pch = 16, col = "orange",
       main = "QQ-plot of Residuals of Sqrt Model",
       cex.main= 1.0)
qqline(residuals(sqrt_model), col = "black") #no departure from normality
dev.off() #Close the graphics device.

#1c:
#Apply a filter to find the row-number in the dataframe:
zero_size_row_number <- which(my_df$Size == 0)

#Ask the hat matrix of the model of interest for the value:
leverage_value <- hatvalues(my_model)[zero_size_row_number]
leverage_value

#1d:
#Create a column of row-numbers, prepended to be the first column:
my_df <- cbind(row_num = seq_len(nrow(my_df)), my_df)

#Filename for upcoming plot:
png("hw03_q01d_cooks_dist.png",
    width = 800, height = 600, units = "px")
#Scatter plot of the two variables:
plot(cooks.distance(my_model),
     typ = "b",
     pch = 16, #solid dot (plotting character)
     cex = 1.0, #size of pch
     main = "Cook's Distance") #title of plot
dev.off() #Close the graphics device.

#Gives the row-numbers of the five largest Cook's distances:
top_five_row_nums <- order(cooks.distance(my_model), decreasing = T)[1:5]

#Add a column to store the Cook's distances:
my_df$my_model_cooks <- cooks.distance(my_model)

#Pulls the details for those five index-values:
my_df[my_df$row_num %in% top_five_row_nums,c("Size","Duration", "my_model_cooks")]
