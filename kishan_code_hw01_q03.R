library(carData)
data(Highway1)

#3b:
accident_rate_model <- lm(rate ~ slim + shld + acpt, data = Highway1)
summary(accident_rate_model)

#3c:
#Filename for upcoming plot:
png("hw01_q03c_car_accident_highway_data.png",
    width = 800, height = 600, units = "px")

#Setting display to be a 1x3 grid of plots:
par(mfrow = c(1, 3))

#y vs. x_1
plot(rate ~ slim, data = Highway1,
     xlab = "Speed Limit (mph)",
     ylab = "Accident Rate (per million vehcile miles)",
     main = "Accident Rate vs. Speed Limit",
     pch = 16,
     col = "red")

#y vs. x_2
plot(rate ~ shld, data = Highway1,
     xlab = "Shoulder Width (ft)",
     ylab = "Accident Rate (per million vehcile miles)",
     main = "Accident Rate vs. Shoulder Width",
     pch = 16,
     col = "blue")

#y vs. x_3
plot(rate ~ acpt, data = Highway1,
     xlab = "Number of Access Points (per mile)",
     ylab = "Accident Rate (per million vehcile miles)",
     main = "Accident Rate vs. Number of Access Points",
     pch = 16,
     col = "green")

dev.off() #Close the graphics device.
