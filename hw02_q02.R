library(carData)
data(Anscombe)

#2a:
car_model <- lm(income ~ education + young + urban, data = Anscombe)
summary(car_model)

#2b:
#Filename for upcoming plot:
png("hw02_q02b_anscombe.png",
    width = 800, height = 600, units = "px")
#Matrix of scatter plots for two variables at a time:
pairs(Anscombe, main = "Anscombe Data")
dev.off() #Close the graphics device.


#2e
rownames(Anscombe)[which.max(Anscombe$education)]

rownames(Anscombe)[which.max(Anscombe$young)]
