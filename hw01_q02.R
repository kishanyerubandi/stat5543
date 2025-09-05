data_path = "06_Data_from_Professor/Data pack 1/natality.csv"
natality_df <- read.csv(data_path, header=TRUE)

#2a:
nrow(natality_df)

#2b:
numerator <- sum(is.na(natality_df))
denominator <- nrow(natality_df) * ncol(natality_df)
proportion <- numerator/denominator
proportion

#2c:
#Filename for upcoming plot:
png("hw01_q02c_birthweight_gestation_data.png",
    width = 800, height = 600, units = "px")

plot(BIRTHWT ~ GESTAGE, #y-variable ~ x-variable
     data = natality_df, #source dataset
     col = "blue", #color of pch
     pch = 16, #solid dot (plotting character)
     cex = 0.8, #size of pch
     cex.lab = 0.75, #size of labels on axes
     cex.axis = 0.75, #size of numbers on axes
     xlab = "Gestational Age (weeks)", #xaxis label
     ylab = "Birth Weight of Babies (grams)", #yaxis label
     main = "Birth Weight of Babies vs. Gestational Age" #title
)

#We use which() rather than subset() since it drops NA's.
#Filter for the underweight babies and then the non-underweight:
underweight_df <- natality_df[which(natality_df$BIRTHWT < 2500), ]
normalweight_df <- natality_df[which(natality_df$BIRTHWT >= 2500), ]

# Now add these specific points to the existing plot
points(underweight_df$GESTAGE, #x-variable
       underweight_df$BIRTHWT, #y-variable
       col = "red", #color of pch
       pch = 16, #solid dot (plotting character)
       cex = 0.8, #size of pch
)

legend("topleft", 
       legend = c("Non-Underweight Weight", "Underweight"),
       col = c("blue", "red"),
       pch = 16,
       cex = 0.8 #size of legend text
)

dev.off() #Close the graphics device.

#2d
#Fit model for the underweight babies and then the non-underweight:
model_underweight <- lm(BIRTHWT ~ GESTAGE, data = underweight_df)
model_normalweight <- lm(BIRTHWT ~ GESTAGE, data = normalweight_df)

#Print the two models:
summary(model_underweight)
summary(model_normalweight)
