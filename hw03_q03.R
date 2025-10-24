#3a:
input_path <- "06_Data_from_Professor/Data pack 1/Triglycerides.csv"
my_df <- read.table(input_path, header = TRUE, sep=",",
                    stringsAsFactors = TRUE)

my_model <- lm(Trig ~ Trunkpct + Age + factor(Gender),data=my_df)
summary(my_model)

#3b:
#Filename for upcoming plot:
png("hw03_q03b_scatter_matrix.png",
    width = 800, height = 600, units = "px")
pairs(my_df[c("Trig","Trunkpct","Age")], pch = 16,
      main = "Matrix of Scatters for
      Non-Categorical Variables")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_trunkpct.png",
    width = 800, height = 600, units = "px")
plot(my_df$Trunkpct, my_model$residuals, pch=16,
     xlab="Trunkpct", ylab="Residuals",
     main="Residuals vs. Trunkpct"); abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_age.png",
    width = 800, height = 600, units = "px")
plot(my_df$Age, my_model$residuals, pch=16,
     xlab="Age", ylab="Residuals",
     main="Residuals vs. Age"); abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_gender.png",
    width = 800, height = 600, units = "px")
boxplot(my_model$residuals ~ my_df$Gender,
        xlab="Gender", ylab="Residuals",
        main="Residuals by Gender")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_qqplot.png",
    width = 800, height = 600, units = "px")
qqnorm(my_model$residuals,
       main="QQ plot of Residuals")
qqline(residuals(my_model), col = "black")
dev.off() #Close the graphics device.

#3c:
new_model <- lm(log(Trig) ~ Trunkpct + Age + factor(Gender),data=my_df)
summary(new_model)

my_df$LogTrig <- log(my_df$Trig)

#Filename for upcoming plot:
png("hw03_q03b_scatter_matrix_new_model.png",
    width = 800, height = 600, units = "px")
pairs(my_df[c("LogTrig","Trunkpct","Age")], pch = 16,
      main = "Matrix of Scatters for
      Non-Categorical Variables in New Model")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_trunkpct_new_model.png",
    width = 800, height = 600, units = "px")
plot(my_df$Trunkpct, new_model$residuals, pch=16,
     xlab="Trunkpct", ylab="Residuals in New Model",
     main="Residuals vs. Trunkpct"); abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_age_new_model.png",
    width = 800, height = 600, units = "px")
plot(my_df$Age, new_model$residuals, pch=16,
     xlab="Age", ylab="Residuals in New Model",
     main="Residuals vs. Age"); abline(h=0)
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_residuals_gender_new_model.png",
    width = 800, height = 600, units = "px")
boxplot(new_model$residuals ~ my_df$Gender,
        xlab="Gender", ylab="Residuals in New Model",
        main="Residuals by Gender")
dev.off() #Close the graphics device.

#Filename for upcoming plot:
png("hw03_q03b_qqplot_new_model.png",
    width = 800, height = 600, units = "px")
qqnorm(new_model$residuals,
       main="QQ plot of Residuals in New Model")
qqline(residuals(new_model), col = "black")
dev.off() #Close the graphics device.

#3d:
exp(0.48334)-1
