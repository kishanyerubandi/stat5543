#1a, 1b, 1c, 1d:
input_path <- "06_Data_from_Professor/Data pack 2/GASTURBINE.csv"
my_df <- transform(
          read.table(input_path, sep = ",", header=TRUE),
          ENGINE = factor(ENGINE),
          SHAFTS = factor(SHAFTS))

model_A <- lm(HEATRATE ~ RPM + INLETTEMP + EXHTEMP + CPRATIO
               + factor(ENGINE) + factor(SHAFTS), data=my_df)
summary(model_A)
summary(model_A)$coefficients #For more decimal places in the coefficients.

#1e:
model_B <- lm(HEATRATE ~ RPM + INLETTEMP + EXHTEMP + CPRATIO
                + relevel(ENGINE, ref = "Traditional")
                + factor(SHAFTS), data=my_df)
summary(model_B)

#1f:
model_C <- lm(HEATRATE ~ RPM + INLETTEMP + EXHTEMP + CPRATIO
               + factor(ENGINE), data=my_df)
anova(model_C, model_A)

#1g:
my_turbine <- data.frame(RPM=7000, INLETTEMP=1000,
        EXHTEMP=500, CPRATIO=17, ENGINE="Traditional",
        SHAFTS="1")
predict(model_A, my_turbine, interval = "confidence", level = 0.99)

#2a:
y <- my_df$HEATRATE #Vector of dataset's response values.
ybar <- mean(y) #Average of dataset's response values.
SS_Total <- sum((y-ybar)^2)
R_squared <- summary(model_A)$r.squared 
SS_Regression <- SS_Total * R_squared
SS_Regression

#2b:
model_D <- lm(HEATRATE ~ RPM + factor(ENGINE)
              + factor(SHAFTS), data=my_df)
anova(model_D)

#2c:
model_E <- lm(HEATRATE ~ RPM + factor(SHAFTS),
              data=my_df)
anova(model_E)

#2d:
model_F <- lm(HEATRATE ~ RPM, data=my_df)
anova(model_F, model_D)
