input_path <- "Nutrition.csv"
dat <- read.table(input_path, sep = ",", header=TRUE)

# Sex: Male as reference
dat$Sex <- relevel(factor(dat$Sex), ref = "Male")

# VitaminUse: ordered levels with "No" as reference
dat$VitaminUse <- factor(dat$VitaminUse,
                         levels = c("No", "Occasional", "Regular"))

# PriorSmoke: factor with labels
dat$PriorSmoke <- factor(dat$PriorSmoke,
                         levels = c(1, 2, 3),
                         labels = c("Never", "Former", "Current"))

summary(dat$BetaPlasma)
dat$TransfBetaPlasma <- log(dat$BetaPlasma + 0.1)
summary(dat$TransfBetaPlasma)

png("hw_final_project_01-02a.png",
    width = 800, height = 600, units = "px")
par(mfrow = c(1, 2),
    mar = c(4, 4, 3, 1))

hist(dat$BetaPlasma,
     main = "Untransformed BetaPlasma",
     xlab = "BetaPlasma (ng/ml)",
     breaks = 7)

boxplot(dat$BetaPlasma,
        main = "Untransformed BetaPlasma",
        ylab = "BetaPlasma (ng/ml)")
dev.off() #Close the graphics device.

png("hw_final_project_01-02b.png",
    width = 800, height = 600, units = "px")
par(mfrow = c(1, 2),
    mar = c(4, 4, 3, 1))

hist(log(dat$BetaPlasma + 0.1),
     main = "Transformed BetaPlasma",
     xlab = "log(BetaPlasma + 0.1)",
     breaks = 5)

boxplot(log(dat$BetaPlasma + 0.1),
     main = "Transformed BetaPlasma",
        ylab = "log(BetaPlasma + 0.1)",
     breaks = my_breaks)
dev.off() #Close the graphics device.

#Check for NA's:
colSums(is.na(dat))

numeric_vars <- c("Age", "Quetelet", "Calories", "Fat",
              "Fiber", "Alcohol", "Cholesterol", "BetaDiet")

summary(dat[, numeric_vars])

png("hw_final_project_01-03a.png",
    width = 800, height = 600, units = "px")

par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

for (var in num_vars) {
  hist(dat[[var]],
       main = var,
       xlab = var)
}
dev.off() #Close the graphics device.

png("hw_final_project_01-03b.png",
    width = 800, height = 600, units = "px")

par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))

for (var in num_vars) {
  boxplot(dat[[var]],
       main = var,
       xlab = var)
}
dev.off() #Close the graphics device.

categ_vars <- c("Sex", "VitaminUse", "PriorSmoke")

png("hw_final_project_01-03c.png",
    width = 800, height = 600, units = "px")

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

for (var in categ_vars) {
  counts <- table(dat[[var]])
  barplot(counts,
          main = var,
          ylab = "Count")
}
dev.off() #Close the graphics device.

png("hw_final_project_01-03d.png",
    width = 800, height = 600, units = "px")
pairs(dat[ , numeric_vars])
dev.off() #Close the graphics device.

full_main <- lm(TransfBetaPlasma ~ Age + Quetelet + Calories + Fat + Fiber +
                  Alcohol + Cholesterol + BetaDiet +
                  Sex + VitaminUse + PriorSmoke,
                data = dat)
summary(full_main)

resid_std <- rstandard(full_main)

png("hw_final_project_qq01.png",
    width = 800, height = 600, units = "px")

qqnorm(resid_std, main = "Normal Q-Q Plot of Standardized Residuals")
qqline(resid_std, col = "red")
dev.off() #Close the graphics device.

png("hw_final_project_cooks01.png",
    width = 800, height = 600, units = "px")
plot(full_main, 4)
dev.off() #Close the graphics device.

#Remove the outlier with a large Cook's distance:
dat <- dat[-257, ]

fit_0 <- lm(TransfBetaPlasma ~ 1, data = dat)
step(fit_0, TransfBetaPlasma ~  Age + Quetelet + Calories + Fiber +
                  Alcohol + Cholesterol + BetaDiet +
                  Sex + VitaminUse + PriorSmoke,
                  direction = "both", trace = 0)

#fit_main is the output of step() above.
fit_main <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + Quetelet
                        + VitaminUse + BetaDiet + Age, data = dat)
summary(fit_main)

step(fit_main, scope = .~.^2, direction = "both", trace = 0)

#final_model is the output of step() above, which included interactions.
final_model <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + Quetelet + 
    VitaminUse + BetaDiet + Age + VitaminUse:BetaDiet + Cholesterol:Age + 
    Cholesterol:VitaminUse + BetaDiet:Age, data = dat)
summary(final_model)

#Cook's distance:
plot(final_model, 4)

#Hypothesis testing via partial F-tests:

#testing Age
reduced_model <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + Quetelet + 
    VitaminUse + BetaDiet + VitaminUse:BetaDiet + 
    Cholesterol:VitaminUse, data = dat)
anova(reduced_model, final_model)

#testing Quetelet
reduced_model <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + 
    VitaminUse + BetaDiet + Age + VitaminUse:BetaDiet + Cholesterol:Age + 
    Cholesterol:VitaminUse + BetaDiet:Age, data = dat)
anova(reduced_model, final_model)

#testing Fiber
reduced_model <- lm(TransfBetaPlasma ~ Cholesterol + Quetelet + 
    VitaminUse + BetaDiet + Age + VitaminUse:BetaDiet + Cholesterol:Age + 
    Cholesterol:VitaminUse + BetaDiet:Age, data = dat)
anova(reduced_model, final_model)

#testing Cholesterol
reduced_model <- lm(TransfBetaPlasma ~ Fiber + Quetelet + 
    VitaminUse + BetaDiet + Age + VitaminUse:BetaDiet
    + BetaDiet:Age, data = dat)
anova(reduced_model, final_model)

#testing BetaDiet
reduced_model <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + Quetelet + 
    VitaminUse + Age + Cholesterol:Age + 
    Cholesterol:VitaminUse, data = dat)
anova(reduced_model, final_model)

#testing VitaminUse
reduced_model <- lm(TransfBetaPlasma ~ Cholesterol + Fiber + Quetelet
    + BetaDiet + Age + Cholesterol:Age
    + BetaDiet:Age, data = dat)
anova(reduced_model, final_model)

