#2b:
input_path <- "06_Data_from_Professor/Data pack 2/TAMSALES4.csv"
my_df <- read.table(input_path, sep = ",", header=TRUE)

my_df$NBHD <- trimws(my_df$NBHD) #Remove whitespace.
my_df$NBHD <- factor(my_df$NBHD) #Convert to factor.

my_model <- lm(SALES ~ LAND*factor(NBHD) + IMP*factor(NBHD), data = my_df)
summary(my_model)

#2e:
my_property <- data.frame(LAND=50,IMP=180,NBHD="HUNTERSGREEN")
predict(my_model, my_property)
