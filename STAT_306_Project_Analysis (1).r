library(readr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(leaps, quietly = TRUE)
library(car, quietly = TRUE)

vehicle_emissions <- read_csv("data/vehicle_emissions.csv")

# Renaming the variables for convenience while keeping readability
colnames(vehicle_emissions) <- c("Model_Year", "Make", "Model", "Vehicle_Class", "Engine_Size_L", 
                                 "Cylinders", "Transmission", "Fuel_Type", "City_L_100km", 
                                 "Highway_L_100km", "Combined_L_100km", "Combined_MPG", 
                                 "CO2_Emissions_g_km", "CO2_Rating", "Smog_Rating")

head(vehicle_emissions, 5)
tail(vehicle_emissions, 5)

# Checking for NAs
na_count <- colSums(is.na(vehicle_emissions))
na_count

# Summary statistics
summary(vehicle_emissions[, c("Engine_Size_L", "Cylinders", "City_L_100km", "Highway_L_100km", "Combined_L_100km", "CO2_Emissions_g_km")])


# Converting categorical variables into factors for use in linear regression models
vehicle_emissions$Model_Year <- factor(vehicle_emissions$Model_Year)
vehicle_emissions$Make <- factor(vehicle_emissions$Make)
vehicle_emissions$Model <- factor(vehicle_emissions$Model)

vehicle_emissions$Vehicle_Class <- factor(vehicle_emissions$Vehicle_Class)
vehicle_emissions$Cylinders <- factor(vehicle_emissions$Cylinders)
vehicle_emissions$Transmission <- factor(vehicle_emissions$Transmission)
vehicle_emissions$Fuel_Type <- factor(vehicle_emissions$Fuel_Type)

vehicle_emissions$CO2_Rating <- factor(vehicle_emissions$CO2_Rating)
vehicle_emissions$Smog_Rating <- factor(vehicle_emissions$Smog_Rating)


summary_stats <- summary(vehicle_emissions[c("Engine_Size_L", "City_L_100km", "Highway_L_100km", "Combined_L_100km", "Combined_MPG", "CO2_Emissions_g_km")])
print(summary_stats)

options(repr.plot.width = 15, repr.plot.height = 9)

par(mfrow=c(3,3)) # Adjust layout to fit all histograms

hist(vehicle_emissions$Engine_Size_L, main="Engine Size (L)", xlab="Engine Size (L)", col="blue")
hist(vehicle_emissions$City_L_100km, main="City L/100km", xlab="City L/100km", col="green")
hist(vehicle_emissions$Highway_L_100km, main="Highway L/100km", xlab="Highway L/100km", col="yellow")
hist(vehicle_emissions$Combined_L_100km, main="Combined L/100km", xlab="Combined L/100km", col="orange")
hist(vehicle_emissions$Combined_MPG, main="Combined MPG", xlab="Combined MPG", col="purple")
hist(vehicle_emissions$CO2_Emissions_g_km, main="CO2 Emissions (g/km)", xlab="CO2 Emissions (g/km)", col="brown")

# Reset layout
par(mfrow=c(1,1))


par(mfrow=c(3,3)) # Adjust layout to fit all box plots

boxplot(vehicle_emissions$Engine_Size_L, main="Engine Size (L)", col="blue")
boxplot(vehicle_emissions$City_L_100km, main="City L/100km", col="green")
boxplot(vehicle_emissions$Highway_L_100km, main="Highway L/100km", col="yellow")
boxplot(vehicle_emissions$Combined_L_100km, main="Combined L/100km", col="orange")
boxplot(vehicle_emissions$Combined_MPG, main="Combined MPG", col="purple")
boxplot(vehicle_emissions$CO2_Emissions_g_km, main="CO2 Emissions (g/km)", col="brown")

# Reset layout
par(mfrow=c(1,1))


options(repr.plot.width = 14, repr.plot.height = 9)

continuous_vars <- vehicle_emissions[, c("Engine_Size_L", "City_L_100km", "Highway_L_100km", "Combined_L_100km", "Combined_MPG", "CO2_Emissions_g_km")]
pairs(continuous_vars, main = "Scatter Plot Matrix")

# Make the correlation matrix with only continuous variables
correlation_matrix <- cor(continuous_vars)
melted_cor_matrix <- melt(correlation_matrix)

ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(color = "black", size = 8) +
  theme_minimal() +
  labs(title = "Correlation Matrix of Continuous Variables",
       x = "Variables",
       y = "Variables")

# Count the number of occurrences for each category
list_of_categorical_vars <- list(vehicle_emissions$Model_Year, vehicle_emissions$Make, vehicle_emissions$Model,
                                 vehicle_emissions$Vehicle_Class, vehicle_emissions$Cylinders, vehicle_emissions$Transmission,
                                 vehicle_emissions$Fuel_Type, vehicle_emissions$CO2_Rating, vehicle_emissions$Smog_Rating)

# Use lapply to apply the table function to each categorical variable
category_counts <- lapply(list_of_categorical_vars, table)

# Print out the counts for each variable
category_counts


# Layout for box plots
par(mfrow=c(2,2), mar=c(5, 8, 4, 2) + 0.1)

# Box plot for Vehicle_Class vs CO2_Emissions_g_km
# boxplot(CO2_Emissions_g_km ~ Vehicle_Class, data=vehicle_emissions, main="CO2 Emissions by Vehicle Class", horizontal=TRUE, las=1, col="blue")

boxplot(CO2_Emissions_g_km ~ Vehicle_Class, data=vehicle_emissions, 
        main="CO2 Emissions by Vehicle Class", 
        horizontal=TRUE, las=1, col="blue", 
        cex.axis=0.7, # Scale down the text size for axis labels
        cex.lab=0.7, # Scale down the text size for axis title
        names.arg=sapply(vehicle_emissions$Vehicle_Class, function(x) substring(x, 1, 20)) # Shorten category names if needed
)

# Box plot for Cylinders vs CO2_Emissions_g_km
boxplot(CO2_Emissions_g_km ~ Cylinders, data=vehicle_emissions, main="CO2 Emissions by Cylinders", horizontal=TRUE, las=1, col="red")

# Box plot for Transmission vs CO2_Emissions_g_km
boxplot(CO2_Emissions_g_km ~ Transmission, data=vehicle_emissions, main="CO2 Emissions by Transmission", horizontal=TRUE, las=1, col="green")

# Box plot for Fuel_Type vs CO2_Emissions_g_km
boxplot(CO2_Emissions_g_km ~ Fuel_Type, data=vehicle_emissions, main="CO2 Emissions by Fuel Type", horizontal=TRUE, las=1, col="yellow")

# Reset the layout
par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)




# Boxplot for Make only
boxplot(CO2_Emissions_g_km ~ Make, data=vehicle_emissions, main="CO2 Emissions by Make", las=2, col="blue", outline=FALSE)

data <- select(vehicle_emissions, 
               -c(City_L_100km, Highway_L_100km, Model_Year, Model, Smog_Rating, CO2_Rating))
head(data)

luxury_makes <- c("Acura", "Alfa Romeo", "Aston Martin", "Audi", "Bentley", "BMW", "Bugatti",
                  "Cadillac", "Ferrari", "Genesis", "Infiniti", "Jaguar", "Lamborghini",
                  "Land Rover", "Lexus", "Lincoln", "Maserati", "Mercedes-Benz", "Porsche",
                  "Rolls-Royce", "Volvo")

non_luxury_makes <- c("Buick", "Chevrolet", "Chrysler", "Dodge", "Ford", "GMC", "Honda", 
                      "Hyundai", "Jeep", "Kia", "Mazda", "MINI", "Nissan", "Ram", "Subaru", 
                      "Toyota", "Mitsubishi", "Volkswagen")

vehicle_emissions_mutated <- vehicle_emissions %>%
  select(-c(City_L_100km, Highway_L_100km, Model_Year, Model, Smog_Rating, CO2_Rating)) %>%
  mutate(Transmission = ifelse(grepl("A", Transmission), "A", 
                               ifelse(grepl("M", Transmission), "M", Transmission)),
         Make = ifelse(Make %in% luxury_makes, "Luxury", 
                       ifelse(Make %in% non_luxury_makes, "Non-Luxury", NA)))

head(vehicle_emissions_mutated)


table(vehicle_emissions_mutated$Make)
table(vehicle_emissions_mutated$Transmission)

# Boxplot for Luxury vs Non-Luxury
ggplot(vehicle_emissions_mutated, aes(x = Make, y = CO2_Emissions_g_km, fill = Make)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions by Make Category", x = "Make Category", y = "CO2 Emissions (g/km)") +
  scale_fill_manual(values = c("Luxury" = "gold", "Non-Luxury" = "steelblue")) +
  theme_minimal()

# Boxplot for A vs M Transmission Types
ggplot(vehicle_emissions_mutated, aes(x = Transmission, y = CO2_Emissions_g_km, fill = Transmission)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions by Transmission Type", x = "Transmission Type", y = "CO2 Emissions (g/km)") +
  scale_fill_manual(values = c("A" = "darkorange", "M" = "dodgerblue")) +
  theme_minimal()


full_model <- lm(formula = CO2_Emissions_g_km ~ ., data = vehicle_emissions_mutated)
summary(full_model)

# Assuming you have a full model called full_model
full_model <- lm(CO2_Emissions_g_km ~ ., data = vehicle_emissions_mutated)

# Plot the residuals vs fitted values
plot(full_model$fitted.values, resid(full_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")

# Add a horizontal line at 0 for reference
abline(h = 0, col = "red")

vif_results <- vif(full_model)
print(vif_results)

aic <- AIC(full_model)
aic

# Backward Elimination
backward_elimination <- regsubsets(CO2_Emissions_g_km ~ ., data = vehicle_emissions_mutated, method = "backward", nvmax = 20)
regsubsets_summary <- summary(backward_elimination)
regsubsets_summary

bic_values <- regsubsets_summary$bic
cp_values <- regsubsets_summary$cp
adjr2_values <- regsubsets_summary$adjr2

# Printing the values
print("BIC Values:")
print(bic_values)
print("Cp Values:")
print(cp_values)
print("Adjusted R-squared Values:")
print(adjr2_values)

model_sizes <- seq_along(regsubsets_summary$cp)
plot_data <- data.frame(ModelSize = model_sizes, Cp = cp_values, AdjR2 = adjr2_values, BIC = bic_values)

# Plotting Cp values with integer x-axis margins
ggplot(plot_data, aes(x = ModelSize, y = Cp)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Mallows' Cp by Number of Parameters") +
  xlab("Number of Parameters") +
  ylab("Cp") +
  scale_x_continuous(breaks = seq(min(plot_data$ModelSize), max(plot_data$ModelSize), by = 1))  # Integer breaks


# Plotting Adjusted R-squared values with integer x-axis margins
ggplot(plot_data, aes(x = ModelSize, y = AdjR2)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Adjusted R-squared by Number of Parameters") +
  xlab("Number of Parameters") +
  ylab("Adjusted R-squared") +
  scale_x_continuous(breaks = seq(min(plot_data$ModelSize), max(plot_data$ModelSize), by = 1))  # Integer breaks


# Plotting BIC values with integer x-axis margins
ggplot(plot_data, aes(x = ModelSize, y = BIC)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("BIC by Number of Parameters") +
  xlab("Number of Parameters") +
  ylab("BIC") +
  scale_x_continuous(breaks = seq(min(plot_data$ModelSize), max(plot_data$ModelSize), by = 1))  # Integer breaks


# Fitting the model with the specified variables
Cylinders_10 <- factor(vehicle_emissions_mutated$Cylinders=='10')
Cylinders_12 <- factor(vehicle_emissions_mutated$Cylinders=='12')
Vehicle_Class_StationSmall <- factor(vehicle_emissions_mutated$Vehicle_Class=='Station wagon: Small')
Vehicle_Class_Minicompact <- factor(vehicle_emissions_mutated$Vehicle_Class=='Minicompact')

model_10 <- lm(CO2_Emissions_g_km ~ Fuel_Type + Combined_L_100km + 
                Combined_MPG + Cylinders_10 + Cylinders_12 + Transmission + 
                Vehicle_Class_StationSmall + Vehicle_Class_Minicompact, 
                data = vehicle_emissions_mutated)

summary(model_10)


# Calculating AIC
aic_value <- AIC(model_10)

# Calculating BIC
bic_value <- BIC(model_10)

# Printing the AIC and BIC values
print(paste("AIC:", aic_value))
print(paste("BIC:", bic_value))


# Residuals vs Fitted
plot(model_10$fitted.values, resid(model_10), xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Normal Q-Q Plot
qqnorm(resid(model_10))
qqline(resid(model_10), col="red")

model_interactions <- lm(CO2_Emissions_g_km ~ Fuel_Type + Vehicle_Class_StationSmall + Vehicle_Class_Minicompact +
                         Cylinders_10 + Transmission +
                         Cylinders_12 * Transmission +
                         Combined_MPG * Vehicle_Class_StationSmall + Combined_MPG +
                         Combined_L_100km, 
                         data = vehicle_emissions_mutated)

summary(model_interactions)


# Assuming model_10 is already fitted
#Calculate Cook's distance for model_10
cooks_dist <- cooks.distance(model_10)

#Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance", ylab="Cook's Distance")
abline(h = 4/length(cooks_dist), col = "red")  # Rule of thumb threshold line

#Identify points with high Cook's distance
influential_points <- which(cooks_dist > (4/length(cooks_dist)))
influential_points

#Plot leverage values
plot(hatvalues(model_10), main="Hat Values (Leverage)", ylab="Leverage")
abline(h = 2*mean(hatvalues(model_10)), col="red") 

#Calculate standardized residuals
std_resid <- rstandard(model_10)
large_std_resid_points <- which(abs(std_resid) > 2)

#Combine points identified by both criteria
all_influential_points <- union(influential_points, large_std_resid_points)
head(vehicle_emissions_mutated[all_influential_points, ])
cleaned_data <- vehicle_emissions_mutated[-all_influential_points, ]


# Refit the model with cleaned data
cleaned_data$Cylinders_10 <- factor(cleaned_data$Cylinders == '10')
cleaned_data$Cylinders_12 <- factor(cleaned_data$Cylinders == '12')
cleaned_data$Vehicle_Class_StationSmall <- factor(cleaned_data$Vehicle_Class == 'Station wagon: Small')
cleaned_data$Vehicle_Class_Minicompact <- factor(cleaned_data$Vehicle_Class == 'Minicompact')

model_10_clean <- lm(CO2_Emissions_g_km ~ Fuel_Type + Combined_L_100km + 
                     Combined_MPG + Cylinders_10 + Cylinders_12 + Transmission + 
                     Vehicle_Class_StationSmall + Vehicle_Class_Minicompact, 
                     data = cleaned_data)

summary(model_10_clean)


# Residuals vs Fitted
plot(model_10_clean$fitted.values, resid(model_10_clean), xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Normal Q-Q Plot
qqnorm(resid(model_10_clean))
qqline(resid(model_10_clean), col="red")


