

####Part A----
# Required packages
required_packages <- c("tidyverse", "readxl", "viridisLite", "scales", "ggplot2", "lme4", "datasets", "knitr", "quarto", "tinytex")

# Install required packages if not already installed
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load required packages
for (package in required_packages) {
  library(package, character.only = TRUE)
}

# Set the file path to get data 
file_path <- "Data/Dataset_S1.csv"

# Read the CSV file
data <- read_delim(file_path, col_types = cols(.default = "character"), delim = ",")

####I want to create the Figure 3, with biomass and eveness, using dissimilarity. 

#Converting variables to numeric so that it works for the plot 
data$evesimpson <- as.numeric(data$evesimpson)
data$jac <- as.numeric(data$jac)
data$biomass <- as.numeric(data$biomass) 

#Plotting the scatterplot 
plot <- ggplot(data, aes(x = evesimpson, y = biomass/1000000, color = jac)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.05, 0.1, 0.2)) +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.00, 0.05, 0.5, 2.5),
    labels = c(0.00, 0.05, 0.5, 2.5)
  ) +
  labs(color = "Dissimilarity") +
  scale_color_gradientn(
    colors = colorRampPalette(c("blue", "beige", "brown"))(6),
    breaks = levels(data$jac),
    labels = levels(data$jac)
  ) +
  xlab("evenness") +
  ylab("biomass") +
  theme(    # Axis labels and theme customization
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.line.x = element_line(arrow = arrow(type = "open", ends = "last", length = unit(0.5, "cm"), angle = 20)),
    axis.line.y = element_line(arrow = arrow(type = "open", ends = "last", length = unit(0.5, "cm"), angle = 20)),
    axis.text = element_text(size = 12),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(color = guide_colorbar(title = "Dissimilarity",     # Customize color legend guide
                                direction = "horizontal",
                                barwidth = 10, 
                                barheight = 1))


# Display the plot
print(plot)


####Part B----

###Simulating the data to show the problem 
set.seed(123)  # Setting seed for reproducibility

# Simulating the data
treatment <- rep(c("A", "B"), each = 50)
tank <- rep(1:5, times = 20)
fish <- rep(1:10, times = 10)
growth <- rnorm(100, mean = ifelse(treatment == "A", 10, 12), sd = 2)

# Creating the data frame
fish_data <- data.frame(treatment, tank, fish, growth)

# Fitting the initial model
initial_model <- lm(growth ~ treatment, data = fish_data)

# Displaying the model summary
summary(initial_model)

#Coefficients:
#                   Estimate     Std. Error   t value     Pr(>|t|)    
#      (Intercept)  10.0688     0.2590       38.875      < 2e-16 ***
#     treatmentB    2.2240     0.3663      6.072        2.4e-08 ***


###Give a better model. Mixed effects model 
mixed_model <- lmer(growth ~ treatment + (1 | tank), data = fish_data)
#summary
summary(mixed_model)


#loading in the dataset lynx fro mthe dataseries package 
data("lynx")
lynx_ts <- ts(lynx, start = 1821, end = 1934, frequency = 1)

#creating the plot 
ggplot() +
  geom_line(aes(x = time(lynx_ts), y = lynx_ts), color = "#1f78b4", linewidth = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Number of Lynx",
       title = "Number of Lynx Trapped in Canada (1821-1934)",
       plot.caption = "Data Source: Lynx Dataset") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_blank())                          
                                                                                      
###examine the acf and pacf

# Calculate the ACF and PACF
acf_result <- acf(lynx_ts, lag.max = 30)
pacf_result <- pacf(lynx_ts, lag.max = 30)

# Plot the ACF
plot(acf_result, main = "ACF of Lynx Data")

# Plot the PACF
plot(pacf_result, main = "PACF of Lynx Data")


#### 4 Chironomid species richness ----
# Set the file path
file_path <- "chironomid.txt"

# Read the TXT file
data <- read.table(file_path, header = TRUE)

# Convert it into a dataframe
df <- as.data.frame(data)
show(df)


###What type of analyses is appropriate? 
#first we can explore the data

#Exploratory Data Analysis
# Summary statistics
summary(df)
# Scatterplot matrix
pairs(df)
# Correlation matrix
cor(df)


###Fit the multiple regression model
model <- lm(noSpecies ~ temperature + pH + depth, data = df)

# Summary of the model
summary(model)

# Plotting the residuals vs. fitted values
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")

# Checking normality of residuals using a QQ plot
qqnorm(model$residuals)
qqline(model$residuals)

# Heteroscedasticity - Residuals vs. Fitted Values
plot(model$fitted.values, abs(model$residuals), xlab = "Fitted Values", ylab = "Absolute Residuals", main = "Residuals vs. Fitted Values")


#log transforming the repons 
# Apply logarithmic transformation to the response variable
df$log_noSpecies <- log(df$noSpecies)

# Fit the multiple regression model with the transformed response variable
model <- lm(log_noSpecies ~ temperature + pH + depth, data = df)

# Summary of the model
summary(model)

# Residual analysis

# Residuals vs. Fitted Values
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")

# Normality of Residuals - QQ Plot
qqnorm(model$residuals)
qqline(model$residuals)

# Heteroscedasticity - Residuals vs. Fitted Values
plot(model$fitted.values, abs(model$residuals), xlab = "Fitted Values", ylab = "Absolute Residuals", main = "Residuals vs. Fitted Values")

###Go back to the origianl model 
model <- lm(noSpecies ~ temperature + pH + depth, data = df)









###Predic speceiss richness 

# Temperature values for prediction
new_temperatures <- c(-5, 5, 30)

# Create a new data frame for prediction
new_data <- data.frame(temperature = new_temperatures, pH = mean(df$pH), depth = mean(df$depth))

# Predict species richness
predictions <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)

# Create a data frame with predictions and confidence intervals
results <- data.frame(Temperature = new_temperatures, 
                      SpeciesRichness = predictions[, "fit"],
                      LowerCI = predictions[, "lwr"],
                      UpperCI = predictions[, "upr"])

# Print the results
print(results)

# Plotting the results
plot(results$Temperature, results$SpeciesRichness, type = "l", lwd = 2,
     xlab = "Temperature", ylab = "Species Richness",
     ylim = c(min(results$LowerCI), max(results$UpperCI)), 
     main = "Predicted Species Richness at Different Temperatures")
# Adding confidence intervals as error bars
lines(results$Temperature, results$LowerCI, lty = 2, col = "blue")
lines(results$Temperature, results$UpperCI, lty = 2, col = "blue")
segments(results$Temperature, results$LowerCI, results$Temperature, results$UpperCI, lwd = 2)
# Adding legend for confidence intervals
legend("topleft", legend = "95% Confidence Interval", lty = 2, col = "blue")


# Create a data frame with predictions and confidence intervals
results_table <- data.frame(Temperature = new_temperatures,
                      SpeciesRichness = predictions[, "fit"],
                      LowerCI = predictions[, "lwr"],
                      UpperCI = predictions[, "upr"])

# Print the results table using 'kable'
kable(results_table, caption = "Predicted Species Richness at Different Temperatures")


