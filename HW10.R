##Created by Samantha Summerfield and Maggie Phillips
##11/6/25
##HW 10


##-----------------------Objective 1-----------------------------##


#this data set is from Sebago Lake Water Monitoring (https://www.pwd.org/sebago-lake-monitoring-buoy/)
#I worked with this in undergraduate
#I didn't have any of my own data to use so I used this set
#data of DO and temperature at 7 feet lake depth in May 2024

#load libraries

library("ggplot2")
library("ggfortify")

#read in the data
data = readxl::read_excel("sebago-lake-DO-temp-2024-data.xlsx")

DO_temp = data.frame(cbind(data$`DO mg/L`, data$`Temp (F)`))

names(DO_temp)[1]<-"DO"
names(DO_temp)[2]<-"Temp"

DO_temp <- subset(DO_temp, DO > 0 & Temp > 0)

regression_line = lm(DO_temp$DO ~ DO_temp$Temp)
print(regression_line)

ggplot(data = DO_temp, mapping = aes(x = `Temp`, y = `DO`)) + 
  geom_point() +
  geom_smooth(method = "lm")

regression_line = lm(DO_temp$DO ~ DO_temp$Temp, data = DO_temp)
print(regression_line)


#------C-------#

###Evaluate the model residuals for signs that regression assumptions are violated. You should evaluate
###at least three assumptions and for each one state to what extent you believe it is violated and how you
###know. You should plot figures and write your response as comments embedded in the code
autoplot(regression_line)

#Normally distributed
#• Independent (no autocorrelation)
#• Homoscedastic (variance is the same)
hist(regression_line$residuals)

#############NOT REALLY SURE WHAT ELSE TO DO HERE ########################


#------D-------#
# Median and 95th percentile of Temp
x_median <- median(DO_temp$Temp)
x_95 <- quantile(DO_temp$Temp, 0.95)

x_median
x_95

# Fit linear model (already done)
regression_line <- lm(DO ~ Temp, data = DO_temp)

# New data for prediction
new_data <- data.frame(Temp = c(x_median, x_95))

# Generate predictions + 95% prediction intervals
preds <- predict(regression_line, newdata = new_data, interval = "prediction", level = 0.95)


# Prediction data frame
pred_df <- as.data.frame(preds)
pred_df$Temp <- c(x_median, x_95)

ggplot(DO_temp, aes(x = Temp, y = DO)) +
  geom_point() +  # actual data points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # regression line
  # prediction intervals (red error bars)
  geom_errorbar(data = pred_df, 
                mapping = aes(x = Temp, ymin = lwr, ymax = upr),
                width = 0.5, color = "red",
                inherit.aes = FALSE) +
  # predicted points (red dots)
  geom_point(data = pred_df,
             mapping = aes(x = Temp, y = fit),
             color = "red", size = 3,
             inherit.aes = FALSE) +
  labs(title = "Predictions with 95% Prediction Intervals",
       subtitle = "Red points = predicted DO; red error bars = prediction intervals") +
  theme_minimal()
















##-----------------------Objective 2-----------------------------##

#-------A/B------#
# Set seed for reproducibility
set.seed(123)

# Sample size
n <- 100

# True regression parameters
beta0 <- 2
beta1 <- 1.5

# Predictor variable (no error in X)
X <- runif(n, 0, 10)

# Lognormal errors (centered to have mean ~0)
err <- rlnorm(n, meanlog = 0, sdlog = 0.6)
err <- err - mean(err)   # center around zero

# Response variable
Y <- beta0 + beta1 * X + err

# Fit a linear model ##this is the linear regression part which is B
model <- lm(Y ~ X)

# Display results
summary(model)

# Plot data and regression line
ggplot(model, aes(x = X, y = Y)) +
  geom_point(color = "skyblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(
    title = "Linear data with lognormal errors",
    x = "X",
    y = "Y"
  ) +
  theme_minimal()


#-------C------#

set.seed(123)

# True parameters
beta0 <- 2
beta1 <- 1.5
n <- 100       # sample size
n_sims <- 100  # number of repetitions

# Empty data frame to store results
results <- data.frame(sim = 1:n_sims,
                      est_intercept = NA,
                      est_slope = NA)

for (i in 1:n_sims) {
  # Generate X (no error)
  X <- runif(n, 0, 10)
  
  # Lognormal errors, centered to mean 0
  err <- rlnorm(n, meanlog = 0, sdlog = 0.6)
  err <- err - mean(err)
  
  # Generate Y
  Y <- beta0 + beta1 * X + err
  
  # Fit linear regression
  model <- lm(Y ~ X)
  
  # Store estimates
  results$est_intercept[i] <- coef(model)[1]
  results$est_slope[i] <- coef(model)[2]
}

# Reshape results for plotting
df_long <- data.frame(
  value = c(results$est_intercept, results$est_slope),
  type = rep(c("Intercept", "Slope"), each = n_sims)
)

# True parameter values (for vertical lines)
true_vals <- data.frame(
  type = c("Intercept", "Slope"),
  value = c(beta0, beta1)
)

# now plot so we can visualize this better
# helps to show the estimated slope and intercept match tru values
ggplot(df_long, aes(x = value, fill = type)) +
  geom_histogram(color = "white", bins = 30, alpha = 0.8) +
  geom_vline(data = true_vals, aes(xintercept = value),
             color = "black", linewidth = 1.2) +
  facet_wrap(~type, scales = "free", nrow = 1) +
  scale_fill_manual(values = c("deeppink", "darkorchid2")) +
  labs(
    title = "Regression Parameter Estimates",
    x = "Estimated Value",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#-------E------#
# Add storage for prediction coverage
results$prediction <- NA

#it was easier to just remake this for loop and add the 95% prediction to it
for (i in 1:n_sims) {
  # Generate X
  X <- runif(n, 0, 10)
  
  # Non-normal errors
  err <- rlnorm(n, meanlog = 0, sdlog = 0.6)
  err <- err - mean(err)
  
  # Generate Y
  Y <- beta0 + beta1 * X + err
  
  # Fit model
  model <- lm(Y ~ X)
  
  # Compute 95% prediction intervals at each X
  preds <- predict(model,
                   newdata = data.frame(X = X),
                   interval = "prediction",
                   level = 0.95)
  
  # Extract lower and upper bounds
  lower <- preds[, "lwr"]
  upper <- preds[, "upr"]
  
  # F. Compute fraction of Y values inside prediction interval
  results$prediction[i] <- mean(Y >= lower & Y <= upper)
  
  # Save estimates
  results$est_intercept[i] <- coef(model)[1]
  results$est_slope[i] <- coef(model)[2]
}


#-------F------#
#see what fraction of y values are in 95% interval
mean(results$prediction)
#I got 0.9529 which tells me that this about 95% fall into the interval


#-------G------#
#this tells me that the intervals are accurate with the 95%, but I think 
#more simulations will be needed to see the true errors
