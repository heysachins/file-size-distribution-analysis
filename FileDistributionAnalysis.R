# Author: Sachin Sasidharan Nair
# Last Modified Date: 24-November-2023

#importing the libraries readr and ggplot2
library(readr)
library(ggplot2)

#importing the file
filesizes <- read_csv("/Users/sachin/My Files/My Projects/File Size Distribution Analysis/filesizes.csv")

##################
### Question 1 ###
##################

#Calculating the mean, standard deviation, median, and quantiles.

mean <- mean(filesizes$x, na.rm = TRUE) #Computing mean
median <- median(filesizes$x, na.rm=TRUE) #Computing median
standard_deviation <- sd(filesizes$x, na.rm = TRUE) #Computing standard deviation
quantiles <- quantile(filesizes$x,c(0.25,0.50 ,0.75)) #Computing quantiles

#Print Histogram
ggplot(filesizes, aes(x=x)) + #aes stands for aesthetic
  geom_histogram(fill="darkgrey",color="white") + #plotting a histogram
  theme_light() +
  labs(title="Histogram of File Sizes",
       x = "File Sizes in kb",
       y = "Count of File Sizes") + #labs stands for label
  theme(plot.title = element_text(hjust = 0.5, color = "black"))

#Print Summaries
cat("Mean: ", mean, "kB\n")
cat("Median: ", median, "kB\n")
cat("Standard Deviation: ", standard_deviation, "kB\n")
cat("The 25th percentile (Q1) is: ", quantiles[1], "kB\n")
cat("The 50th percentile (Q2) is: ", quantiles[2], "kB\n")
cat("The 75th percentile (Q3) is: ", quantiles[3], "kB\n")

##################
### Question 2 ###
##################

# Computing Maximum Likelihood Estimate (MLE) for Alpha

x_m <- 2000
sumLog <- sum(log(filesizes$x))
n <- length(filesizes$x)
alpha_hat <- n / (sumLog - ( n * log(x_m) ) )
cat("The MLE of alpha OR Alpha_Hat is: ", alpha_hat)

##################
### Question 3 ###
##################

# Computing the Fisher Information of Alpha

I <- n / (alpha_hat^2 )
variance_alpha_hat <- 1 / I
distribution_of_alpha_hat <- list (alpha_hat, variance_alpha_hat)
cat("The Fisher Information of alpha is: ", I)

##################
### Question 4 ###
##################

# Computing the Method of Moments Estimator (MME) for Alpha

alpha_tilda <- mean / (mean - x_m)
cat("Method of moments estimator for Alpha OR Alpha_Tilda: ", alpha_tilda)

##################
### Question 5 ###
##################

# Computing the 90 % Confidence Interval for Alpha

stdErr_alpha_hat <- sqrt(variance_alpha_hat)
z_score <- qnorm(0.95)
cat(z_score) # 1.645
lower <- alpha_hat - z_score * stdErr_alpha_hat
upper <- alpha_hat + z_score * stdErr_alpha_hat
cat("90% Confidence Interval for Alpha: [",lower, ",",upper,"]")

##################
### Question 6 ###
##################

#Using the same equation as in question 4 to compute MME for Alpha

alpha_tilda <- mean / (mean - x_m)
cat("Method of moments estimator for Alpha OR Alpha_Tilda: ", alpha_tilda)

##################
### Question 7 ###
##################

#importing the Pareto library
library(Pareto)

#number of simulations will be 1500
num_simulations <- 1500

# The below function will simulate one run and calculate Y'
simulate_Y_dash <- function() {
  simulated_sizes <- rPareto(n, x_m, alpha_hat)
  mean(simulated_sizes)
}

# The below code calls the function simulate_Y_dash() 1500 times
Y_dashes <- replicate(num_simulations, simulate_Y_dash())

# creating a dataframe for ggplot
Y_dashes_df <- data.frame(Y_dash = Y_dashes)

# printing the histogram
ggplot(Y_dashes_df, aes(x=Y_dash)) + #aes stands for aesthetic
  geom_histogram(fill="darkgrey",color="white") + #plotting a histogram
  theme_light() +
  labs(title="Histogram of Simulated Mean File Sizes Y'",
       x = "Mean File Size",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, color = "black"))

#hist(Y_dashes, main="Histogram of Simulated Mean File Sizes Y'", xlab="Mean File Size")

# Calculating numerical summaries
mean_Y_dash <- mean(Y_dashes)
sd_Y_dash <- sd(Y_dashes)
median_Y_dash <- median(Y_dashes)
quantiles_Y_dash <- quantile(Y_dashes, probs = c(0.25, 0.5, 0.75))

# Printing the summaries
print(paste("Mean: ", mean_Y_dash))
print(paste("Standard Deviation: ", sd_Y_dash))
print(paste("Median: ", median_Y_dash))
print(quantiles_Y_dash)
cat("The 25th percentile (Q1) is: ", quantiles_Y_dash[1])
cat("The 50th percentile (Q2) is: ", quantiles_Y_dash[2])
cat("The 75th percentile (Q3) is: ", quantiles_Y_dash[3])



