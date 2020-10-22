# Simulating the model 
# y = b_0 + b_1 * x + error

# Set a seed for reproducible results
set.seed(1627)

# Let's say we have 1000 individual plants and we want to model their height 
# change in centimeters as a function of how many liters of water we give them
# in a summer.

# defining the true values of the parameters
# Intercept
# The true height change if we give the plants no water
b_0 <- -5

# Slope 1 == beta_1 == coefficient estimate for first covariate == effect size
# The change in plant height in centimeters for a 1-unit increase in the amount
# of water we give them
b_1 <- 3

# residual standard deviation == "error term"
# unaccounted for stochasticity in the relationship between height change 
# and water added
err_sd <- 5

# number of individuals measured
n_plants <- 1000

# measured values of the first covariate for each individual
# random number generator for a uniform distribution with specified parameters
# amount of water actually given to each plant
x_1 <- runif(n = n_plants, min = 0, max = 5)

# We simulate the error for each measured individual using the random number
# generator for a normal distribution with a mean of 0 and a standard deviation
# equal to our "known" standard error
# the true residuals (the difference between the expected plant height change 
# and the actual plant height change for unmodeled reasons)
error <- rnorm(n = n_plants, mean = 0, sd = err_sd)

# measured response value for each individual, given the *known*
# values of the intercept, measured value of x_1, effect size of x_1 (i.e., the slope aka b_1), and residual error (assuming a linear combination of these things,
# like we do in linear regression)
# the measured change in plant height in centimeters
y <- b_0 + b_1*x_1 + error

# put in a data frame
data <- data.frame(x_1 = x_1, y = y)
head(data)

# plot the data
plot(data$x_1, data$y, xlab = "Water added (L)", ylab = "Plant height change (cm)")

# fitted model using an oridinary least squares (OLS) epistemology
fm1 <- lm(y ~ x_1, data = data)

# summary of the model
summary(fm1)

# add the model fit to the plot
abline(fm1, col = "red")
abline(a = b_0, b = b_1, col = "blue")

# Modifying the standard deviation of the error makes the relationship between
# y and x noisier, and a bit harder to estimate parameters