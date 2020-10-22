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

# plots in which the plants are measured (n = 10)
n_plots <- 10
plots <- rep(1:n_plots, each = 10)
plots

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

# Simulated offsets for each plot 
sd_plot_intercept <- 1
re_intercept <- rnorm(n=n_plots, mean = 0, sd = sd_plot_intercept)

# measured response value for each individual, given the *known*
# values of the intercept, measured value of x_1, effect size of x_1 (i.e., the slope aka b_1), and residual error (assuming a linear combination of these things,
# like we do in linear regression)
# the measured change in plant height in centimeters
y <- b_0 + b_1*x_1 + error + re_intercept[plots]

# put in a data frame
data <- data.frame(x_1 = x_1, y = y, plot_id = plots)

# plot the data
library(ggplot2)
ggplot(data, aes(x = x_1, y = y, color = as.factor(plot_id))) +
         geom_point()+
          xlab("Water added (L)")+
          ylab("Plant height change (cm)")

# fitted model using an Maximum Likelihood 
library(lme4)
fm1 <- lmer(y ~ x_1 + (1|plot_id), data = data)

# summary of the model
summary(fm1)

