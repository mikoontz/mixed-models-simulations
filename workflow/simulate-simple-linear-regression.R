# Set a seed for reproducible results
set.seed(1627)

# Intercept
b_0 <- 1

# Slope 1 == beta_1 == coefficient estimate for first covariate == effect size
b_1 <- 3

# residual standard deviation == "error term"
err_sd <- 10

# number of individuals measured
n_plants <- 1000

# measured values of the first covariate for each individual
x_1 <- runif(n = n_plants, min = -5, max = 5)

# measured response value for each individual, given the *known*
# values of the intercept, measured value of x_1, effect size of x_1, and residual error (assuming a linear combination of these things,
# like we do in linear regression)
y <- b_0 + b_1*x_1 + rnorm(n = n_plants, mean = 0, sd = err_sd)

# put in a data frame
data <- data.frame(x_1 = x_1, y = y)

# plot the data
plot(data$x_1, data$y)

# fitted model using an oridinary least squares (OLS) epistemology
fm1 <- lm(y ~ x_1, data = data)

# summary of the model
summary(fm1)

