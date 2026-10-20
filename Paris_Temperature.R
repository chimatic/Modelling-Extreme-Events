############################################################
# PROJECT: ANALYSING EXTREME TEMPERATURES IN PARIS
# Period: 1995–2019
# In this project, we use daily average temperature data
# from Paris to investigate extreme temperature behaviour.


# Main question:
# How can we describe annual temperature extremes in Paris,
# and do these extremes change over time?


# Method:
# 1. Explore daily average temperature.
# 2. Extract the annual maximum for each year.
# 3. Fit Generalized Extreme Value (GEV) models.
# 4. Compare the models using AIC to see whether allowing a trend over time
# improves our description of the data.





############################################################
# SECTION 1: PREPARE R AND LOAD THE PACKAGES
############################################################

# We install and load the packages needed to organise our data and fit
# Extreme Value Theory models.

# Install packages

install.packages("dplyr")
install.packages("ismev")
install.packages("evd")
install.packages("fExtremes")

# Loading Packages 

library(dplyr)  # Helps us organise and summarise data.
library(ismev)  # Provides functions for extreme value models.
library(evd)
library(fExtremes)




############################################################
# 2. LOADING THE PARIS TEMPERATURE DATA
############################################################

# We import the daily temperature records into R.
# Each row represents an observation, while the columns
# contain information such as the year, month, day and
# average temperature.


paris_temp_data <- read.csv("paris_temp_1995_2019_.csv")
View(paris_temp_data)

# We inspect the dataset to understand its structure and
# check whether the variables have been imported correctly.

View(paris_temp_data)
dim(paris_temp_data)
names(paris_temp_data)
summary(paris_temp_data$AvgTemp)

#Results
#> dim(paris_temp_data)
#[1] 9072    5
#> names(paris_temp_data)
#[1] "City"    "Month"   "Day"     "Year"    "AvgTemp"
#> summary(paris_temp_data$AvgTemp)
~#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-10.100   7.475  12.200  12.222  17.200  33.100 





############################################################
# 3. PREPARING THE DATE VARIABLE
############################################################

# The year, month and day are initially stored separately.
# We combine them into one date variable so that we can
# arrange the observations in chronological order and
# visualise how temperature changes over time.


paris_temp_data$Date <- as.Date(
  with(
    paris_temp_data,
    paste(Year, Month, Day, sep = "-")
  ),
  format = "%Y-%m-%d"
)

names(paris_temp_data)

#> names(paris_temp_data)
#[1] "City"    "Month"   "Day"     "Year"    "AvgTemp" "Date" 


# We arrange the records from the earliest date to the latest.
# This ensures that the time series is displayed correctly.

paris_temp_data <- paris_temp_data %>%
  arrange(Date)

# We check the period covered by the dataset and identify
# any missing temperature observations.

range(paris_temp_data$Date, na.rm = TRUE)
sum(is.na(paris_temp_data$AvgTemp))

#> sum(is.na(paris_temp_data$AvgTemp))
#[1] 0



############################################################
# 4. EXPLORING DAILY TEMPERATURES
############################################################

# We begin by plotting the daily average temperatures.
# This helps us observe seasonal patterns and changes
# in temperature throughout the study period.

plot(
  paris_temp_data$Date,
  paris_temp_data$AvgTemp,
  type = "l",
  main = "Daily Average Temperature in Paris Over Time",
  xlab = "Date",
  ylab = "Average Temperature (°C)",
  col = "navy"
)


# We use the autocorrelation function (ACF) to investigate
# whether temperatures on one day are related to temperatures
# on previous days. For example, warm days often occur in
# sequences rather than as completely isolated events.



############################################################
# CHECKING FOR SEASONALITY
############################################################

# Calculate the mean daily average temperature for each month
# using observations from 1995 to 2019.

Monthly_Avg <- aggregate(
  AvgTemp ~ Month,
  data = paris_temp_data,
  FUN = mean,
  na.rm = TRUE
)

# Plot the monthly mean temperatures to identify the
# recurring seasonal pattern in Paris.

plot(
  Monthly_Avg$Month,
  Monthly_Avg$AvgTemp,
  type = "b",
  pch = 19,
  xaxt = "n",
  main = "Seasonal Pattern of Average Temperature in Paris (1995–2019)",
  xlab = "Month",
  ylab = "Mean Daily Average Temperature (°C)"
)

axis(1, at = 1:12, labels = month.abb)


############################################################
# 2.1biii CHECKING FOR POSSIBLE OUTLIERS
############################################################

boxplot(
  paris_temp_data$AvgTemp,
  main = "Boxplot of Daily Average Temperatures in Paris (1995–2019)",
  ylab = "Daily Average Temperature (°C)",
  col = "lightblue"
)


acf((paris_temp_data$AvgTemp),
main = "ACF of Daily Average Temperature"
)

############################################################
# 5. EXTRACTING ANNUAL MAXIMUM TEMPERATURES
############################################################

# Our focus is on extreme temperatures rather than every
# daily observation.
#
# We therefore divide the data into yearly blocks and select
# the highest daily average temperature in each year.
#
# This is called the annual block maxima approach in
# Extreme Value Theory.

paris_annual_max <- paris_temp_data %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%
  group_by(Year) %>%
  summarise(
    AnnualMax = max(AvgTemp),
    .groups = "drop"
  )

# We display the resulting annual maxima and check how many
# years are available for the statistical analysis.

print(paris_annual_max)
nrow(paris_annual_max)

#> nrow(paris_annual_max)
#[1] 25

# We plot the annual maxima to explore whether the most
# extreme daily average temperatures appear to change
# over the years.

plot(
  paris_annual_max$Year,
  paris_annual_max$AnnualMax,
  type = "b",
  pch = 19,
  main = "Annual Maximum Daily Average Temperature in Paris",
  xlab = "Year",
  ylab = "Annual Maximum Temperature (°C)",
  col = "darkred"
)

acf(
  paris_annual_max$AnnualMax,
  main = "ACF of Annual Maximum Temperatures"
)

data <- paris_annual_max$AnnualMax

############################################################
# 6. FITTING THE STATIONARY GEV MODEL
############################################################

# We first fit a stationary Generalized Extreme Value (GEV)
# model to the annual maximum temperatures.
#
# A stationary model assumes that the statistical
# characteristics of extreme temperatures remain constant
# throughout the study period.
#
# The GEV distribution has three parameters:
# Location describes the general level of the extremes.
# Scale describes how widely the extremes vary.
# Shape describes the behaviour of the distribution's tail.

gev_constant <- gev.fit(data)

#$nllh
#[1] 51.17253

#$mle
#[1] 27.099791022  1.593513411 -0.002410794
# mu= 27.099791022  sigma= 1.593513411 gamma= -0.002410794


# We display the estimated parameters and examine diagnostic
# plots to assess how well the GEV model describes the data.


gev.diag(gev_constant)


############################################################
# 7. FITTING A LINEAR NON-STATIONARY GEV MODEL
############################################################

# Unlike the stationary model, a non-stationary model allows
# selected distribution parameters to change over time.
#
# Here, we allow the location parameter to follow a linear
# trend. The scale and shape parameters remain constant.
#
# This helps us investigate whether the general level of
# annual temperature extremes has increased or decreased
# during the study period.

loc1 = matrix(1:25, ncol=1) 

LinearModel <- gev.fit(
  data,
  ydat = loc1,
  mul = 1
)

#$nllh
#[1] 51.07185
#$mle
#[1]   27.434503062 -0.025926034  1.582525470  0.004102379
# mu(t)= beta0 + beta1*t

#  Then we have 
# mu(t) = 27.434503062  - 0.025926034*t,  
# sigma = 1.582525470 
# gamma = 0.004102379



############################################################
# 8. FITTING A QUADRATIC NON-STATIONARY GEV MODEL
############################################################

# Temperature extremes may not follow a straight-line trend.
# We therefore fit a quadratic model that allows the
# location parameter to follow a curved pattern over time.
#
# This model includes both time and squared time.
# It is more flexible than the linear model, but it also
# requires the estimation of an additional parameter.

loc2 = cbind(matrix(1:25,ncol=1), matrix((1:25)^ 2, ncol=1))

QuadraticModel = gev.fit(data, ydat = loc2, mul = 1:2)

#$nllh
#[1] 47.36438
#$mle
#[1] 30.05309682 -0.53209745  0.01877973  1.42481427 -0.06133105
#$se
#[1] 1.22390678 0.23700377 0.00962147 0.30337006 0.23526301

# mu(t)= beta0+beta1*t+ beta2*t^2

#  Then we have 
# mu(t) = 30.05309682  - 0.53209745 *t + 0.01877973 *t^2,  
# sigma = 1.42481427  
# gamma = -0.06133105

# We inspect the fitted quadratic model before comparing
# it with the stationary and linear models.



############################################################
# 9. COMPARING MODELS USING AIC
############################################################

# AIC balances model fit against the number of parameters.
# A lower AIC is preferred when comparing models fitted
# to the same observations.

# gev.fit() reports nllh, the negative log-likelihood.
# Therefore, AIC = 2 * nllh + 2 * number of parameters.

# The stationary GEV estimates three parameters:
# location, scale and shape.

AICconstant <- 2 * gev_constant$nllh + 2 * 3

# The linear GEV estimates four parameters:
# two location coefficients, scale and shape.

AIClinear <- 2 * LinearModel$nllh + 2 * 4

# The quadratic GEV estimates five parameters:
# three location coefficients, scale and shape.

AICquadratic <- 2 * QuadraticModel$nllh + 2 * 5

# We place the results in one table for direct comparison.

AICcomparison <- data.frame(
  Model = c(
    "Stationary GEV",
    "Linear GEV",
    "Quadratic GEV"
  ),
  Parameters = c(3, 4, 5),
  AIC = c(
    AICconstant,
    AIClinear,
    AICquadratic
  )
)

# Rank the models from lowest AIC to highest AIC.

AICcomparison <- AICcomparison[order(AICcomparison$AIC), ]

print(AICcomparison)


############################################################
# PLOT: COMPARING STATIONARY AND NON-STATIONARY GEV MODELS
############################################################

years <- 1995:2019

plot(years, data, pch = 19, col = "black",
     main = "Annual Temperature Maxima and Fitted GEV Trends",
     xlab = "Year", ylab = "Annual Maximum Temperature (°C)")

abline(h = gev_constant$mle[1], col = "red", lty = 2, lwd = 2)
lines(years, LinearModel$vals[, 1], col = "blue", lwd = 2)
lines(years, QuadraticModel$vals[, 1], col = "darkgreen", lwd = 2)

legend("topleft",
       legend = c("Annual maxima", "Stationary location",
                  "Linear location", "Quadratic location"),
       col = c("black", "red", "blue", "darkgreen"),
       pch = c(19, NA, NA, NA),
       lty = c(NA, 2, 1, 1),
       lwd = c(NA, 2, 2, 2),
       bty = "n")

############################################################
# 10. CONFIDENCE INTERVALS FOR THE GEV PARAMETERS
############################################################

# Confidence intervals for mu, sigma and gamma at level 95%
# This helps us assess the uncertainty in the estimated GEV parameters
# and the range of plausible values for each parameter.

alpha = 0.05

# Confidence Interval for mu at level 95%
# This gives the range of plausible values for the location parameter
gev_constant$mle[1] - gev_constant$se[1] * qnorm(1 - alpha/2, 0, 1)
gev_constant$mle[1] + gev_constant$se[1] * qnorm(1 - alpha/2, 0, 1)

# Confidence Interval for sigma at level 95%
# This gives the range of plausible values for the scale parameter
gev_constant$mle[2] - gev_constant$se[2] * qnorm(1 - alpha/2, 0, 1)
gev_constant$mle[2] + gev_constant$se[2] * qnorm(1 - alpha/2, 0, 1)

# Confidence Interval for gamma at level 95%
# This gives the range of plausible values for the shape parameter
# The interval helps us assess the tail behaviour of extreme temperatures
gev_constant$mle[3] - gev_constant$se[3] * qnorm(1 - alpha/2, 0, 1)
gev_constant$mle[3] + gev_constant$se[3] * qnorm(1 - alpha/2, 0, 1)

# Extract GEV parameter estimates
gev_estimates <- gev_constant$mle

# Calculate standard errors from the covariance matrix
gev_se <- sqrt(diag(gev_constant$cov))

# Calculate 95% confidence intervals
gev_lower <- gev_estimates - 1.96 * gev_se
gev_upper <- gev_estimates + 1.96 * gev_se

# Create a table of results
gev_ci <- data.frame(
  Parameter = c("Location (mu)", "Scale (sigma)", "Shape (gamma)"),
  Estimate = gev_estimates,
  Standard_Error = gev_se,
  Lower_95_CI = gev_lower,
  Upper_95_CI = gev_upper
)

print(gev_ci)


############################################################
# 11. TESTING THE GEV SHAPE PARAMETER
############################################################

# Test if the GEV shape parameter is significantly different from zero
# H0: gamma = 0
# H1: gamma is not equal to zero
# This helps us determine whether the shape parameter provides
# evidence of tail behaviour different from the Gumbel case.

gamma_hat <- gev_constant$mle[3]
gamma_se  <- gev_constant$se[3]

z_gamma <- gamma_hat / gamma_se
p_gamma <- 2 * pnorm(-abs(z_gamma))

c(
  Estimate = gamma_hat,
  SE = gamma_se,
  z_statistic = z_gamma,
  p_value = p_gamma
)

# The estimated gamma value is -0.002410794, which is very close to zero.
# The p-value is 0.988907547, which is greater than 0.05.
# Therefore, we fail to reject the null hypothesis.
# This means that our analysis does not provide sufficient evidence
# that the shape parameter differs from zero.


############################################################
# 12. CALCULATING GEV RETURN LEVELS
############################################################

# Calculate return levels for 100, 200, 500 and 1000 years
# This helps us estimate extreme annual maximum temperatures
# associated with different return periods.

# Extract the estimated GEV parameters
mu <- gev_constant$mle[1]
sigma <- gev_constant$mle[2]
gamma <- gev_constant$mle[3]

# Define the return periods in years
return_periods <- c(100, 200, 500, 1000)

# Calculate the GEV return levels for each return period
gev_levels <- sapply(return_periods, function(T) {
  p <- 1 / T
  yp <- -log(1 - p)
  
  if (abs(gamma) < 1e-6) {
    mu - sigma * log(yp)
  } else {
    mu - (sigma / gamma) * (1 - yp^(-gamma))
  }
})

# Display the return periods and their estimated GEV return levels
gev_return_table <- data.frame(
  ReturnPeriod_years = return_periods,
  GEV_ReturnLevel = gev_levels
)

print(gev_return_table)

# Plot the GEV return levels for different return periods
plot(
  return_periods,
  gev_levels,
  type = "l",
  log = "x",
  lwd = 2,
  main = "GEV Return Level Curve for Paris",
  xlab = "Return Period (years, log scale)",
  ylab = "Return Level (°C)"
)


############################################################
# 13. POT ANALYSIS: SELECTING THE THRESHOLD
############################################################

# Extract the daily average temperatures and remove non-finite values
daily_temp <- paris_temp_data$AvgTemp
daily_temp <- daily_temp[is.finite(daily_temp)]

# Mean residual life plot
# This helps us identify a suitable threshold for the POT analysis
mrlplot(daily_temp)

# threshold identified is 22.645


############################################################
# 14. ESTIMATING GPD PARAMETERS
############################################################

# Select the threshold using the 95th percentile of daily average temperatures
u <- as.numeric(
  quantile(daily_temp, 0.95, na.rm = TRUE)
)

# Extract temperatures that exceed the selected threshold
exceedances <- daily_temp[daily_temp > u]

# Calculate the threshold, number of exceedances and proportion of exceedances
c(
  threshold = u,
  number_exceeding = length(exceedances),
  proportion_exceeding = length(exceedances) / length(daily_temp)
)

# Fit the Generalized Pareto Distribution using the selected threshold
gpd_model <- fpot(daily_temp, u)

# Display the fitted GPD model and its estimated parameters
print(gpd_model)

# Threshold: 22.645 
# Number Above: 454 
# Proportion Above: 0.05 

# Estimates
# scale    shape  
# 2.4508  -0.1544  

# Standard Errors
# scale    shape
# 0.14998  0.03994  

# Optimization Information
# Convergence: successful 
# Function Evaluations: 28 
# Gradient Evaluations: 7 


############################################################
# 15. CONFIDENCE INTERVALS FOR THE GPD PARAMETERS
############################################################

# Confidence intervals for scale and shape at level 95%
# This helps us assess the uncertainty in the estimated GPD parameters

alpha = 0.05

# Confidence Interval for scale at level 95%
# This gives the range of plausible values for the GPD scale parameter
gpd_model$estimate[1] - gpd_model$estimate[1] * 0 +
  1.96 * 0


# Confidence intervals for scale and shape at level 95%
# This helps us assess the uncertainty in the estimated GPD parameters

alpha = 0.05

# Estimated GPD parameters and their standard errors
scale_hat <- 2.4508
scale_se <- 0.14998

shape_hat <- -0.1544
shape_se <- 0.03994

# Confidence Interval for scale at level 95%
# This gives the range of plausible values for the GPD scale parameter
scale_hat - scale_se * qnorm(1 - alpha/2, 0, 1)
scale_hat + scale_se * qnorm(1 - alpha/2, 0, 1)

# Confidence Interval for shape at level 95%
# This gives the range of plausible values for the GPD shape parameter
# The interval helps us assess the tail behaviour of extreme temperatures
shape_hat - shape_se * qnorm(1 - alpha/2, 0, 1)
shape_hat + shape_se * qnorm(1 - alpha/2, 0, 1)

# Extract GPD parameter estimates
gpd_estimates <- gpd_temp$mle

# Calculate standard errors from the covariance matrix
gpd_se <- sqrt(diag(gpd_temp$cov))

# Calculate 95% confidence intervals
gpd_lower <- gpd_estimates - 1.96 * gpd_se
gpd_upper <- gpd_estimates + 1.96 * gpd_se

# Create a table of results
gpd_ci <- data.frame(
  Parameter = c("Scale (sigma)", "Shape (xi)"),
  Estimate = gpd_estimates,
  Standard_Error = gpd_se,
  Lower_95_CI = gpd_lower,
  Upper_95_CI = gpd_upper
)

print(gpd_ci)

# Plot diagnostic graphs for the fitted GPD model

gpd_temp <- gpd.fit(
  xdat = daily_temp,
  threshold = u,
  npy = 365
)

print(gpd_temp)
gpd.diag(gpd_temp)



############################################################
# 16 TESTING THE GPD SHAPE PARAMETER
############################################################

# Test if the GPD shape parameter is significantly different from zero
# H0: gamma = 0
# H1: gamma is not equal to zero

# Extract the estimated shape parameter and its standard error
gamma_hat <- -0.1544
gamma_se <- 0.03994

# Calculate the test statistic and p-value
z_gamma <- gamma_hat / gamma_se
p_gamma <- 2 * pnorm(-abs(z_gamma))

# Display the estimated shape parameter, standard error, test statistic and p-value
c(
  Estimate = gamma_hat,
  SE = gamma_se,
  z_statistic = z_gamma,
  p_value = p_gamma
)

# Interpret the test result at the 5% significance level
if (p_gamma < 0.05) {
  print("Reject H0: the GPD shape parameter is significantly different from zero")
} else {
  print("Do not reject H0: there is insufficient evidence that the GPD shape parameter differs from zero")
}

# Estimate            SE   z_statistic       p_value 
# -0.1544000000  0.0399400000 -3.8657986980  0.0001107262
# Interpret the test result at the 5% significance level
# "Reject H0: the GPD shape parameter is significantly different from zero"


############################################################
# 17. CALCULATING GPD RETURN LEVELS
############################################################

# Extract the estimated GPD parameters
scale_hat <- gpd_temp$mle[1]
shape_hat <- gpd_temp$mle[2]

# Calculate the proportion of daily temperatures exceeding the threshold
exceedance_rate <- sum(daily_temp > u) / length(daily_temp)

# Define the return periods in years
return_periods <- c(100, 200, 500, 1000)

# Calculate the GPD return levels
gpd_levels <- sapply(return_periods, function(T) {
  
  # Convert the return period from years to days
  T_days <- T * 365
  
  # Calculate the return level using the daily exceedance rate
  if (abs(shape_hat) < 1e-6) {
    u + scale_hat * log(T_days * exceedance_rate)
  } else {
    u + (scale_hat / shape_hat) *
      ((T_days * exceedance_rate)^shape_hat - 1)
  }
})

# Display the GPD return levels
gpd_return_table <- data.frame(
  ReturnPeriod_years = return_periods,
  GPD_ReturnLevel = gpd_levels
)

print(gpd_return_table)

# Plot the GPD return levels
plot(
  return_periods,
  gpd_levels,
  type = "b",
  pch = 19,
  log = "x",
  main = "GPD Return Levels for Paris",
  xlab = "Return Period (years, log scale)",
  ylab = "Return Level (°C)"
)

############################################################
# 18. COMPARING GEV AND GPD RETURN LEVELS
############################################################

# Combine the return levels from both models into one table
return_level_comparison <- data.frame(
  ReturnPeriod_years = return_periods,
  GEV_ReturnLevel = gev_levels,
  GPD_ReturnLevel = gpd_levels
)

# Display the comparison table
print(return_level_comparison)

# Plot both models on the same graph
plot(
  return_periods,
  gev_levels,
  type = "b",
  pch = 19,
  lwd = 2,
  log = "x",
  ylim = range(c(gev_levels, gpd_levels)),
  main = "Comparison of GEV and GPD Return Levels",
  xlab = "Return Period (years, log scale)",
  ylab = "Return Level (°C)"
)

# Add the GPD return levels
lines(
  return_periods,
  gpd_levels,
  type = "b",
  pch = 17,
  lwd = 2,
  lty = 2,
  col = "red"
)

# Add a legend
legend(
  "topleft",
  legend = c("GEV: Annual Maxima", "GPD: Threshold Exceedances"),
  col = c("black", "red"),
  lty = c(1, 2),
  pch = c(19, 17),
  lwd = 2
)


# Display the comparison table

# ReturnPeriod_years GEV_ReturnLevel GPD_ReturnLevel
# 1                100        34.38969        33.53696
# 2                200        35.48510        34.04176
# 3                500        36.92747        34.63127
# 4               1000        38.01546        35.02493



############################################################
# Section 18: RETURN LEVELS WITH CONFIDENCE INTERVALS
############################################################

# Number of simulations
set.seed(123)
B <- 10000

# Return periods in years
return_periods <- c(100, 200, 500, 1000)


# -----------------------------------------
# GEV: Simulate parameter uncertainty
# -----------------------------------------

gev_estimates <- gev_constant$mle
gev_cov <- gev_constant$cov

gev_sim <- MASS::mvrnorm(
  n = B,
  mu = gev_estimates,
  Sigma = gev_cov
)

# Keep valid GEV parameter values
gev_sim <- gev_sim[
  gev_sim[, 2] > 0,
]

# Calculate GEV return levels
gev_boot_levels <- sapply(return_periods, function(T) {
  
  mu <- gev_sim[, 1]
  sigma <- gev_sim[, 2]
  gamma <- gev_sim[, 3]
  
  p <- 1 / T
  yp <- -log(1 - p)
  
  ifelse(
    abs(gamma) < 1e-6,
    mu - sigma * log(yp),
    mu - (sigma / gamma) * (1 - yp^(-gamma))
  )
})

# GEV point estimates and confidence intervals
gev_results <- data.frame(
  Return_Period = return_periods,
  
  GEV_Estimate = sapply(return_periods, function(T) {
    
    mu <- gev_estimates[1]
    sigma <- gev_estimates[2]
    gamma <- gev_estimates[3]
    
    p <- 1 / T
    yp <- -log(1 - p)
    
    if (abs(gamma) < 1e-6) {
      mu - sigma * log(yp)
    } else {
      mu - (sigma / gamma) * (1 - yp^(-gamma))
    }
  }),
  
  GEV_Lower_95 = apply(
    gev_boot_levels, 2, quantile, probs = 0.025
  ),
  
  GEV_Upper_95 = apply(
    gev_boot_levels, 2, quantile, probs = 0.975
  )
)


# -----------------------------------------
# GPD: Simulate parameter and rate uncertainty
# -----------------------------------------

gpd_estimates <- gpd_temp$mle
gpd_cov <- gpd_temp$cov

gpd_sim <- MASS::mvrnorm(
  n = B,
  mu = gpd_estimates,
  Sigma = gpd_cov
)

# Estimate daily exceedance probability
n_daily <- length(daily_temp)
n_exceed <- sum(daily_temp > u)
p_hat <- n_exceed / n_daily

# Simulate uncertainty in the exceedance probability
p_sim <- rbinom(
  B,
  size = n_daily,
  prob = p_hat
) / n_daily

# Keep valid scale estimates and positive exceedance rates
valid <- gpd_sim[, 1] > 0 & p_sim > 0

gpd_sim <- gpd_sim[valid, , drop = FALSE]
p_sim <- p_sim[valid]

# Calculate GPD return levels
gpd_boot_levels <- sapply(return_periods, function(T) {
  
  scale <- gpd_sim[, 1]
  xi <- gpd_sim[, 2]
  
  expected_exceedances <- T * 365 * p_sim
  
  ifelse(
    abs(xi) < 1e-6,
    u + scale * log(expected_exceedances),
    u + (scale / xi) *
      (expected_exceedances^xi - 1)
  )
})

# GPD point estimates and confidence intervals
gpd_results <- data.frame(
  Return_Period = return_periods,
  
  GPD_Estimate = sapply(return_periods, function(T) {
    
    expected_exceedances <- T * 365 * p_hat
    scale <- gpd_estimates[1]
    xi <- gpd_estimates[2]
    
    if (abs(xi) < 1e-6) {
      u + scale * log(expected_exceedances)
    } else {
      u + (scale / xi) *
        (expected_exceedances^xi - 1)
    }
  }),
  
  GPD_Lower_95 = apply(
    gpd_boot_levels, 2, quantile, probs = 0.025
  ),
  
  GPD_Upper_95 = apply(
    gpd_boot_levels, 2, quantile, probs = 0.975
  )
)

# -----------------------------------------
# Combine GEV and GPD results
# -----------------------------------------

return_level_comparison <- merge(
  gev_results,
  gpd_results,
  by = "Return_Period"
)

# Display the comparison table
print(return_level_comparison)

# Display the comparison table
# print(return_level_comparison)
# Return_Period GEV_Estimate GEV_Lower_95 GEV_Upper_95 GPD_Estimate GPD_Lower_95 GPD_Upper_95
# 1           100     34.38969     30.89597     42.78209     33.53696     31.95660     35.51283
# 2           200     35.48510     31.19154     47.68846     34.04176     32.26095     36.34102
# 3           500     36.92747     31.48480     56.48000     34.63127     32.60006     37.36665
# 4          1000     38.01546     31.66666     64.96347     35.02493     32.80629     38.10197



############################################################
# Section 19: COMPARING RETURN LEVELS WITH CONFIDENCE INTERVALS
############################################################

# Plot GEV return levels
plot(
  return_periods,
  gev_results$GEV_Estimate,
  type = "b",
  pch = 19,
  lwd = 2,
  log = "x",
  ylim = range(
    gev_results$GEV_Lower_95,
    gev_results$GEV_Upper_95,
    gpd_results$GPD_Lower_95,
    gpd_results$GPD_Upper_95
  ),
  main = "GEV and GPD Return Levels with 95% Confidence Intervals",
  xlab = "Return Period (years, log scale)",
  ylab = "Return Level (°C)"
)

# Add GEV confidence intervals
arrows(
  return_periods,
  gev_results$GEV_Lower_95,
  return_periods,
  gev_results$GEV_Upper_95,
  angle = 90,
  code = 3,
  length = 0.05
)

# Add GPD return levels
lines(
  return_periods,
  gpd_results$GPD_Estimate,
  type = "b",
  pch = 17,
  lwd = 2,
  lty = 2,
  col = "red"
)

# Add GPD confidence intervals
arrows(
  return_periods,
  gpd_results$GPD_Lower_95,
  return_periods,
  gpd_results$GPD_Upper_95,
  angle = 90,
  code = 3,
  length = 0.05,
  col = "red"
)

# Add a legend
legend(
  "topleft",
  legend = c(
    "GEV: Annual Maxima",
    "GPD: Threshold Exceedances"
  ),
  col = c("black", "red"),
  lty = c(1, 2),
  pch = c(19, 17),
  lwd = 2
)


