library(ggplot2)
library(forcats)
library(car)
library(lmtest)
library(sandwich)

# file
audi <- read.csv("audi.csv")
str(audi)
summary(audi)

# Check missing values
colSums(is.na(audi))           

# viz viz
ggplot(audi, aes(x = year)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of year")

ggplot(audi, aes(x = mileage)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of mileage")

ggplot(audi, aes(x = tax)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of tax")

ggplot(audi, aes(x = mpg)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of mpg")

ggplot(audi, aes(x = engineSize)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of engineSize")

ggplot(audi, aes(x = fct_infreq(model))) +
  geom_bar(fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of model")

ggplot(audi, aes(x = fct_infreq(transmission))) +
  geom_bar(fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of transmission")

ggplot(audi, aes(x = fct_infreq(fuelType))) +
  geom_bar(fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of fuelType")

ggplot(audi, aes(x = price)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  labs(title = "Distribution of price")

# viz viz 2 (relation?)
ggplot(audi, aes(x = year, y = log(price))) +
  geom_point(color = "#69b3a2") +
  labs(title = "year vs price")

ggplot(audi, aes(x = mileage, y = log(price))) +
  geom_point(color = "#69b3a2") +
  labs(title = "mileage vs price")

ggplot(audi, aes(x = tax, y = log(price))) +
  geom_point(color = "#69b3a2") +
  labs(title = "tax vs price")

ggplot(audi, aes(x = log(mpg), y = log(price))) +
  geom_point(color = "#69b3a2") +
  labs(title = "mpg vs price")

ggplot(audi, aes(x = engineSize, y = log(price))) +
  geom_point(color = "#69b3a2") +
  labs(title = "engineSize vs price")

# deal dummies
audi$manual = as.integer(audi$transmission=="Manual")
audi$automatic = as.integer(audi$transmission=="Automatic")

audi$diesel = as.integer(audi$fuelType=="Diesel")
audi$petrol = as.integer(audi$fuelType=="Petrol")

# LMS
model_base <- lm(log(price) ~ mileage + 
                   tax + 
                   log(mpg) + 
                   engineSize + 
                   manual+
                   automatic + 
                   petrol +
                   diesel,
                 data=audi)
summary(model_base)
# fit an OLS model with log(price) as the dependent variable and the listed regressors.
# summary() shows coefficients, standard errors, t-values, p-values, R-squared, etc.

# Multicollinearity (VIF)
vif(model_base) >= (1/(1-summary(model_base)$r.squared))









###############################################################################
###############################################################################




# calculate VIF for each predictor to detect multicollinearity.
# The right-hand expression (1/(1-R^2)) gives the threshold


# rerun
model_base <- lm(log(price) ~ mileage + 
                   tax + 
                   log(mpg) + 
                   #engineSize + # i forgot why we remove engineSize here, ask yehor
                   manual+
                   automatic + 
                   diesel +
                   I(tax^2) +
                   I(engineSize^2),
                 data=audi)
summary(model_base)

vif(model_base) >= (1/(1-summary(model_base)$r.squared))
# re check vif on the updated model.

# Heteroskedasticity – Breusch-Pagan Test
bptest(model_base)
# bp test checks for heteroskedasticity.
# the null hypothesis is homoskedasticity.
# we reject so there is heteroskedasticity

coeftest(model_base, vcov = vcovHC(model_base))
# bp test indicates heteroskedasticity, use robust standard errors.
# coeftest() with vcovHC() displays coefficient estimates with robust se.

# RESET
resetReg <- lm(log(price) ~ mileage + 
                 tax + 
                 log(mpg) + 
                 #engineSize + 
                 manual +
                 automatic +
                 diesel +
                 I(tax^2) +
                 I(engineSize^2) +
                 I(fitted(model_base)^2) +
                 I(fitted(model_base)^3),
               data=audi)
# reset test to check for model misspecification, if h0 rejected, we should add 
# nonlinear functions of the regressors (squared, cubes, log, idk)

waldtest(model_base, resetReg, vcov = vcovHC(resetReg, type = "HC0"))
# wald test instead of reset to account for heteroskedasticity 
# (they're the same test effectively [im, pretty sure])
# compare original model to augmented one. if h0 rejected, aka the fitted^2 and 
# fitted^3 are jointly significant, model is misspecified 
