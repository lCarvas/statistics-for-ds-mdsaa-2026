# Group 13
# Beatris Daicu - 20221854
# Diogo Carvalho - 20221935
# Ricardo Pereira - 20250343
# Yehor Malakhov - 20221691

library(ggplot2)
library(forcats)
library(car)
library(lmtest)
library(sandwich)

# File
audi <- read.csv("data_group_13.csv")
str(audi)
summary(audi)

# Check missing values
colSums(is.na(audi))

# Visualizations
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

# Dummies
audi$manual = as.integer(audi$transmission == "Manual")
audi$automatic = as.integer(audi$transmission == "Automatic")

audi$diesel = as.integer(audi$fuelType == "Diesel")
audi$petrol = as.integer(audi$fuelType == "Petrol")

# LMS
model_base <- lm(
  log(price) ~ mileage +
    tax +
    log(mpg) +
    engineSize +
    manual +
    automatic +
    petrol +
    diesel,
  data = audi
)
summary(model_base)

# Multicollinearity (VIF)
vif(model_base) >= (1 / (1 - summary(model_base)$r.squared))

# Rerun
model_base <- lm(
  log(price) ~ mileage +
    tax +
    log(mpg) +
    engineSize +
    manual +
    automatic +
    diesel,
  data = audi
)
summary(model_base)

vif(model_base) >= (1 / (1 - summary(model_base)$r.squared))

# Heteroskedasticity – Breusch-Pagan Test
bptest(model_base)

coeftest(model_base, vcov = vcovHC(model_base, type = "HC0"))

# RESET
resetReg <- lm(
  log(price) ~ mileage +
    tax +
    log(mpg) +
    engineSize +
    manual +
    automatic +
    diesel +
    I(fitted(model_base)^2) +
    I(fitted(model_base)^3),
  data = audi
)

waldtest(model_base, resetReg, vcov = vcovHC(resetReg, type = "HC0"))

newBaseModel <- lm(
  log(price) ~ # mileage +
    # tax +
    # log(mpg) +
    # engineSize +
    manual +
    automatic +
    diesel +
    I(mileage^2) +
    I(tax^2) +
    I(log(mpg)^2) +
    I(engineSize^2),
  data = audi
)

vif(newBaseModel) >= (1 / (1 - summary(newBaseModel)$r.squared))
vif(newBaseModel)

newResetReg <- lm(
  log(price) ~ manual +
    automatic +
    diesel +
    I(mileage^2) +
    I(tax^2) +
    I(log(mpg)^2) +
    I(engineSize^2) +
    I(fitted(model_base)^2) +
    I(fitted(model_base)^3),
  data = audi
)

waldtest(newBaseModel, newResetReg, vcov = vcovHC(newResetReg, type = "HC0"))

