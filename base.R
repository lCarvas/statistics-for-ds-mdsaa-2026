library(ggplot2)
library(forcats)
library(car)
library(lmtest)

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

# viz viz 2
ggplot(audi, aes(x = year, y = price)) +
  geom_point(color = "#69b3a2") +
  labs(title = "year vs price")

ggplot(audi, aes(x = mileage, y = price)) +
  geom_point(color = "#69b3a2") +
  labs(title = "mileage vs price")

ggplot(audi, aes(x = tax, y = price)) +
  geom_point(color = "#69b3a2") +
  labs(title = "tax vs price")

ggplot(audi, aes(x = mpg, y = price)) +
  geom_point(color = "#69b3a2") +
  labs(title = "mpg vs price")

ggplot(audi, aes(x = engineSize, y = price)) +
  geom_point(color = "#69b3a2") +
  labs(title = "engineSize vs price")




# deal dummies
audi$manual = as.integer(audi$transmission=="Manual")
audi$automatic = as.integer(audi$transmission=="Automatic")

audi$diesel = as.integer(audi$fuelType=="Diesel")
audi$petrol = as.integer(audi$fuelType=="Petrol")


# LMS
model_base <- lm(log(price) ~ year + 
                              mileage + 
                              tax + 
                              mpg + 
                              engineSize + 
                              manual+
                              automatic + 
                              petrol +
                              diesel,
                 data=audi)
summary(model_base)

### Multicollinearity (VIF)
vif(model_base)
# shouldn't be bigger than 1/(1-0.8894) -> r squared
# rerun 
model_base <- lm(log(price) ~ year + 
                   mileage + 
                   tax + 
                   mpg + 
                   engineSize + 
                   manual+
                   automatic + 
                   diesel,
                 data=audi)
summary(model_base)
vif(model_base)


### Heteroskedasticity – Breusch-Pagan Test # drop before or after 
# does it matter for dummies?
model_base <- lm(log(price) ~ year + 
                   mileage + 
                   tax + 
                   mpg + 
                   engineSize + 
                   manual,
                 data=audi)
summary(model_base)
bptest(model_base) # good


# RESET
resetReg <- lm(log(price) ~ year + 
                            mileage + 
                            tax + 
                            mpg + 
                            engineSize + 
                            manual +
                            I(fitted(model_base)^2) +
                            I(fitted(model_base)^3),
                data=audi)
# resettest(model_base) # smaller but less detailed
linearHypothesis(resetReg, matchCoefs(resetReg,"fitted"))



# TODO FIX N DONE

#model2 <- lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + I(NOX^2) + RM + I(RM^2) + AGE + DIS + I(DIS^2) + RAD + TAX + PTRATIO + B + LSTAT + I(LSTAT^2) , data=boston)
#summary(model2)
#bptest(model2)
#bptest(model2, ~ fitted(model2) + I(fitted(model2)^2))
#coeftest(model2, vcov = vcovHC(model2))
#resetReg2 <- lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + I(NOX^2) + RM + I(RM^2) + AGE + DIS + I(DIS^2) + RAD + TAX + PTRATIO + B + LSTAT + I(LSTAT^2) + I(fitted(model2)^2) + I(fitted(model2)^3), data=boston)
#waldtest(model2, resetReg2, vcov = vcovHC(resetReg2, type = "HC0"))




