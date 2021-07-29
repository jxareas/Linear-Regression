# Packages ----------------------------------------------------------------

require(stringr)
require(car)

# Data --------------------------------------------------------------------


df <- read.csv(file = "./data/ToySales.csv",
               header = T)

names(df) <- gsub(x = names(df), pattern = "\\.", replacement = "")


# Multiple Linear Regression Model ----------------------------------------


fit <- lm(data = df, 
          formula = UnitSales ~ . -Month)

results <- summary(fit)


# Linear Hypothesis Test --------------------------------------------------

lh <- linearHypothesis(fit, "Adexp000 = 500")

CI <- confint(fit, level = 0.95)
