# Packages ----------------------------------------------------------------

library(datasets)
library(ggplot2)
library(GGally)

# Data --------------------------------------------------------------------

data(swiss)
 
ggpairs(swiss, lower = list(continuous = wrap("smooth", method = "lm", col = "blue4")))

# Simple Linear Regression: Fertility ~ Agriculture -----------------------

fit <- 
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture
        )

sumfit <- 
        summary(fit)

# Multiple Linear Regression: Fertility ~ . ----------------------------------------------

fit <-
        lm(
                data = swiss,
                formula = Fertility ~ .
        )

sumfit <- 
        summary(fit)



