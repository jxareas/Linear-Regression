# Packages ----------------------------------------------------------------

library(carData)
library(car)
library(effects)


# Data --------------------------------------------------------------------

data("Mroz")
?Mroz


# Logistic Regression Fit -------------------------------------------------

fit <-
        glm(
                data = Mroz,
                formula = lfp ~ .,
                family = "binomial" (link = logit)
        )

sumfit <-
        S(fit)


# Plotting Predictor Effects ----------------------------------------------

plot(predictorEffects(fit))
