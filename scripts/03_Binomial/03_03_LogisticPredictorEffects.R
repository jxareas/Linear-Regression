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
                formula = lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                family = "binomial" (link = logit)
        )

sumfit <-
        S(fit)


# Plotting Predictor Effects ----------------------------------------------

plot(predictorEffects(fit))
