# Packages ----------------------------------------------------------------

library(carData)
library(car)
library(effects)


# Data --------------------------------------------------------------------

data("Prestige")
?Prestige


# Multivariate Linear Regression Model ------------------------------------

fit <-
        lm(
                prestige ~ education + log2(income) + women,
                data = Prestige
        )

sumfit <-
        S(fit)



# Predictor Effects Plot --------------------------------------------------

plot(predictorEffects(fit))

