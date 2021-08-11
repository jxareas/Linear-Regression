# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)

# Data --------------------------------------------------------------------

df <- 
        fread("./data/datatest.csv") |> 
        select(Temperature, Humidity, Light,
               CO2, Occupancy)


# Multivariate Logistic Regression Model -----------------------------------------------

fit <-
        glm(
        formula = Occupancy  ~ .,
        data = df,
        family = "binomial"
        )

sumfit <-
        summary(fit)

