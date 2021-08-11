# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(data.table)

# Data --------------------------------------------------------

data <-
        fread("./data/orings.csv") 

# The 1986 crash of the space shuttle Challenger was linked to failure of 
# O-ring seals in the rocket engines. 
# Data was collected on the 23 previous shuttle missions.

# Temp: temperature at launch in degrees F
# Damage: number of damaged o-ring seals out of n = 6  possible seals,
# for all i = 1, ..., 23 .

# Creating Dataframe of damaged vs non-damaged o-rings
df <- 
        data.frame("DamagedORings" = data$damage, 
                   "NonDamagedORings" = 6 - data$damage)


# Univariate Logistic Regression Model ------------------------------------

fit <-
        glm(cbind(data$damage, 6 - data$damage) ~ temp,
            data = data,
            family= "binomial"
            )

sumfit <-
        summary(fit)


# Sigmoid Curve -----------------------------------------------------------

ggplot() +
        geom_point(mapping = aes(x = fit$data$damage, y = fit$fitted.values,
                                 col = fit$fitted.values)) +
        geom_hline(yintercept = 1, col = "red", size = 1) +
        labs(
                title = "Logistic Regression Curve",
                x = "Score",
                y = "Probability (Ravens Win)"
        ) +
        scale_color_gradient(low = "magenta4", high = "blue") +
        theme_gdocs() +
        theme(
                plot.title = element_text(hjust = .5, face = "bold",
                                          color = "black", size = 18),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray")
        ) + guides(col = "none")


