# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggfortify)


# Data --------------------------------------------------------------------

data(swiss)

# Linear Regression Model -------------------------------------------------

fit <-
        lm(
                data = swiss, 
                formula = Fertility ~ .
        )


# Diagnostic Plots -------------------------------------------------------------------

autoplot(fit, which = 1:6, colour = "blue4",
         smooth.colour = "red",
         ad.colour = "black",
         label.size = 3, label.n = 3, label.colour = "red3",
         ncol = 3) +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.y = element_text( color = "red4"),
                axis.title.x = element_text(color = "dimgray")
        ) 
