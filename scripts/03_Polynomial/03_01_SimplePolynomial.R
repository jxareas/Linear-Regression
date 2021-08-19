# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(purrr)

# Simulation of Non-Linear Data -------------------------------------------

set.seed(287)
n_simulations <- 150

df <-
        data.frame(
                "x" = runif(n = n_simulations, -2.25, 2.25)
        ) |> 
        mutate("y" = -.5*x^3 - 2*x^2 - 0.2*x + rnorm(n_simulations, 0, 1))


# Linear Models: Line vs Polynomial ---------------------------------------

linfit <- 
        S(lm(
                data = df,
                formula = y ~ x
        ))

polyfit <-
        S(lm(
                data = df,
                formula = y ~ poly(x, 3, raw = T)
        ))


# Polynomial Fit Plot --------------------------------------------------------------------
plot <- 
        ggplot(data = df, mapping = aes(x, y)) +
        geom_point() +
        geom_smooth(method = "lm",
                    formula = y ~ x,
                    color = "green") +
        geom_smooth(method = "lm",
                    formula = y ~ poly(x, 3, raw = T),
                    color = "magenta") +
        labs(
                title = "Simple Lineal Regression",
                subtitle = "Polynomial Fit",
                x = "X",
                y = "Y"
        ) +
        theme_fivethirtyeight() +
        theme(
                plot.title = element_text(face = "bold",
                                          hjust = .5),
                plot.subtitle = element_text(face = "bold",
                                             color = "dimgrey",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        )

plot
