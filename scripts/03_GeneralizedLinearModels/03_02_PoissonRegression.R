# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)

# Data --------------------------------------------------------------------

df <-
        read.csv(file = "./data/gaData.csv", header = T)


# Regression Models ------------------------------------------------------

linearfit <-
        lm(
                data = df,
                formula = visits ~ julian
        )

poissonfit <-
        glm(
                data = df,
                formula = visits ~ julian,
                family = "poisson"
        )

# Plot: Linear v Poisson Fit --------------------------------------------------------------------
plot <-
ggplot(data = df, mapping = aes(x = julian, y = visits, col = visits)) +
        geom_point() +
        geom_smooth(method = glm, method.args = list(family = "poisson"),
                    size = 2, color = "skyblue", formula = y ~ x) +
        geom_smooth(method = lm, formula = y ~ x,
                    size = 2, color = "red") +
        labs(
                title = "Poisson Fit vs Linear Fit",
                x = "Date (Julian)",
                y = "Visits"
        ) +
        theme_igray() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray")
        ) + guides(col = "none")

plot

# Poisson Regression Residuals --------------------------------------------

ggplot() +
        geom_point(mapping = aes(x = fitted(poissonfit),
                                 y = resid(poissonfit),
                                 col = resid(poissonfit)
                                )) +
        labs(
                title = "Poisson Regression Model Residuals",
                x = "Fitted Values",
                y = "Residuals"
        ) +
        scale_color_viridis_b(end = .9) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray")
        ) + guides(col = "none")


