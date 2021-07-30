# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)

# Data --------------------------------------------------------------------

df <-
        read.csv(file = "./data/gaData.csv", header = T)


# Poisson Regression Model ------------------------------------------------------

fit <-
        glm(
                data = df,
                formula = visits ~ julian,
                family = "poisson"
        )

# Plot --------------------------------------------------------------------

ggplot(data = df, mapping = aes(x = julian, y = visits, col = visits)) +
        geom_point() +
        geom_smooth(method = glm, method.args = list(family = "poisson"),
                    size = 2, color = "skyblue", formula = y ~ x) +
        labs(
                title = "Poisson Regression Curve",
                x = "Date (Julian)",
                y = "Visits"
        ) +
        theme_hc() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray")
        ) + guides(col = "none")



