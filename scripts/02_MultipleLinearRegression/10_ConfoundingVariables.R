# Packages ----------------------------------------------------------------

library(ggplot2)


# Data --------------------------------------------------------------------

n <- 100
x2 <- 1:n
x1 <- .01 * x2 + runif(n,-.1, .1)
y <- -x1 + x2 + rnorm(n = n, mean = 0, sd = .01)

df <-
        data.frame(y, x1, x2)

multiplelm <- lm(data = df,
                 formula = y ~ x1 + x2)

simplelm <- lm(data = df,
               formula = y ~ x1)


# Plot --------------------------------------------------------------------

data <-
        data.frame(y,
                   x1,
                   x2,
                   ey = resid(lm(y ~ x2)),
                   ex1 = resid(lm(x1 ~ x2)))


ggplot(data = data, mapping = aes(x = x1, y = y , col = x2)) +
        geom_point() +
        geom_smooth(method = lm, col = "red", se = T, formula = y ~ x) +
        labs(
                title = "Simulation of a Multiple Linear Regression Model",
                subtitle = "Y = f(X1, X2) = -X1 + X2 + Normal(mu = 0, sigma = 0.01)",
                x = "X1",
                y = "Y",
                col = "X2"
        ) +
        scale_color_gradient(low = "black",
                             high = "magenta") +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", color = "dimgray",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold", color = "purple4"),
                axis.title.y = element_text(face = "bold", color = "purple4"),
                legend.text = element_text(color = "dimgray"),
                legend.title = element_text(face = "bold", hjust = .5,
                                            vjust = 5, color = "purple4")
        )
