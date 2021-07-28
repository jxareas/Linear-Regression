# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggthemes)

# Simulation --------------------------------------------------------------

n <- 100

df <- local(
        {
                x <- rnorm(n)
                y <- x + rnorm(n = n, sd = .3)
                x <- c(5, x); y <- c(5, y)
                
                data.frame(x, y)
        }
)

# Linear Regression -------------------------------------------------------

fit <-
        lm(data = df,
           formula = y ~ x)

sumfit <-
        summary(fit)


# Diagnostics -------------------------------------------------------------

# High Influence
round(dfbetas(fit), 3) |> 
        as.data.frame() |> 
        arrange(desc(x)) |> 
        head(5)

# Low Leverage
hatvalues(fit) |>
        sort(decreasing = T) |>
        head(5)

# Plot --------------------------------------------------------------------

plot <-
        ggplot(data = df, mapping = aes(x, y, col = y, size = )) +
        geom_point(size = 3) +
        geom_smooth(
                method = lm,
                formula = y ~ x,
                col = "red",
                size = 1,
                se = T
        ) +
        labs(
                title = "Simple Linear Regression with Outlier",
                subtitle = "Outlier with High Influence & Low Leverage",
                x = "X",
                y = "Y"
        ) +
        scale_color_gradient(low = "darkblue", high = "magenta") +
        theme_economist_white() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5,
                                          vjust = 3),
                plot.subtitle = element_text(hjust = .5,
                                             vjust = 2),
                axis.title.x = element_text(face = "bold", hjust = .5,
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold", hjust = .5,
                                            color = "dimgray")
        ) + guides(col = "none")

plot
