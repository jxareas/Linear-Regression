# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggthemes)

# Simulation --------------------------------------------------------------

set.seed(287)
n <- 100

df <-
        data.frame(
                x = c(10, rnorm(n)),
                y = c(10, c(rnorm(n)))
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

# High Leverage
hatvalues(fit) |>
        sort(decreasing = T) |>
        head(5)

# Scatterplot --------------------------------------------------------------------
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
                subtitle = "Outlier with High Influence & High Leverage",
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


