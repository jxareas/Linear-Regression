# Packages ----------------------------------------------------------------

require(UsingR)
require(ggplot2)
require(ggthemes)
require(dplyr)

# Data --------------------------------------------------------------------

data(father.son)

father.son <- father.son |>
        Filter(f = is.numeric) |>
        scale() |>
        as.data.frame()

# Plot --------------------------------------------------------------------

# Simple Linear Regression
# Y: Son's Height
# X: Father's Height
ggplot(data = father.son, mapping = aes(x = fheight, y = sheight, col = sheight)) +
        geom_point() +
        labs(
                title = "Simple Linear Regression Plot",
                x = "Father Height",
                y = "Son Height"
        ) +
        geom_smooth(method = "lm", se = T, col = "red", formula = y ~ x) +
        scale_color_gradient(low = "blue4", high = "pink") +
        xlim(-3, 3) +
        ylim(-3, 3) +
        geom_vline(xintercept = 0, col = "black", alpha = 0.5) +
        geom_hline(yintercept = 0, col = "black", alpha = 0.5) +
        theme_igray() +
        theme(
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                axis.title.x = element_text(face = "bold.italic"),
                axis.title.y = element_text(face = "bold.italic")
        ) +
        guides(col = "none")


# Simple Linear Regression Model ------------------------------------------

fit <- lm(
        data = father.son,
        formula = sheight ~ fheight
)

sumfit <- summary(fit)

# Intercept is not statistically significant.
# Son's Height is statistically significant
