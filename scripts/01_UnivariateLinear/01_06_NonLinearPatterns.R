# Packages ----------------------------------------------------------------

require(ggplot2)
require(ggthemes)
require(dplyr)

# Simulation of Non-linear Data --------------------------------------------------------------

set.seed(281)
df <- data.frame(
        "x" = runif(n = 100, min = -3, max = 3)) |> 
        mutate(
                y = x + sin(x) + rnorm(n = 100, mean = 0, sd = .2)
        )



# Scatterplot -------------------------------------------------------------

ggplot(data = df, mapping = aes(x = x, y = y, col = y)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x,
                    size = 1.4, col = "red", se = T) +
        labs(
                title = "Scatterplot",
                x = "X",
                y = "Y"
        ) +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold.italic"),
                axis.title.y = element_text(face = "bold.italic")
        ) +
        guides(col = "none") 

# Regression Model ---------------------------------------------------------------

# Simple Linear Regression
fit <- lm(
        data = df,
        formula = y ~ x
)

sumfit <- summary(fit)

# R-Squared of approximately 0.97
sumfit$r.squared

# Residuals
residualsDf <- data.frame("residuals" = fit$residuals,
                          "x" = df$x
                          )


# Residuals Plot ----------------------------------------------------------

ggplot(data = residualsDf, mapping = aes(x = x, y = residuals, col = residuals)) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, size = 1.3, col = "red") +
        labs(
                title = "Residual Variation",
                x = "X",
                y = "Residual"
        ) +
        scale_color_gradient(low = "black", high = "skyblue") +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) +
        guides(col = "none") 