# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggthemes)

# Data --------------------------------------------------------------------

df <- data.frame(
        "x" = runif(100, 0, 6)
) |> mutate("y" = x + rnorm(100, 0, sd = .001*x))


# Scatterplot -------------------------------------------------------------

ggplot(data = df, mapping = aes(x, y, col = y)) +
        geom_point(size = 2) +
        geom_line(stat = "smooth", method = "lm", formula = y ~ x,
                  size = 2, color = "pink", alpha = 0.8) +
        labs(
                title = "Scatterplot: Linear Trend",
                x = "X", 
                y = "Y"
        ) +
        scale_color_viridis_c(end = 0.95) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold", color = "dimgray"),
                axis.title.y = element_text(face = "bold", color = "dimgray")
        ) +
        guides(col = "none") 


# Regression Model --------------------------------------------------------

fit <- lm(
        data = df,
        formula = y ~ x
)

sumfit <- summary(fit)

# Regression Model suggests y = x is approximately the best fit
coef(fit)

# Model is perfectly multicolinear (with some error due to precision)
with(df, cor(x, y)) 

# Residuals
residualsDf <- data.frame(
        "residuals" = resid(fit),
        "x" = df$x
)

# Residual Plot -----------------------------------------------------------

ggplot(data = residualsDf, aes(x = x, y = residuals, col = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, size = 1.4, col = "red") +
        labs(
                title = "Residuals Variation",
                x = "X",
                y = "Residuals"
        ) +
        theme_economist_white() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2),
                axis.title.x = element_text(face = "bold", color = "black"),
                axis.title.y = element_text(face = "bold", color = "black")
        ) +
        guides(col = "none") 