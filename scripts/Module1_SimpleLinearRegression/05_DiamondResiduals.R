# Packages ----------------------------------------------------------------

require(UsingR)
require(ggplot2)
require(ggthemes)

# Data --------------------------------------------------------------------

data(diamond)

# Simple Linear Regression Model ------------------------------------------

# Regression Model
fit <- lm(data = diamond,
          formula = price ~ carat)

sumfit <- summary(fit)


# Residuals ---------------------------------------------------------------

residualdf <- data.frame("residuals" = resid(fit),
                         "carat" = diamond$carat
                         )

# Residual Plot -----------------------------------------------------------

ggplot(data = residualdf,
       mapping = aes(
               x = carat,
               y = residuals,
               col = residuals
       )) +
        geom_point(size = 2) +
        geom_hline(
                yintercept = 0,
                color = "red",
                size = 0.8,
                linetype = "solid"
        ) +
        geom_linerange(aes(x = carat, ymax = residuals, ymin = 0),
                       linetype = "dashed", col = "gray", size = 0.75) +
        scale_y_continuous(breaks = seq(-75, 75, 25)) +
        labs(title = "Residual Variation",
             x = "Mass (Carats)",
             y = "Residual") +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) + guides(col = "none")
