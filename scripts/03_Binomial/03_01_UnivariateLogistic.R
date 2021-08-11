# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)


# Data --------------------------------------------------------------------

df <- read.csv("./data/Ravens.csv")

# Logistic Regression Model -----------------------------------------------


fit <-
        glm(
                formula = ravenWinNum ~ ravenScore,
                data = df,
                family = binomial
        )

sumfit <- summary(fit)


# Logistic Curve -----------------------------------------------------

ggplot() +
        geom_point(mapping = aes(x = df$ravenScore, y = fit$fitted.values,
                                 col = fit$fitted.values)) +
        geom_hline(yintercept = 1, col = "red", size = 1) +
        labs(
                title = "Logistic Regression Curve",
                x = "Score",
                y = "Probability (Ravens Win)"
        ) +
        scale_color_gradient(low = "magenta4", high = "blue") +
        theme_gdocs() +
        theme(
                plot.title = element_text(hjust = .5, face = "bold",
                                          color = "black", size = 18),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray")
        ) + guides(col = "none")


# Confidence Intervals & Odd Ratios ---------------------------------------


exp(coef(fit))

exp(confint(fit))
