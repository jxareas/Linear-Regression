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
plot <- ggplot() +
        geom_point(mapping = aes(x = df$ravenScore, y = fit$fitted.values,
                                 col = fit$fitted.values), size = 2) +
        geom_hline(yintercept = 1, col = "red", size = 1) +
        geom_hline(yintercept = 0, col = "red", size = 1) +
        labs(
                title = "Sigmoid Curve",
                x = "Score",
                y = "Probability (Ravens Win)"
        ) +
        coord_cartesian(ylim = c(0, 1), xlim = c(0, 70)) +
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

plot

# Confidence Intervals & Odd Ratios ---------------------------------------

# Odd Ratios 
odds <-
        exp(coef(fit))

# Confidence Intervals for the Odds
confint_odds <-
        exp(confint(fit))
