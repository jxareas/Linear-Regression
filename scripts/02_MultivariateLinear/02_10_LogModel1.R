# Packages ----------------------------------------------------------------

require(dplyr)
require(stringr)
require(gridExtra)
require(ggplot2)


# Data --------------------------------------------------------------------

df <- read.csv(file = "./data/RealEstate.csv")

names(df) <-
        names(df) |>
        gsub(pattern = "\\.", replacement = "")  |>
        str_to_title()


# Linear Relationship -----------------------------------------------------

grid.arrange(
        ggplot(data = df, aes(
                x = Sqft, y = Price, col = Price
        )) +
                geom_point() +
                geom_smooth(method = "lm", col = "red", formula = y ~ x) +
                labs(
                        title = "Sqft v Price",
                        subtitle = paste0("Correlation: ", cor(df$Sqft, df$Price)
                                          |> round(2)),
                        x = "Square Feet",
                        y = "Price"
                ) +
                theme(
                        plot.title = element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(
                                hjust = 0.5,
                                face = "italic",
                                color = "gray"
                        )
                ) + guides(col = "none") +
                scale_color_viridis_c(),
        ggplot(data = df, aes(
                x = Sqft, y = Price, col = Price
        )) +
                geom_point() +
                geom_smooth(method = "lm", col = "blue", formula = y ~ x) +
                scale_x_log10() +
                scale_y_log10() +
                labs(
                        title = "Log(Sqft) v Log(Price)",
                        subtitle = paste0("Correlation: ", cor(log(df$Sqft),
                                                               log(df$Price))
                                          |> round(2)),
                        x = "log(SquareFeet)",
                        y = "log(Price)"
                ) +
                theme(
                        plot.title = element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = element_text(
                                hjust = 0.5,
                                face = "italic",
                                color = "gray"
                        )
                ) + guides(col = "none") +
                scale_color_viridis_c(),
        nrow = 1
)

# Multiple Linear Regression Model ----------------------------------------

# LN(PRICE) = β0 + β1LN(SQFT) + β2BED + β3BATH + β4FLOORS + β5DIST

fit <- lm(data = df,
            formula = log(Price) ~ log(Sqft) + Bed + Bath + Floors + Distkm)

summodel <- summary(fit)


# Quiz Solution -----------------------------------------------------------

# Question 1: B4 value
# Report the estimated value of β4, round the answer to four decimal digits.
coef(fit)[5]

# Question 2: Prediction
# Using the estimated regression model, predict the price in dollars
# of an apartment that is 1000 sqft in size, has 2 Bedrooms, 2 Bathrooms, 
# is in a building with 8 Floors and is 1.2 Km from the City Park. 
# Round your answer to a whole number.

coef(fit) %*% c(1, log(1000), 2, 2, 8, 1.2) |> exp() |> round(0)

# Question 3: Confidence Bounds for Prediction
# Calculate a 95% confidence interval for your predicted price
predict.lm(
        object = fit,
        newdata= df[1, ] |> 
                select(-Price) |> 
                within({
                        Sqft <- 1000
                        Bed <- 2
                        Bath <- 2
                        Floors <- 8
                        Distkm <- 1.2
                }),
        interval = "prediction",
        level = 0.95
) |> exp()
