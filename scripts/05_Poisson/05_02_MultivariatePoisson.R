### Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(car)

### Data Wrangling ----------------------------------------------------------

# Poisson Regression Model to explain & predict the total number
# of cyclists on the Manhattan Bridge on any given day.

df <-
        read.csv("./data/bike.csv", header = T)

# Checking for NA's. There are no NA values in the dataset.
any(is.na(df))

# Notice that:
# I) Precipitation is a character variable
# II) Date is a character variable
# III) Day is a character variable
str(df)

# I)  Precipitation
# Why is precipitation a character?
table(df$precip)

# Recoding "T"
df <- df |>
        mutate(precip = case_when(precip == "T" ~ 0,
                                  TRUE ~ as.numeric(precip)))

# II) Changing Date format
df <-
        df |>
        mutate(date = paste0(date, "/17") |>
                       as.Date(format = "%m/%d/%y"))


# Train-Test Split --------------------------------------------------------

set.seed(8585)

bound <- floor(nrow(df) * .8) # Percentage of the Training Set

# Scramble the Df
sample <-
        df[sample(nrow(df)), ]

train <-
        sample[1:bound, ]

test <-
        sample[(bound + 1):nrow(sample), ]


# Multivariate Poisson Regression Model: Train Data -----------------------------------

fit_train <-
        glm(
                formula = mb ~ precip + temp_h + temp_l + day,
                data = train,
                family = "poisson" (link = "log")
        )

sumfit <-
        S(fit_train)

# Interpretation of the Coefficients of the Regression Model
# as Percentage Increase
increase <-
        exp(coef(fit_train)) - 1
round(increase, 3)



# Train Set Data: Y vs Fitted Values ---------------------------------------------

fitted_train <-
        data.frame("Y" = fit_train$model$mb,
                   "Fitted Values" = fit_train$fitted.values)

ggplot(data = fitted,
       mapping = aes(x = y_train, y = mu_train, col = mu_train)) +
        geom_point() +
        geom_smooth(method = "lm",
                    formula = y ~ x,
                    col = "red") +
        labs(
                title = "Model Fit",
                subtitle = "Multivariate Poisson Regression Model",
                x = "Predicted Values",
                y = "Observed Values"
        ) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", color = "gray",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        ) + guides(col = "none")



# Predicting Test Data ----------------------------------------------------

fitted_test <-
        data.frame("test_prediction" = predict(fit,
                                               test, type = "response"),
                   "test_mu" = test$mb)

ggplot(data = fitted_test,
       mapping = aes(x = test_prediction,
                     y = test_mu,
                     col = test_mu)) +
        geom_point() +
        geom_smooth(
                method = "lm",
                formula = y ~ x,
                color = "red",
                size = 1,
                se = T
        ) +
        labs(
                title = "Predicting Test Data",
                subtitle = "Multivariate Poisson Regression Model",
                x = "Fitted Values",
                y = "Test Mean"
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", color = "gray",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        ) + guides(col = "none")
