# Packages ----------------------------------------------------------------

require(dplyr)

# Data --------------------------------------------------------------------

df <- read.csv(file = "./data/Cocoa.csv")

# Multiple Linear Semi-Logarithmic Regression Model ---------------------------------------

fit <- lm(data = df,
          formula = Demand ~ .)

# Each coefficient is interpreted as an economic elasticity
logfit <- lm(data = df,
             formula = log(Demand) ~ log(Price) + log(Pcinc) + Year)


# Prediction Interval with Log Model -----------------------------------------------------

# Predict Demand for Year 2009, Price 1.15$ and Pcinc 40'000$

Prediction <-
        local({
                # In terms of log
                logprediction <-
                        predict.lm(
                                object = logfit,
                                interval = "prediction",
                                newdata = df[1,] |>
                                        select(-Demand) |>
                                        within({
                                                Year <- 2009
                                                Price <- 1.15
                                                Pcinc <- 40000
                                        })
                        )
                
                # In terms of mLbs
                prediction <- exp(x = logprediction)
                
                df <- data.frame(Map(t, list(logprediction, prediction)))
                names(df) <- c("logprediction", "prediction")
                df
        })
