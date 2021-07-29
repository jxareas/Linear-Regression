# Packages ----------------------------------------------------------------

require(dplyr)

# Data --------------------------------------------------------------------

df <- read.csv(
        file = "./data/Cocoa.csv"
) 

# Multiple Linear Semi-Logarithmic Regression Model ---------------------------------------

linmodel <- lm(data = df,
            formula = Demand ~ .)

# Each coefficient is interpreted as an economic elasticity
linlogmodel <- lm(data = df,
            formula = log(Demand) ~ log(Price) + log(Pcinc) + Year)

sumlinmodel <- summary(linmodel)
sumlinlogmodel <- summary(linlogmodel)


# Prediction Interval with Log Model -----------------------------------------------------

# Predict Demand for Year 2009, Price 1.15$ and Pcinc 40'000$


# In terms of log
logprediction <- 
        predict.lm(
        object = linlogmodel,
        interval = "prediction",
        newdata = df[1, ] |> 
                select(-Demand) |>
                within({Year <- 2009 
                        Price <- 1.15
                        Pcinc <- 40000})
        ) 

# In terms of mLbs
prediction <- exp(x = logprediction)


