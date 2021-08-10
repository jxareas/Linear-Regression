# Packages ----------------------------------------------------------------

require(dplyr)

# Data --------------------------------------------------------------------

df <- read.csv(file = "./data/Delivery2.csv", header = T)

df <- 
        df |> mutate(DummyRegionA = as.numeric(Region == "A"), 
             DummyRegionB = as.numeric(Region == "B"),
             .after = Region
             )


# Multiple Linear Regression Model ----------------------------------------

fit <- lm(data = df,
            formula = Minutes ~ . - Region -Trip)

summodel <- summary(fit)

aovtable <- anova(fit)


# Prediction --------------------------------------------------------------

# Predict time to deliver 25 parcels across all regions with a truck that
# is 10 years old.

prediction <-
        c(
        "A" = coef(fit) %*% c(1, 1, 0, 25, 10),
        "B" = coef(fit) %*% c(1, 0, 1, 25, 10),
        "C" = coef(fit) %*% c(1, 0, 0, 25, 10)
)
