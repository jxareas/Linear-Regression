# Packages ----------------------------------------------------------------

require(dplyr)

# Data --------------------------------------------------------------------


df <- read.csv(file = "./data/Delivery.csv", header = T)

# Dummy Variable
df <- df |> mutate(DummyRegionA = ifelse(Region == "A", 1, 0),
                   .after = Region) |>
        select(-Trip)


# Multiple Linear Regression Model ----------------------------------------

fit <- lm(data = df,
            formula = Minutes ~ . - Region)

result <- summary(fit)

aovtable <- anova(fit)

fit
