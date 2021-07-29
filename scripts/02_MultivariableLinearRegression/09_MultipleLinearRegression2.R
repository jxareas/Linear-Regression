# Import and Load Data ----------------------------------------------------


# A consultancy wants to analyze what is the relationship between the profits of a startup
# and variables such as spending on research and development, spending on administration
# and spending on market research.
# For this purpose, information was collected on the profit and expenditure of
# 50 companies from the US, from the states of New York, California and
# Florida.
# The information is in the file “Startups.csv”.

df <- read.csv("./data/Startups.csv")

names(df) <-
        gsub(x = names(df),
             pattern = "\\.",
             replacement = "")



# Multiple Linear Regression Model ----------------------------------------

fit <- lm(data = df,
          formula = Profit ~ . - State)

sumfit <- summary(fit)
aovt <- anova(fit)


# Prediction using the Predict Function --------------------------------------------------------------

# Observed Values
v <- list(78013.11,	121597.55,	264346.06, "New York",	126992.93) |>
        as.data.frame()

names(v) <- names(df)


# Profit Prediction
prediction <-
        predict(fit, newdata = v) |>
        as.numeric()



# Prediction using Vector Multiplication  ---------------------------------

prediction <-
        fit$coefficients %*% c(1, 78013.11,	121597.55,	264346.06) |>
        as.numeric()
