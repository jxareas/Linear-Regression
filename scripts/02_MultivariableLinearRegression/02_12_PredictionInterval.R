# Packages ----------------------------------------------------------------

require(stringr)
require(dplyr)

# Data --------------------------------------------------------------------

df <-
        read.csv(
                file = "./data/GrocerySales.csv",
                header = T
        )

names(df) <-
        c("StoreID", "SalesSquareFoot", "SizeStore", "Advertising", "NumberProducts")

# Multiple Linear Regression Model ----------------------------------------

fit <- df |> 
        select(-StoreID) |> 
        lm(
                formula = SalesSquareFoot ~ .
        )

result <- summary(fit)


# Prediction --------------------------------------------------------------


# What would be the expected Sales per Square Foot if
# the Size of Store was 60,000 square feet, 
# they spent $70,000 in Advertising Dollars, 
# and offered 30,000 products (in $) ?   

prediction <- predict(object = fit, interval = "prediction",
              newdata = data.frame(
                      "SizeStore" = 6e4,
                      "Advertising" = 7e4,
                      "NumberProducts" = 3e4
              )
              )
