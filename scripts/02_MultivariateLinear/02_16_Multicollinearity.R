##### Packages ----------------------------------------------------------------

require(dplyr)


##### Data --------------------------------------------------------------------

df <- read.csv("./data/Cars.csv") |>
        select(-Car_Model)


##### Multiple Linear Regression Model ----------------------------------------------

# Coefficients are not significant.
mreg <-
        lm(data = df,
           formula = MPG  ~ .)


##### Simple Linear Regression ------------------------------------------------

# Coefficients are significant in both simple linear regression models
regDisp <-
        lm(data = df,
           formula = MPG ~ Displacement)

regCyl <- 
        lm(data = df,
           formula = MPG ~ Cylinders)



##### Multicollinearity -------------------------------------------------------

# High Correlation between Cylinders and Displacement
X <- cor(mreg$model |> select(-MPG)) 

## Determinant is almost 0, implying approximate multicollinearity.
all.equal(target = 0, current = det(t(X) %*% X)) 
