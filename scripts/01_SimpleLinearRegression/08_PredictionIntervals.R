# Packages ----------------------------------------------------------------

library(UsingR)
library(ggplot2)
library(dplyr)

# Data Wrangling --------------------------------------------------------------------

data(diamond)
fit <- lm(data = diamond, formula = price ~ carat)

data <- with(diamond,
             data.frame("carat" =seq(min(carat), max(carat), length = 100)))


confidence <-
        data.frame(predict(fit, newdata = data, interval = "confidence")) |> 
        mutate(interval = "Confidence",
               carat = data) 

prediction <-
        data.frame(predict(fit, newdata = data, interval = "prediction")) |> 
        mutate(interval = "Prediction",
               carat = data)


data <- rbind(confidence, prediction) |> 
        mutate(across(everything(), ~ unlist(.x)))

# Prediction and Confidence Interval Plot ---------------------------------

ggplot(data = data, mapping = aes(x = carat, y = fit)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) +
        geom_line() +
        geom_point(data = diamond, mapping = aes(x = carat, y = price), size = 2) +
        labs(title = "Simple Linear Regression Model: Diamond Price",
             subtitle = "Confidence Interval v Prediction Interval",
             x = "Mass (Carat)",
             y = "Price (SGD)",
             caption = "Dataset: Diamond (UsingR)",
             fill = "Interval") +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(face = "italic", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) 
