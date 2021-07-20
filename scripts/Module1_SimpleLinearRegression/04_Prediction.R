### Packages ----------------------------------------------------------------

require(UsingR)
require(ggplot2)

### Data --------------------------------------------------------------------

data(diamond)

### Scatterplot: Simple Linear Regression -------------------------------------------------------------

ggplot(data = diamond,
       mapping = aes(x = carat, y = price, col = price)) +
        geom_point(size = 2) +
        geom_line(
                stat = "smooth",
                method = "lm",
                alpha = 0.3,
                formula = y ~ 0 + x,
                color = "magenta",
                size = 2,
                linetype = "solid"
        ) + #Regression through the Origin
        geom_line(
                stat = "smooth",
                method = "lm",
                alpha = 0.5,
                formula = y ~ x,
                color = "skyblue",
                size = 2,
                linetype = "solid"
        ) + #Regression towards the Mean
        labs(
                title = "Scatterplot: Diamond Mass v Price",
                x = "Mass (Carats)",
                y = "Price (SGD)",
                caption = "Dataset: Diamond (UsingR)"
        ) +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold", color = "dimgray"),
                axis.title.y = element_text(face = "bold", color = "dimgray")
        ) +
        scale_color_viridis_c(begin = 0,
                              end = 0.9,
                              option = "plasma") +
        guides(color = "none")



### Simple Linear Regression Model ------------------------------------------

fit <- lm(data = diamond,
          formula = price ~ carat)

sumfit <- summary(fit)


### Simple Linear Regression Model: Mean-Centered Predictor ------------------

fit <- lm(data = diamond,
          formula = price ~ I(carat - mean(carat)))

sumfit <- summary(fit)


### Simple Linear Regression Model: Scaled Predictor ------------------------

# Scaling the Mass of Diamond unit of measurement
# from Carat to Tenth of a Carat

fit <- lm(data = diamond,
          formula = price ~ I(carat * 10))

sumfit <- summary(fit)


### Prediction --------------------------------------------------------------

# Predict the price of diamonds given the following masses (in carats)
# of: 0.16, 0.27, 0.34

# Regression Model
fit <- lm(data = diamond,
          formula = price ~ carat)

sumfit <- summary(fit)

# Prediction using the Regression Model
predict(object = fit,
        newdata = data.frame("carat" = c(0.16, 0.27, 0.34)))

### Scatterplot: Prediction -------------------------------------------------

ggplot(data = diamond,
       mapping = aes(x = carat, y = price, col = price)) +
        geom_point(size = 2) +
        stat_smooth(geom = "smooth", method = "lm", formula = y ~ x,
                    color = "violet") +
        geom_point(
                data = data.frame("carat" = diamond$carat,
                                  "predictedPrice" = as.numeric(predict(fit))),
                mapping = aes(x = carat, y = predictedPrice),
                color = "red",
                size = 2,
                alpha = 0.4
        ) +
        labs(
                title = "Scatterplot: Diamond Mass v Price",
                x = "Mass (Carats)",
                y = "Price (SGD)",
                caption = "Dataset: Diamond (UsingR)"
        ) +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold", color = "dimgray"),
                axis.title.y = element_text(face = "bold", color = "dimgray")
        ) +
        guides(col = "none")


