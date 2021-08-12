# Packages ----------------------------------------------------------------

library(ggplot2)

# Data --------------------------------------------------------------------
set.seed(287)
# Vector of r-squared values
rsquare <- c()

for (n_predictors in 2:200)
{
        n_observations <- 200
        variable_range <- 1:n_predictors
        
        Y <- rpois(n = n_observations, lambda = 50)
        
        for (i in variable_range <- 1:n_predictors)
        {
                assign(paste0("x", i),
                       value = rnorm(
                               n = n_observations,
                               mean = i,
                               sd = 3.14 * i
                       ))
        }
        
        
        get_variables <- Vectorize(get)
        
        df <-
                cbind(Y,
                      get_variables(paste0("x", variable_range))) |>
                as.data.frame()
        
        fit <-
                lm(data = df,
                   formula = Y ~ .)
        
        R <-
                summary(fit)$r.squared
        
        rsquare[n_predictors - 1] <- R
        
}

# Removing all x-variables
rm(list = c(
        paste0("x", variable_range),
        "i",
        "n_observations",
        "R", "fit", "df", "variable_range"
))

# Scatterplot: Number of Predictors vs R-Square ---------------------------
plot <-
        ggplot(mapping = aes(
                x = 2:n_predictors,
                y = rsquare,
                col = rsquare
        )) +
        geom_line(size = 1.3) +
        labs(title = "R-square Increase",
             x = "Number of Predictors",
             y = "R-square") +
        scale_color_gradient(low = "purple", high = "orange") +
        theme_gray() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        ) +
        guides(col = "none")

plot
