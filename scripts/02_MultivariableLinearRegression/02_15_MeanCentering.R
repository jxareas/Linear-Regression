# Packages ----------------------------------------------------------------

require(dplyr)
require(ggplot2)

# Data --------------------------------------------------------------------

df <- read.csv("./data/Heights&Weights.csv") |> 
        mutate(Gender = as.factor(Gender)) |> 
        within(Gender <- relevel(Gender, ref = "W")) |>  # Change reference category to Women
        mutate(Gender = ifelse(Gender == "W", "Female", "Male"))

names(df) <- 
        names(df) |> 
        gsub(pattern = "\\.", replacement = "")

names(df)[4:5] <- c("Height", "Weight")

df <- df |> 
        mutate(MeanCenteredHeight = Height - mean(Height),
               .after = Height)

# Multiple Linear Regression Model ----------------------------------------

# Weight = B0 + B1*Male + B2*Height

fit <- lm(data = df,
            formula = Weight ~ Gender + MeanCenteredHeight)

summodel <- summary(fit)

aovtable <- anova(fit)

# Interpretation of B0 is meaningful.
# B0 => Y|X = 0
# B0: Weight of a female athlete with average height (1.77cm)
fit

# Scatterplot: Height v Weight --------------------------------------------

plot <- ggplot(data = df, mapping = aes(x = MeanCenteredHeight, y = Weight,
                                        fill = Gender, col = Gender)) +
        geom_point(shape = 21) +
        geom_smooth(mapping = aes(col = Gender, y = predict(fit, df)), method = lm, 
                    formula = y ~ x, size = 3, se = F) +
        labs(
                title = "Height vs Weight by Gender",
                subtitle = "Parallel Slopes Model",
                x = "Mean Centered Height (cm)",
                y = "Weight (kg)"
        ) +
        scale_color_manual(values = c("magenta4", "blue4")) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = .5, face = "bold",
                                             color = "gray"),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgray"),
                legend.title = element_text(face = "bold",
                                            color = "dimgray")
        )

plot

