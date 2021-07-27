# Packages ----------------------------------------------------------------

library(datasets)
library(ggplot2)
library(dplyr)

# Data --------------------------------------------------------------------

data(swiss)

swiss <-
        swiss |> 
        mutate(
                Religion = ifelse(Catholic > 50, "Catholic", "Protestant") 
        )


# Scatterplot -------------------------------------------------------------

scatterplot <-
        ggplot(data = swiss, mapping = aes(x = Agriculture, 
                                           y = Fertility,
                                           col = Religion)) +
        geom_point() +
        scale_color_brewer(type = "qual", palette = 2) +
        theme_light() +
        labs(
                x = "% of males involved in Agriculture",
                y= "Standardized Fertility",
                caption = "Dataset: Swiss (R)"
        ) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(color = "dimgray", hjust = .5),
                plot.caption = element_text(color = "gray"),
                axis.title.x = element_text(face = "bold", color = "dimgray"),
                axis.title.y = element_text(face = "bold", color = "dimgray"),
                legend.title = element_text(face = "bold", color = "dimgray")
        )

scatterplot

# Simple Linear Regression: Fertility ~ Agriculture -----------------------

simplefit <-
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture
        )


# Plot: Simple Linear Regression --------------------------------------------------------------------

simplelmplot <-
        scatterplot +
        geom_abline(intercept = simplefit$coef[1], slope = simplefit$coef[2], 
                    col = "black", size = 2)  +
        labs(
                title = "Simple Linear Regression Model",
                subtitle = "Fertility ~ Agriculture",
        ) 

simplelmplot

# Parallel Slopes Model: Fertility ~ Agriculture + Religion -----------------------
        
dummyfit <- 
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture +
                        relevel(factor(Religion), ref = "Protestant")
        )


# Plot: Parallel Slopes --------------------------------------------------------------------

# Lines with the same slope and different y-intercept (Parallel)

dummylmplot <-
        scatterplot +
        geom_abline(
                intercept = coef(dummyfit)[1] + coef(dummyfit)[3],
                slope = coef(dummyfit)[2],
                color = "darkgreen",
                size = 2
        ) +
        geom_abline(
                intercept = coef(dummyfit)[1],
                slope = coef(dummyfit)[2],
                color = "orange3",
                size = 2
        ) +
        labs(
                title = "Parallel Slopes Model",
                subtitle = "Fertility ~ Agriculture + Religion"
        ) 



# Model with Interaction: Fertility ~ Agriculture * Religion --------------

interactionfit <- 
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture *
                        relevel(factor(Religion), ref = "Protestant")
        )


# Plot: Interaction Effects -----------------------------------------------

# Different y-intercept and different-slopes
interactionlmplot <-
        scatterplot +
        geom_abline(
                intercept = coef(interactionfit)[1] + coef(interactionfit)[3],
                slope = coef(interactionfit)[2] + coef(interactionfit)[4],
                color = "darkgreen",
                size = 2
        ) +
        geom_abline( 
                intercept = coef(interactionfit)[1],
                slope = coef(interactionfit)[2],
                color = "orange", 
                size = 2
        ) +
        labs(
                title = "Linear Regression with Interaction Effects",
                subtitle = "Fertility ~ Agriculture * Religion"
        ) 
        
interactionlmplot
