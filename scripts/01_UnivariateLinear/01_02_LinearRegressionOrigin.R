# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(UsingR)
library(dplyr)

# Data --------------------------------------------------------------------

data(galton)

frequency <- as.data.frame(table(galton$child, galton$parent)) |>
        mutate(across(everything(), .fns = ~ as.numeric(.x)))

names(frequency) <- c("child", "parent", "freq")


# Scatterplot -------------------------------------------------------------

ggplot(frequency |>
               filter(freq > 0),
       mapping = aes(
               x = parent,
               y = child,
               size = freq,
               col = freq
       )) +
        geom_point() +
        geom_smooth(
                color = "yellow",
                size = 2,
                method = "lm",
                se = T
        ) +
        scale_color_viridis_c(begin = 0.2,
                              end = 0.8,
                              option = "plasma") +
        labs(title = "Bubbleplot: Height of Parents vs Children",
             x = "Parents (Inches)",
             y = "Children (Inches)") +
        theme(
                plot.title = element_text(
                        face = "bold",
                        hjust = 0.5,
                        color = "purple4"
                ),
                axis.title.x = element_text(face = "bold.italic"),
                axis.title.y = element_text(face = "bold.italic"),
                legend.position = "none"
        )


# Mean Centering Height ---------------------------------------------------

galton <- galton |>
        mutate(#Mean Centering Variables
                mc_child = child - mean(child),
                mc_parent = parent - mean(parent))
mc_frequency <-
        as.data.frame(table(galton$mc_child, galton$mc_parent)) |>
        apply(2, as.numeric) |>
        as_tibble()

names(mc_frequency) <- c("child", "parent", "freq")


# Scatterplot: Mean Centered Variables ------------------------------------

ggplot(mc_frequency |>
               filter(freq > 0),
       mapping = aes(
               x = parent,
               y = child,
               size = freq,
               col = freq
       )) +
        geom_point() +
        geom_smooth(
                color = "yellow",
                size = 2,
                method = "lm",
                se = T
        ) +
        scale_color_viridis_c(begin = 0.2,
                              end = 0.8,
                              option = "plasma") +
        labs(title = "Bubbleplot: Height of Parents vs Children",
             x = "Parents (Inches)",
             y = "Children (Inches)") +
        theme(
                plot.title = element_text(
                        face = "bold",
                        hjust = 0.5,
                        color = "purple4"
                ),
                axis.title.x = element_text(face = "bold.italic"),
                axis.title.y = element_text(face = "bold.italic"),
                legend.position = "none"
        )


# Simple Linear Regression Model ------------------------------------------

data(galton)
galton <- galton |> 
        mutate(
                mc_child = child - mean(child),
                mc_parent = parent - mean(parent)
        )

# Simple Linear Regression
# B0 ~ 23.94 (Not equal to zero)
# B1 ~ 0.6463
model <- lm(
        data = galton,
        formula = child ~ parent
)


# Forcing Regression through the Origin
# B0 = 0. (Assumed by the model)
# B1 ~ 0.6463
origin_model <-
        lm(data = galton,
           formula = I(child - mean(child)) ~ 0 + I(parent - mean(parent))) 

# Regression using Mean Centered Variables
# B0 is approximately 0.
# B1 ~ 0.6463
mc_model <- 
        lm(data = galton,
           formula = mc_child ~ mc_parent)
