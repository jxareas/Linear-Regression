# Packages ----------------------------------------------------------------

require(ggplot2)
require(UsingR)
require(tidyr)
require(dplyr)
require(ggthemes)
require(stringr)

# Data --------------------------------------------------------------------

data(galton)

galton <- galton |>
        pivot_longer(everything(),
                     names_to = "status",
                     values_to = "height") |>
        mutate(status = str_to_title(status))

# Boxplot --------------------------------------------------------

ggplot(data = galton,
       mapping = aes(x = status, y = height, fill = status)) +
        stat_boxplot(geom = "errorbar", size = 0.8) +
        geom_boxplot(size = 0.8) +
        labs(title = "Height of Children & Parents",
             x = "Status",
             y = "Height") +
        scale_fill_viridis_d(option = "magma", begin = 0.8) +
        theme_solarized() +
        theme(
                plot.title = element_text(face = "bold",
                                          color = "black",
                                          hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) +
        guides(fill = "none")

# Histogram --------------------------------------------------------------------

ggplot(data = galton, mapping = aes(x = height, fill = status)) +
        geom_histogram(binwidth = 1, color = "black") +
        facet_wrap(. ~ status) +
        scale_fill_viridis_d(option = "cividis", alpha = 0.6) +
        labs(title = "Height Histogram",
             x = "Height (Inches)",
             y = "Frequency",
             fill = "Status") +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) +
        guides(fill = "none")


