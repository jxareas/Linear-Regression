# Packages ----------------------------------------------------------------

library(ggplot2)


# Data Simulation ---------------------------------------------------------

set.seed(287)
df <- 
        data.frame(
                x = 1:100,
                y = rnorm(100, 0, 25)
        )


# Plot: Normally Distributed Residuals --------------------------------------------------------------------

ggplot(data = df, mapping = aes(x, y, col = y)) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, col = "red",
                   size = 2) +
        labs(
                title = "Normally Distributed Residuals",
                subtitle = "Residuals ~ Normal(0, Sigma)",
                x = "Observation",
                y = "Residual"
        ) +
        scale_color_gradient(low = "magenta4", high = "blue") +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", color = "gray",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold", 
                                            color = "dimgray"),
                axis.title.y = element_text(face = "bold", 
                                            color = "dimgray")
        ) +
        guides(col = "none")