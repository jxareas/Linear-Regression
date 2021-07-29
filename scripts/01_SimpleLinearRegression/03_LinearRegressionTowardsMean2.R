##### Import Packages ---------------------------------------------------------

require(dplyr)
require(stringr)
require(ggplot2)
require(ggthemes)

##### Loading Data ------------------------------------------------------------

df <- read.csv(file = "./datas/ToySales.csv", header = T)

names(df) <- c("Month", "UnitSales", "Price", "AdExp", "PromExp")

df$UnitSales <- df$UnitSales / 1000

# Simple Linear Regression Model ------------------------------------------
fit <- lm(data = df,
            formula = 
                    UnitSales ~ Price
                    )
sumfit <- summary(fit)


# Scatterplot with Regression Line -------------------------------------------------------------


plot <- 
        ggplot(data = df, mapping = aes(x = Price, y = UnitSales, col = UnitSales)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = T, color = "magenta") +
        coord_cartesian(xlim = c(7.25, 9)) +
        labs(
                title = "Price vs Unit Sales",
                subtitle = "Unit Sales = 114215.08 - 4913.73 * Price",
                x = "Price (U$D)",
                y = "Unit Sales (K)"
        ) +
        theme_pander() +
        theme(
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 13, colour = "darkgray", hjust = 0.5, vjust = -0.6),
                axis.title.x = element_text(size = 15, colour = "darkgreen"),
                axis.title.y = element_text(size = 15, colour = "darkgreen"),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12, face = "bold")
        ) +
        scale_color_viridis_c(option = "viridis", begin = 0.4, end = 0.8) +
        guides(col = "none") 
plot
