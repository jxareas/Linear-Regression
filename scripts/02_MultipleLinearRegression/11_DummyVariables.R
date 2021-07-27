### Packages ----------------------------------------------------------------

library(datasets)
library(stats)
library(ggplot2)

### Data --------------------------------------------------------------------

data("InsectSprays")


### Violin plot -------------------------------------------------------------

ggplot(data = InsectSprays, mapping = aes(x = spray, y = count, fill = spray)) +
        geom_violin(alpha = .5, size = 2) +
        stat_summary(fun=mean, geom="point",
                     aes(shape = "Mean"),
                     size = 3, color = "red") +
        labs(
                title = "Violin Plot: Insect Deaths by Spray",
                x = "Type of Spray",
                y = "Insect Count",
                fill = "Spray",
                shape = "Statistic"
        ) +
        scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold", 
                                            colour = "dimgray"),
                axis.title.y = element_text(face = "bold", 
                                            colour = "dimgray"),
                legend.title = element_text(face = "bold",
                                            colour = "dimgray"),
                legend.position = "right"
        ) + guides(fill = "none") 

### Multiple Linear Regression Model with Dummy Variables -------------------

# The coefficients of this linear regression model are the difference in means
# of the insect counts of each category with respect to the mean of the
# reference category A.
fit <-
        lm(
                data = InsectSprays,
                formula = count ~ spray
        )

sumfit <- summary(fit)


# Mean by each group
compare <- InsectSprays |> 
        group_by(spray) |> 
        summarise(mean = mean(count)) 

# Substracting the mean of the reference category to all other categories
compare <- 
        compare |> 
        mutate(
                coefficients = 
                        ifelse(
                                test = spray == "A",
                                yes = mean,
                                no = mean - 
                                        (
                                                compare |> 
                                                        filter(spray == "A") |> 
                                                        select(mean) |> 
                                                        unlist()
                                        )
                        )
        )

# Testing for equality of the coefficients
do.call(all.equal,
        Map(as.numeric, list(fit$coefficients, compare$coefficients)))

### Forcing Regression through the Origin -----------------------------------

# The coefficients are the average insect deaths by each category
fit <- 
        lm(
                data = InsectSprays,
                formula = count ~ 0 + spray
        )

sumfit <- summary(fit)

# Testing for equality of the coefficients
Map(as.numeric, list(compare$coefficients, fit$coefficients))
