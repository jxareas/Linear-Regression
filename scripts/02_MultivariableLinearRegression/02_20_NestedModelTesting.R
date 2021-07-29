# Data ----------------------------------------------------------------

data(swiss)


# Nested Models -----------------------------------------------------------

fit1 <-
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture
        )

fit2 <-
        lm(
                data = swiss,
                formula = Fertility ~ Agriculture + Examination + Education
        )

fit3 <- 
        lm(
                data = swiss,
                formula = Fertility ~
                        Agriculture + Examination + Education +
                        Catholic + Infant.Mortality
        )


# Analysis of Variance ----------------------------------------------------

fitanova <-
        Map(anova, list(fit1, fit2, fit3))

comparison <-
        anova(fit1, fit2, fit3)
