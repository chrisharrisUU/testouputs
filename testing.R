### Testing----
source("function.R")

# T-Test
output <- iris %>%
  filter(Species %in% c("setosa", "versicolor")) %$%
  t.test(Sepal.Length ~ Species)

# ANOVA
output <- iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  anova

# Regression
output <- iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  summary
