### Provide examples of regular output compared to output by the testoutput-function
source("function.R")

## T-Test----

# Standard output
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %$%
  t.test(Sepal.Length ~ Species)
# APAish output
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %$%
  t.test(Sepal.Length ~ Species) %>%
  testoutputs

## ANOVA----

# Standard output for single factor
iris %$%
  lm(Sepal.Length ~ Species) %>%
  anova
# APAish output for single factor
iris %$%
  lm(Sepal.Length ~ Species) %>%
  anova %>%
  testoutputs

# Standard output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species + Petal.Width) %>%
  anova
# APAish output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species + Petal.Width) %>%
  anova %>%
  testoutputs

# Standard output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  anova
# APAish output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  anova %>%
  testoutputs

## Regression----

# Standard output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  summary
# APAish output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species * Petal.Width) %>%
  summary %>%
  testoutputs
