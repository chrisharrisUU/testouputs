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
# Intext version for RMarkdown
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %$%
  t.test(Sepal.Length ~ Species) %>%
  testoutputs(print = FALSE)
# Include extra information (e.g. subgroup) for reporting
iris %>%
  filter(Species == "setosa") %$%
  t.test(Sepal.Length, mu = 4.9, alternative = "greater") %>%
  testoutputs(varname = "Setosa")

## ANOVA----

# Standard output for single factor
iris %$%
  lm(Sepal.Length ~ Species) %>%
  anova
# APAish output for single factor
iris %$%
  lm(Sepal.Length ~ Species) %>%
  anova %>%
  testoutputs()

# Standard output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species + Sepal.Width) %>%
  anova()
# APAish output for multiple factors
iris %$%
  lm(Sepal.Length ~ Species + Sepal.Width) %>%
  anova() %>%
  testoutputs()

# Standard output for multiple factors
iris %$%
  lm(Petal.Length ~ Species * Petal.Width) %>%
  anova
# APAish output for multiple factors
iris %$%
  lm(Petal.Length ~ Species * Petal.Width) %>%
  anova() %>%
  testoutputs()

## Regression----

# Standard output for multiple factors
iris %$%
  lm(Petal.Length ~ Species * Petal.Width) %>%
  summary()
# APAish output for multiple factors
iris %$%
  lm(Petal.Length ~ Species * Petal.Width) %>%
  summary() %>%
  testoutputs()
