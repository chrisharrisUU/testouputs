### Provide examples of regular output compared to output by the testoutput-function
source("functions.R")

## T-Test----

# Standard output
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = factor(Species, levels = c("versicolor", "setosa"))) %$%
  t.test(Sepal.Length ~ Species)
# APAish output
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = factor(Species, levels = c("versicolor", "setosa"))) %$%
  t.test(Sepal.Length ~ Species) %>%
  testoutputs
# Intext version for RMarkdown
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = factor(Species, levels = c("versicolor", "setosa"))) %$%
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

## Bayesian T-Test----

# APAish output
ttestBF(
  subset(iris,
         Species == "setosa")$Sepal.Length,
  mu = 4.9,
  nullInterval = c(0, Inf)
) %>% printBFt()

# Intext version for RMarkdown
ttestBF(
  subset(iris,
         Species == "setosa")$Sepal.Length,
  mu = 4.9,
  nullInterval = c(0, Inf)
) %>% printBFt(print = TRUE)

## Bayesian binominal test----
# Distribution
distr <- iris %>%
  filter(Species != "setosa") %>%
  mutate(bin_Spl_lgth = ifelse(Sepal.Length > median(Sepal.Length), 1, 0)) %$%
  table(Species, bin_Spl_lgth)
proportionBF(y = distr["versicolor", "0"],
             N = distr["versicolor", "1"] + distr["versicolor","0"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>%
  printBFb(print = F)
