# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr)

# The Function
testoutputs <- function(output) {
  if ("statistic" %in% names(output)) { # T test
    kind <- "t"
    results <- vector(mode = "character", length = 2)
    results[1] <- paste("T-test: ", output$data.name, "\n")
    dfreedom <- round(output$parameter, 2)
    testvalue <- round(output$statistic, 2)
    pvalue <- output$p.value
    if (output$p.value < 0.001) {
      psign <- "<"
      pvalue <- 0.001
    } else {
      psign <- "="
      pvalue <- round(output$p.value, 3)
    }
    cohensd <- round((2 * output$statistic) / sqrt(output$parameter), 2)
    results[2] <- paste(kind, "(", dfreedom, ")", " = ", testvalue, ", p ", psign, " ", pvalue, ", d = ", cohensd, sep = "")
  } else if ("F value" %in% colnames(output)) { # ANOVA
    kind <- "F"
    results <- vector(mode = "character", length = length(output$Df))
    results[1] <- paste("ANOVA:",
                        attributes(output)$heading[2] %>%
                          sub("Response: ", "", .),
                        "~",
                        paste(attributes(output)$row.names, collapse = " + ") %>%
                          sub(", Residuals", "", .),
                        "\n",
                        sep = " ")
    for (i in 1 : (length(output$Df) - 1)) {
      dfreedom <- round(output$Df[i], 2)
      testvalue <- round(output$`F value`[i], 2)
      if (output$`Pr(>F)`[i] < 0.001) {
        psign <- "< "
        pvalue <- 0.001
      } else {
        psign <- "= "
        pvalue <- round(output$`Pr(>F)`[i], 2)
      }
      eta2 <- round(output$`Sum Sq`[i] / sum(output$`Sum Sq`), 2) # Output is eta squared, not partial
      results[i + 1] <- paste(attributes(output)$row.names[i], ": ", kind, "(", dfreedom, ", ", output$Df[length(output$Df)], ")", " = ", testvalue, ", p ", psign, pvalue, ", p.eta^2 = ", eta2, "\n", sep = "")
    }
  } else if ("call" %in% names(output)) { # Regression
    # To DO
    # Are df correct?
    # transform b coefficients to beta coefficients 
    dfreedom <- output$df[2]
    results <- vector(mode = "character", length = length(output$aliased) + 2)
    results[1] <- paste("Regression:",
                        dimnames(attributes(output$terms)$factors)[[1]][1],
                        "~",
                        paste(dimnames(attributes(output$terms)$factors)[[2]], collapse = " + "),
                        "\n",
                        sep = " ")
    if (output$coefficients[1, 4] < 0.001) {
      psign <- "< "
      pvalue <- 0.001
    } else {
      psign <- "= "
      pvalue <- round(output$coefficients[1, 4], 2)
    }
    results[2] <- paste("Overall: Adj_R_squared = ",
                       round(output$adj.r.squared, 2),
                       ", F(",
                       output$df[1] - 1,
                       ", ",
                       dfreedom,
                       ") = ",
                       round(output$fstatistic[1], 2),
                       ", p ",
                       psign,
                       pvalue,
                       "\n",
                       sep = "")
    for (i in 1 : length(output$aliased)) {
      if (output$coefficients[i, 4] < 0.001) {
        psign <- "< "
        pvalue <- 0.001
      } else {
        psign <- "= "
        pvalue <- round(output$coefficients[i, 4], 2)
      }
      results[i + 2] <- paste(dimnames(output$coefficients)[[1]][i],
            ": b = ",
            round(output$coefficients[i, 1], 2),
            ", t(",
            dfreedom,
            ") = ",
            round(output$coefficients[i, 3], 2),
            ", p ",
            psign,
            pvalue,
            "\n",
            sep = "")
    }
  } else { # Exit with "not-supported" message
    results <- "Your statistical test is currently not supported by this function."
  }
  for (i in 1 : length(results)) {
    cat(results[i]) # cat for line breaks
  }
}
