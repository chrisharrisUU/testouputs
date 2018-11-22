# Load dependencies
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(papaja)) {install.packages("papaja"); library(papaja)}
if (!require(magrittr)) {install.packages("magrittr"); library(magrittr)}
if (!require(BayesFactor)) {install.packages("BayesFactor"); library(BayesFactor)}

# Frequentist tests
testoutputs <- function(output, varname = NA, print = TRUE) {
  if ("statistic" %in% names(output)) { # T test
    kind <- "t"
    results <- vector(mode = "character", length = 2)
    if (is.na(varname)) {
      results[1] <- paste("T-test: ", output$data.name, "\n")
    } else {
      results[1] <- paste("T-test: ", varname, "\n")
    }
    dfreedom <- round(output$parameter, 2)
    testvalue <- round(output$statistic, 2)
    pvalue <- output$p.value
    if (testvalue < 0) {
      pvalue <- 1 - pvalue
    }
    if (pvalue < 0.001) {
      psign <- "<"
      pvalue <- 0.001
    } else {
      psign <- "="
      pvalue <- round(pvalue, 3)
    }
    cohensd <- round((2 * output$statistic) / sqrt(output$parameter), 2)
    if (print) {
      results[2] <- paste(kind, "(", dfreedom, ")", " = ", testvalue, ", p ", psign, " ", pvalue, ", d = ", cohensd, sep = "")
    } else {
      kind <- "*t*"
      results <- paste(kind, "(", dfreedom, ")", " = ", testvalue, ", *p* ", psign, " ", pvalue, ", *d* = ", cohensd, sep = "")
    }
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
      results[i + 1] <- paste(attributes(output)$row.names[i], ": ", kind, "(", dfreedom, ", ", output$Df[length(output$Df)], ")", " = ", testvalue, ", p ", psign, pvalue, ", eta^2 = ", eta2, "\n", sep = "")
      if (print) {
        results[i + 1] <- paste(attributes(output)$row.names[i], ": ", kind, "(", dfreedom, ", ", output$Df[length(output$Df)], ")", " = ", testvalue, ", p ", psign, " ", pvalue, ", eta^2 = ", eta2, "\n", sep = "")
      } else {
        kind <- "*F*"
        results[i + 1] <- paste(attributes(output)$row.names[i], ": ", kind, "(", dfreedom, ", ", output$Df[length(output$Df)], ")", " = ", testvalue, ", *p* ", psign, " ", pvalue, ", *eta^2^* = ", eta2, "\n", sep = "")
      }
    }
  } else if ("call" %in% names(output)) { # Regression
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
    if (print) {
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
    } else {
      results[2] <- paste("Overall: Adj_R_squared = ",
                          round(output$adj.r.squared, 2),
                          ", *F*(",
                          output$df[1] - 1,
                          ", ",
                          dfreedom,
                          ") = ",
                          round(output$fstatistic[1], 2),
                          ", *p* ",
                          psign,
                          pvalue,
                          "\n",
                          sep = "")
    }
    for (i in 1 : length(output$aliased)) {
      if (output$coefficients[i, 4] < 0.001) {
        psign <- "< "
        pvalue <- 0.001
      } else {
        psign <- "= "
        pvalue <- round(output$coefficients[i, 4], 2)
      }
      if (print) {
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
      } else {
        results[i + 2] <- paste(dimnames(output$coefficients)[[1]][i],
                                ": b = ",
                                round(output$coefficients[i, 1], 2),
                                ", *t*(",
                                dfreedom,
                                ") = ",
                                round(output$coefficients[i, 3], 2),
                                ", *p* ",
                                psign,
                                pvalue,
                                "\n",
                                sep = "")
      }
    }
  } else { # Exit with "not-supported" message
    results <- "Your statistical test is currently not supported by this function."
  }
  if (print) {
    for (i in 1 : length(results)) {
      cat(results[i])
    }
    cat("\n")
  } else {
    return(results)
  }
}

# BayesFactor T-Test
# Adapted from Tobias Heycke https://osf.io/q5eak/
printBFt <- function(BF, index = 1, postit = 100000, print = FALSE) {
  test_dir <- rownames(BF@bayesFactor)[1] %>%
    substr(., 15, nchar(.))
  b <- as.vector(BF[index])
  if (test_dir == "-Inf<d<0") {
    h <- "-0"
  } else if (test_dir == "0<d<Inf") {
    h <- "+0"
  } else {
    if (as.vector(BF[index]) < 1) {
      b <- 1 / b
      h <- "01"
    } else {
      h <- "10"
    }
  }
  
  s <- " = "
  if (b > 1000000) {
    b <- 1000000
    s <- " > "
  }
  
  if (as.character(class(BF@numerator[[names(BF@numerator)[index]]])) == "BFoneSample") { 
    rBF <- BayesFactor::ttestBF(BF@data[,1],
                                mu = BF@numerator[[names(BF@numerator)[index]]]@prior$mu,
                                rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale)
  }
  
  if (as.character(class(BF@numerator[[names(BF@numerator)[1]]])) == "BFindepSample") { 
    rBF <- BayesFactor::ttestBF(subset(BF@data,
                                       BF@data[,2] == "x")[,1] ,
                                subset(BF@data,
                                       BF@data[,2] == "y")[,1],
                                rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale,
                                paired = FALSE)
  }
  post <- BayesFactor::posterior(rBF, index = index, iterations = postit)
  d <- median(post[, "delta"])
  HDI <- coda::HPDinterval(post[, "delta"])
  ifelse(print,
         paste0('*BF~', h, '~*', s, printnum(b), ', ', '*d* = ', printnum(d), ', ', '*95% HDI* [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'),
         paste0('BF', h, s, printnum(b), ', ', 'd = ', printnum(d), ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'))
}

# Bayesfactor Binominal test
printBFb <- function(BF) {
  as.vector(BF[1])
}