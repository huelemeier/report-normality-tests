
#' Automatic report of findings from various normality tests
#'
#' This function automatically creates reports of different normality tests, such as
#' - shapiro.test() Shapiro-Wilk normality test
#' - ad.tests() Anderson-Darling normality test
#' - cvm.test() Cramer-von Mises normality test
#' - lillie.test() # Lilliefors (Kolmogorov-Smirnov) normality test
#' - pearson.test() # Pearson chi-square normality test
#' - sf.test() # Shapiro-Francia normality test
#' - ks.test() # Kolmogorov-Smirnov test (one-sample and two-sample; alternatives: "two-sided", "less", "greater";  exact: TRUE, FALSE)
#'
#' @inheritParams report_normality
#' @return A character vector.
#'
#' @seealso [reportnormalitytests::report_ols()]
#' @import{nortest}
#'
#' library(nortest)
#' library(stats)
#' report_normality(lillie.test(iris$Sepal.Length))
#' report_normality(cvm.test(iris$Sepal.Length))
#' report_normality(ad.test(iris$Sepal.Length))
#' report_normality(sf.test(iris$Sepal.Length))
#' report_normality(shapiro.test(iris$Sepal.Length))
#' report_normality(pearson.test(iris$Sepal.Length))
#' report_normality(ks.test(iris$Sepal.Length, 'pnorm', exact = TRUE, alternative = "less"))
#' report_normality(ks.test(iris$Sepal.Length, iris$Sepal.Width))
#'
#' @export
report_normality <- function(x) {
  p <- x[["p.value"]] # get p-value so that it can be styled in apa-format

  # Anderson-Darling Test
  if (x[["method"]][[1]] == "Anderson-Darling normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "We performed an ", x[["method"]], ", and it showed the distribution of ", x[["data.name"]], " departed significantly from normality (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "We performed an ", x[["method"]], ", and it indicated the distribution of ", x[["data.name"]], " did not fit normal distribution (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }

    end
  }
  # Shapiro-Wilk Test
  else if (x[["method"]][[1]] == "Shapiro-Wilk normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "We performed a ", x[["method"]], ", and it showed the distribution of ", x[["data.name"]], " departed significantly from normality (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "We performed a ", x[["method"]], ", and it indicated the distribution of ", x[["data.name"]], " did not show evidence of non-normality (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }
    end
  }

  # Shapiro-Francia Test
  else if (x[["method"]][[1]] == "Shapiro-Francia normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "We performed a ", x[["method"]], ", and it showed the distribution of ", x[["data.name"]], " departed significantly from normality (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "We performed a ", x[["method"]], ", and it indicated the distribution of ", x[["data.name"]], " did not show evidence of non-normality (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }
    end
  }

  # Cramer-von Mises Test
  else if (x[["method"]][[1]] == "Cramer-von Mises normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "We calculated a ", x[["method"]], ", and it showed the distribution of ", x[["data.name"]], " departed significantly from the normal distribution (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "We calculated a ", x[["method"]], ", and it indicated the distribution of ", x[["data.name"]], " did not depart significantly from the normal distribution  (W = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }
    end
  }

  # Lilliefors Test
  else if (x[["method"]][[1]] == "Lilliefors (Kolmogorov-Smirnov) normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "The ", x[["method"]], " suggested ", x[["data.name"]], " departed significantly from normality (D = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "The ", x[["method"]], " suggested the distribution of ", x[["data.name"]], " did not show evidence of non-normality (D = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }
    end
  }

  # Pearson Chi-square test
  else if (x[["method"]][[1]] == "Pearson chi-square normality test") {
    if (x[["p.value"]] < 0.05) {
      text <- c( # Output for significant values
        "Based on the ", x[["method"]], " of goodness of fit, the sample's distribution of ", x[["data.name"]], " matched that of the population's (X2(", x[["df"]], ") = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text <- c(
        "Based on the ", x[["method"]], " of goodness of fit, the sample's distribution of ", x[["data.name"]], " did not match that of the population's (X2(", x[["df"]], ") = ", round(x[["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }
    end
  }

   # Kolmogorof-Smirnov test
  else if (x[["method"]][[1]] == "Asymptotic one-sample Kolmogorov-Smirnov test") {
    if (x[["alternative"]] == "two-sided") {
      p <- p
    }
    if (p  == 0) {
      text <- c( # Output for significant values
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " did not come from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = <.05). "
      )
    } else { # Output for non-significant values
      text <- c(
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " came from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }
    if (x[["alternative"]] == "two-sided") {
      alternative <- c("We applied a two-tailed test.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies above the null hypothesis") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies above the null hypothesis.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies below the null hypothesis") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies below the null hypothesis.")
      text <- c(text, alternative)
    }
    end
  } else if (x[["method"]][[1]] == "Exact one-sample Kolmogorov-Smirnov test") {
    if (x[["alternative"]] == "two-sided") {
      p <- p
    }
    if (p < 0) {
      text <- c( # Output for significant values
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " did not come from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    } else { # Output for non-significant values
      text <- c(
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " came from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }

    if (x[["alternative"]] == "two-sided") {
      alternative <- c("We applied a two-tailed test.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies above the null hypothesis") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies above the null hypothesis.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies below the null hypothesis") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies below the null hypothesis.")
      text <- c(text, alternative)
    }

    end
  } else if (x[["method"]][[1]] == "Asymptotic two-sample Kolmogorov-Smirnov test") {
    if (x[["alternative"]] == "two-sided") {
      p <- p
    }
    if (p == 0) {
      text <- c( # Output for significant values
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " did not come from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = <.05). "
      )
    } else { # Output for non-significant values
      text <- c(
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " came from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }
    if (x[["alternative"]] == "two-sided") {
      alternative <- c("We applied a two-tailed test.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies below that of y") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies below.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies above that of y") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies above.")
      text <- c(text, alternative)
    }

    end
  } else if (x[["method"]][[1]] == "Exact two-sample Kolmogorov-Smirnov test") {
    if (x[["alternative"]] == "two-sided") {
      p <- p
    }
    if (p < 0) {
      text <- c( # Output for significant values
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " did not come from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    } else { # Output for non-significant values
      text <- c(
        "Based on the ", x[["method"]], ", we have sufficient evidence to say ", x[["data.name"]], " came from our assumed distribution (D = ", round(x[["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }

    if (x[["alternative"]] == "two-sided") {
      alternative <- c("We applied a two-tailed test.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies above that of y") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies above.")
      text <- c(text, alternative)
    } else if (x[["alternative"]] == "the CDF of x lies below that of y") {
      alternative <- c("We applied a one-tailed test assuming the cumulative distribution function lies below.")
      text <- c(text, alternative)
    }


    end
  }



  end

  text <- (paste(text, collapse = "")) # format output
  cat(text, collapse = "") # print output
}




