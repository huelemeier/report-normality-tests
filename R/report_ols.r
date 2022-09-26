
#' Automatic report of findings from normality tests ols_test_normality
#'
#' This function automatically creates reports of normality tests from
#' - ols_test_normality() # multiple tests (e.g. shaprio-wilk and anderson-darling) are combined in ols-test-normality()
#'
#' @inheritParams report_normality
#' @return A character vector.
#'
#' @seealso [reportnormalitytests::report_normality()]
#'
#' @examples
#' library(olsrr)
#' report_ols(ols_test_normality(iris$Sepal.Length))
#'
#' @import{olsrr}
#'
#' @export
report_ols <- function(x) {

  ### Results from ols_test_normality

  # Shapiro-Wilk Test
  if (x[["shapiro"]][['method']] == "Shapiro-Wilk normality test") {
    p <- x[["shapiro"]][["p.value"]]

    if (x[["shapiro"]][["p.value"]] < 0.05) {
      text_shapiro <- c( # Output for significant values
        "We performed a ", x[["shapiro"]][["method"]], ", and it showed the distribution departed significantly from normality (W = ", round(x[["shapiro"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    } else { # Output for non-significant values
      text_shapiro <- c(
        "We performed a ", x[["shapiro"]][["method"]], ", and it indicated the distribution of did not show evidence of non-normality (W = ", round(x[["shapiro"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }

    end
  }



  if (x[["kolmogorv"]][["method"]] == "Asymptotic one-sample Kolmogorov-Smirnov test") {
    p <- x[["kolmogorv"]][["p.value"]]

    if (x[["kolmogorv"]][["p.value"]] == 0) {
      text_kolmogorov <- c( # Output for significant values
        "Based on the two-tailed ", x[["kolmogorv"]][["method"]], ", we have sufficient evidence to say our data did not come from normal distribution (D = ", round(x[["kolmogorv"]][["statistic"]], 2), ", p = <.05). "
      )
    } else { # Output for non-significant values
      text_kolmogorov <- c(
        "Based on the two-tailed ", x[["kolmogorv"]][["method"]], ", we have sufficient evidence to say our data came from normal distribution (D = ", round(x[["kolmogorv"]][["statistic"]], 2), ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }

    end
  }

  # Cramer-von Mises Test
  if (x[["cramer"]][["data.name"]] == "y") {
    p <- x[["cramer"]][["p.value"]]
    if (x[["cramer"]][["p.value"]] < 0.05) {
      text_cramer <- c( # Output for significant values
        "We calculated a Cramer-von Mises test of goodness-of-fit test and it showed the distribution of our data departed significantly from the normal distribution (W = ", round(x[["cramer"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    } else { # Output for non-significant values
      text_cramer <- c(
        "We calculated a Cramer-von Mises test of goodness-of-fit test and it indicated the distribution of our data did not depart significantly from the normal distribution  (W = ", round(x[["cramer"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), "). "
      )
    }

    end
  }


  # Anderson-Darling Test
  if (x[["anderson"]][[3]] == "Anderson-Darling normality test") {
    p <- x[["anderson"]][["p.value"]]
    if (x[["anderson"]][["p.value"]] < 0.05) {
      text_anderson <- c( # Output for significant values
        "The ", x[["anderson"]][["method"]], " indicated the distribution of our data departed significantly from normality (W = ", round(x[["anderson"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    } else { # Output for non-significant values
      text_anderson <- c(
        "The ", x[["anderson"]][["method"]], " indicated the distribution of our data did not fit normal distribution (W = ", round(x[["anderson"]][["statistic"]], 2),
        ", p = ", sub("^(-?)0.", "\\1.", sprintf("%.3f", p)), ")."
      )
    }

  }




  end

  text <- (paste(c(text_shapiro, text_kolmogorov, text_cramer, text_anderson), collapse = "")) # format output
  cat(text, collapse = "") # print output
}


