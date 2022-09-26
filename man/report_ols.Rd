\name{report_ols}
\title{Automatic report of findings from ols_test_normality}
\alias{
report_ols
}
\usage{
report_ols(x)
}

\description{
This function automatically creates reports of normality tests from the ols_test_normality() function
}
\arguments{
  \item{x}{R object you want to report.}
}

\examples{
library(olsrr)
report_ols(ols_test_normality(iris$Sepal.Length))
}
