\name{report_normality}
\alias{
report_normality
}
\title{Automatic report of findings from various normality tests}
\usage{
report_normality(x)
}

\description{
This function automatically creates reports of different normality tests. These tests are: shapiro.test, ad.tests, cvm.test, lillie.test, pearson.test, sf.test and ks.test.
}
\arguments{
  \item{x}{R object you want to report.}
}

\examples{
library(nortest)
library(stats)
report_normality(lillie.test(iris$Sepal.Length))
report_normality(cvm.test(iris$Sepal.Length))
report_normality(ad.test(iris$Sepal.Length))
report_normality(sf.test(iris$Sepal.Length))
report_normality(shapiro.test(iris$Sepal.Length))
report_normality(pearson.test(iris$Sepal.Length))
report_normality(ks.test(iris$Sepal.Length, 'pnorm', exact = TRUE, alternative = "less"))
report_normality(ks.test(iris$Sepal.Length, iris$Sepal.Width))
}
