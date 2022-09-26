# cite the R package:
cite <- function(x) {

  if (x == "reportnormalitytests") {

  text <- c("You can cite this package as following:

Hülemeier, A.-G. (2022).
Automatic report of findings from various normality tests.
Available from https://github.com/huelemeier/report-normality-tests:


BibTeX entry for LaTeX:\n

@Article{Anna-gesina Hülemeier
  title = {Automatic report of findings from various normality tests}
  author = {Anna-Gesina Hülemeier}
  year = {2022}
  url = {https://github.com/huelemeier/report-normality-tests}
}")

  cat(text, sep = "\n") # print output
  }
  end
}
