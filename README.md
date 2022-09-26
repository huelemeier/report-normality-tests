# Report results from normality tests
This repository provides two functions ```report_ols() report_normality()``` to automatically generate the results from the most common normality tests as text.
If you have questions, feedback, or further ideas, feel free to contact me. 

# Installation
The package is not yet published on CRAN. Nevertheless, you can install this package from Github. Type in the following lines of code:
```r
install.packages("remotes")

remotes::install_github("huelemeier/report-normality-tests")
```

Load the package every time you start R:
```r
library(reportnormalitytests)
```


# Functions
The ```report_ols()``` works with: 
```r
ols_normality_test # Shapiro-Wilk normality test, Anderson-Darling normality test, one-sample Kolmogorov-Smirnov test and Cramer-von Mises normality test
```

The ```report_normality()``` is compatible with: 
```r 
lillie.test() # Lilliefors (Kolmogorov-Smirnov) normality test
cvm.test() #Cramer-von Mises normality test
ad.test() #Anderson-Darling normality test
sf.test() # Shapiro-Francia normality test
shapiro.test() # Shapiro-Wilk normality test
pearson.test() # Pearson chi-square normality test
ks.test() # Kolmogorov-Smirnov test (one-sample and two-sample; alternatives: "two-sided", "less", "greater";  exact: TRUE, FALSE)
```


## Examples

```r
# report_ols() function
report_ols(ols_test_normality(iris$Sepal.Length))
We performed a Shapiro-Wilk normality test, and it showed the distribution departed significantly from normality (W = 0.98, p = .010). Based on the two-tailed Asymptotic one-sample Kolmogorov-Smirnov test, we have sufficient evidence to say our data came from normal distribution (D = 0.09, p = .189). We calculated a Cramer-von Mises test of goodness-of-fit test and it showed the distribution of our data departed significantly from the normal distribution (W = 50, p = .000). The Anderson-Darling normality test indicated the distribution of our data departed significantly from normality (W = 0.89, p = .023).



# report_normality() function
report_normality(lillie.test(iris$Sepal.Length))
The Lilliefors (Kolmogorov-Smirnov) normality test suggested iris$Sepal.Length departed significantly from normality (D = 0.09, p = .006).

report_normality(cvm.test(iris$Sepal.Length))
We calculated a Cramer-von Mises normality test, and it showed the distribution of iris$Sepal.Length departed significantly from the normal distribution (W = 0.13, p = .047).

report_normality(ad.test(iris$Sepal.Length))
We performed an Anderson-Darling normality test, and it showed the distribution of iris$Sepal.Length departed significantly from normality (W = 0.89, p = .023).

report_normality(sf.test(iris$Sepal.Length))
We performed a Shapiro-Francia normality test, and it showed the distribution of iris$Sepal.Length departed significantly from normality (W = 0.98, p = .026).

report_normality(shapiro.test(iris$Sepal.Length))
We performed a Shapiro-Wilk normality test, and it showed the distribution of iris$Sepal.Length departed significantly from normality (W = 0.98, p = .010).

report_normality(pearson.test(iris$Sepal.Length))
Based on the Pearson chi-square normality test of goodness of fit, the sample's distribution of iris$Sepal.Length did not match that of the population's (X2(12) = 17.4, p = .135).

report_normality(ks.test(iris$Sepal.Length, 'pnorm', exact = TRUE, alternative = "less"))
Based on the Exact one-sample Kolmogorov-Smirnov test, we have sufficient evidence to say iris$Sepal.Length came from our assumed distribution (D = 1, p = .000). We applied a one-tailed test assuming the cumulative distribution function lies below the null hypothesis.

report_normality(ks.test(iris$Sepal.Length, iris$Sepal.Width))
Based on the Asymptotic two-sample Kolmogorov-Smirnov test, we have sufficient evidence to say iris$Sepal.Length and iris$Sepal.Width did not come from our assumed distribution (D = 0.99, p = <.05). We applied a two-tailed test.
```


