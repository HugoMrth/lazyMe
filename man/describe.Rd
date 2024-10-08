\title{
  describe
}
\name{describe}
\alias{describe}
\description{
  Descriptive analysis of a dataset. Can be univariate of bivariate.
}\usage{
  describe(# Base arguments
            data,
            vars,
            factor,
            label,
            weights,

            # Confidence interval
            conf.level = 0.95,
            conf.method.cat = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson", "boot"),
            conf.method.num = c("classic", "boot"),

            # Quantitative variables
            prop.type = c("col.percent", "row.percent", "tot.percent"),
            prop.test = c("test", "chi", "fisher"),
            chi.correct = FALSE,
            simulate.p.value = FALSE,

            # Qualitative variables
            num.type = c("mean", "both", "med"),
            mean.test = c("test", "student", "kruskal"),

            # Missing values
            na.omit = TRUE,
            na.str.default = c("missing", "mode", "value"),
            na.str.value = NULL,
            na.num.default = c("none", "mean", "med", "value"),
            na.num.value = NULL,

            # Incidence rate
            pop.ref = NULL,

            # Decimal
            decimal = 1,
            cut.pvalue = 0.001,

            # p-value
            p.decimal = 3,
            p.adjust = FALSE,
            p.adjust.method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),

            # Columns to include
            include.n = TRUE,
            include.tot = TRUE,
            include.conf = TRUE,
            include.minmax = FALSE,
            include.p = TRUE,
            include.test.name = FALSE,

            # Formatting
            lang = c("fr", "en"),
            merge.cols = TRUE,
            big.mark = " ")
}\arguments{
  \item{data}{
    A data-frame containing the data. 
}
  \item{vars}{
Names or indexes of the columns of \code{data} to describe. By default, every columns are included.
}
  \item{factor}{
Name or index of the factor variable. Must be unique. If \code{factor = NULL} (default), only univariate analysis will be computed. 
}
  \item{label}{
Character vector of the names that will substitute the columns names in the output table. Must be the same length as \code{vars}.
By default, columns names are used.
}
  \item{weights}{
Name or index of the weight column for the weighted mean calculation. Must be unique. If \code{weights = NULL} (default), no weighting will be done
}
  \item{conf.level}{
Confidence interval level (default is 0.95). Must be between 0 and 1.
}
  \item{conf.method.cat}{
Method for the proportions confidence interval calculation. 
Default is \code{conf.method = "waldcc"}, other options are \code{"cplus1"}, \code{"goodman"}, \code{"wald"}, \code{"sisonglaz"} and \code{"wilson"}, \code{boot}.

}
  \item{conf.method.num}{
    Method for the numerical confidence interval calculation. 
    Default is \code{conf.method = "classic"}. Other option is \code{"boot"} for a bootstrap estimation.
}
  \item{prop.type}{
Type of the proportions to compile, which correspond to the side of the margins. 
Can either be \code{prop.type = "col.percent"} for a column total of a 100percent, or bottom margin (default), 
\code{prop.type = "col.percent"} for a row total of a 100percent, or right side margin, or \code{prop.type = "tot.percent"} for both
and a grand total of a 100percent.
}
  \item{prop.test}{
Choice of which proportion test to use. Default method is \code{prop.test = "test"} which will first try to run 
a Chi-2 test and go onto a Fisher test if the Chi-2 does not converge. Other options are \code{prop.test = "chi"} and \code{prop.test = "fisher"}
to force either of of the two.
}
  \item{chi.correct}{
Enable the \code{correct} argument of the \code{chisq.test} function. Default is \code{chi.correct = TRUE} as in the original method.
}
  \item{simulate.p.value}{
Enable the \code{simulate.p.value} argument of the \code{fisher.test} function. Default is \code{simulate.p.value = FALSE} as in the original method.
}
  \item{num.type}{
Choice of which set of descriptive indicators to compute for the numerical variable. Can be either \code{quanti = "mean"}, \code{quanti = "med"} or \code{quanti = "both"}.
Standard deviation and mean CI95percent are associated to the mean, IQR and meadian CI95percent are associated to the median.
When choosing \code{quanti = "both"}, two lines are computed in the output datatable for each numerical variable.
}
  \item{mean.test}{
Choice of which mean test to use. Default method is \code{mean.test = "test"} which will first run a Bartlett test for homoscedasticity,
then apply either a Student or Kruskal-Wallis test depending on the outcome. 
Other options are \code{prop.test = "student"} and \code{prop.test = "kruskal"} to force either of of the two.
Whenever the factor variable only includes two groups, the Kruskal-Wallis test is substituted to a Wilcoxson test. 
Whenever there are more than two groups, the Student test in substituted to an Anova test.
}
  \item{na.omit}{
Whether or not missing values should be included in the proportions calculations. Default is \code{na.omit = TRUE} and missing values are ignored.
}
  \item{na.str.default}{
Method to use to indent missing datas in categorical variables. Only relevent when \code{na.omit = FALSE}.
Choices are \code{na.str.default = "missing"} to create a "Missing value" category (default), 
\code{na.str.default = "mode"} to indent the most represented class and 
\code{na.str.default = "value"} to choose which value to indent.
If \code{na.str.default = "value"}, one must then fill up \code{na.str.value} to choose the value.
}
  \item{na.str.value}{
Value to indent to missing categorical variables whenever \code{na.str.default = "value"}.
}
  \item{na.num.default}{
Method to use to indent missing datas in numerical variables. Only relevent when \code{na.omit = FALSE}.
Default is \code{na.num.default = "none"} which will not affect the mean calcultaion but only change the total number of observations to include the missing values.
Other choices are \code{na.num.default = "mean"} or \code{na.num.default = "med"} to indent respectivly the mean or median value of the data, 
and \code{na.num.default = "value"} to choose which value to indent.
If \code{na.num.default = "value"}, one must then fill up \code{na.num.value} to choose the value.
}
  \item{na.num.value}{
Value to indent to missing numerical variable whenever \code{na.num.default = "value"}.
}
  \item{pop.ref}{
Deprecated.
}
  \item{decimal}{
Number of decimals for the descriptive statistics, default is \code{decimal = 1}. Contingency table frequency are not affected.
}
  \item{cut.pvalue}{
Threshold at which to cut the p-values. Default is \code{cut.pvalue = 0.001} and p-values lesser than 0.001 will appear as \code{<0.001} in the output datatable.
}
  \item{p.decimal}{
Number of decimals for the p-values, default is \code{p.decimal = 3}.
}
  \item{p.adjust}{
P-values adjustment for multiple comparisons. Default is \code{p.adjust = FALSE} and no adjustement will be conducted.
See \code{p.ajdust()} for further details.
}
  \item{p.adjust.method}{
Method of p-values adjustment to use. Only applied whenever \code{p.adjust = TRUE}. Default is \code{p.adjust.method = "BH"} for the Benjamini-Hochberg correction, other
options are \code{p.adjust.method = "holm"}, \code{p.adjust.method = "hochberg"}, \code{p.adjust.method = "hommel"}, \code{p.adjust.method = "bonferroni"}, 
\code{p.adjust.method = "BY"}, \code{p.adjust.method = "fdr"} and \code{p.adjust.method = "none"}. See \code{p.ajdust()} for further details.
}
  \item{include.n}{
Include the size of the data sample in the output datatable. Default is \code{include.n = TRUE}.
}
  \item{include.tot}{
Include the univariate analysis along with the bivariate analysis whenever \code{factor} in filled in. Default is \code{include.tot = TRUE}.
}
  \item{include.conf}{
Include the confidence interval in the output datatable. Default is \code{include.conf = TRUE}.
}
  \item{include.minmax}{
Include the minimum and maximum (numerical variables) and number of categories (categorical variables) in the output datatable. Default is \code{include.range = FALSE}.
}
  \item{include.p}{
Include the tests p-values in the output datatable. Default is \code{include.p = TRUE}.
}
  \item{include.test.name}{
Include the names of the statistical tests used in the output datatable. Default is \code{include.test.name = FALSE}.
}
  \item{lang}{
Language used to compute the results. Default is french : \code{langue = "fr"}, can also be english \code{langue = "en"}.
}
  \item{merge.cols}{
Merge the three descriptive columns for each factors into one, or let them split into three. Default is \code{merge.cols = TRUE} and only one column contains all of the indicators.
Output is of the form : \code{mean (sd) [CI95percent]}.
}
  \item{big.mark}{
  Character; if not \code{NULL} used as mark between every big interval decimals before the decimal point.
}
}
\details{
Tests and p-value adjustment are computed using existing functions from base R (see the seealso section for further details). 
Confidence interval calculations however are directly implemented in this function and do not refer to any existing R functions.  \cr

The algorithm used to choose which tests to conduct are based on my own practice, 
that is choosing between Fisher and Chi-2 tests for a difference in proportions (based on frequencies in the contingency table)
and choosing between Student and Wilcoxson/Kruskal-Wallis tests for a difference in mean (based on the outcome of the Bartlett test for heteroscedasticity). \cr

Unrepresented factor levels can be taken into account in the analysis (line being displayed even tho N=0), 
but the variable has to be a factor with the levels being assigned accordingly. This will not work with character vectors.
}
\value{
}
\author{
  Hugo Marthinet
}
\seealso{
  \code{\link{chisq.test}}, \code{\link{fisher.test}}, \code{\link{kruskal.test}}, \code{\link{wilcox.test}}, \code{\link{t.test}}, \code{\link{anova}},
\code{\link{p.adjust}}
}
\examples{
  # Full univariate descriptive analysis
  descriptif(data = iris,
           num.type = "both",
           decimal = 2,
           include.minmax = TRUE,
           merge.cols = FALSE)

  # Minimalist bivariate analysis
  descriptif(data = tab::tabdata,
           factor = "Group",
           vars = c("Age", "Sex", "Race", "BMI"))

  # Focus on changes in the statistical testing
  descriptif(data = tab::tabdata,
           factor = "Group",
           vars = c("Age", "Sex", "Race", "BMI"),
           label = c("Age", "Sexe", "Race", "IMC"),
           include.test.name = TRUE, include.tot = FALSE, include.n = FALSE,
           cut.pvalue = 0.0001,
           p.adjust = TRUE, p.adjust.method = "bonferroni")
}
