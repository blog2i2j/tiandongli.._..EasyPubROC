% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_roc.R
\name{plot_roc}
\alias{plot_roc}
\title{Plot Individual ROC Curves for Multiple Predictors}
\usage{
plot_roc(
  data,
  response_col = NULL,
  themes = ggthemes::theme_clean(),
  titles = NULL,
  predictor = NULL,
  show_ci = FALSE,
  color = "#0072B5",
  color_text = "#0072B5",
  line_size = 0.68,
  title_size = 14,
  digits = 3,
  alpha = 0.15
)
}
\arguments{
\item{data}{A data frame containing the response variable and predictor variables.}

\item{response_col}{The name of the column in \code{data} containing the binary response variable
(e.g., 0/1 or "Case"/"Control"). If \code{NULL}, the user will be prompted to enter the column name.}

\item{themes}{A ggplot2 theme object to customize the appearance of the plot. Default is
\code{ggthemes::theme_clean()}.}

\item{titles}{A named list of custom titles for each predictor. If \code{NULL}, the predictor names
will be used as titles.}

\item{predictor}{A character vector specifying the names of the predictor columns to plot.
If \code{NULL}, all columns except \code{response_col} will be used.}

\item{show_ci}{Logical. If \code{TRUE}, confidence intervals (CI) for the ROC curves will be displayed.
Default is \code{FALSE}.}

\item{color}{The color of the ROC curve. Default is \code{"#0072B5"}.}

\item{color_text}{The color of the text annotations (AUC, CI, and p-value). Default is \code{"#0072B5"}.}

\item{line_size}{The thickness of the ROC curve line. Default is \code{0.66}.}

\item{title_size}{The font size of the plot title. Default is \code{14}.}

\item{digits}{The number of decimal places to round the AUC and p-value. Default is \code{3}.}

\item{alpha}{The transparency level of the confidence interval ribbon. Default is \code{0.15}.}
}
\value{
\itemize{
\item If a single predictor is specified, a \code{ggplot} object representing the ROC curve is returned.
\item If multiple predictors are specified, a list of \code{ggplot} objects is returned, where each element
corresponds to the ROC curve of a predictor.
}
}
\description{
This function generates Receiver Operating Characteristic (ROC) curves for one or more predictors
in a dataset. It is designed to handle wider-format data, where each predictor is stored in a
separate column. The function can plot individual ROC curves for each predictor and optionally
display confidence intervals (CI) for the curves.
}
\details{
The function uses the \code{pROC} package to calculate ROC curves and AUC values. It also supports
adding confidence intervals to the ROC curves using \code{pROC::ci.se()}. The resulting plots are
customized using ggplot2 and can be further modified using the \code{themes} parameter.
}
\examples{
# Load example data
data("example_data_wide")
# 1.Plot ROC curves for multiple predictors without confidence intervals
result <- plot_roc(example_data_wide, response_col = "Group")
patchwork::wrap_plots(result, nrow = 2)
# 2.Plot ROC curves for multiple predictors with confidence intervals
result <- plot_roc(example_data_wide,
   response_col = "Group",
   show_ci = TRUE)
patchwork::wrap_plots(result, nrow = 2)
# 3.Plot ROC curve for the selected predictors
result <- plot_roc(example_data_wide,
   response_col = "Group",
   predictor = c("Predictor1","Predictor2"))
patchwork::wrap_plots(result, nrow = 1)
# 4.Plot ROC curve for the selected predictors using customized theme
result <- plot_roc(example_data_wide,
   response_col = "Group",
   predictor = c("Predictor1","Predictor2"),
   themes = theme_base())
patchwork::wrap_plots(result, nrow = 1)

}
