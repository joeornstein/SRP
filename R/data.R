#' Marginal Frequency Distribution
#'
#' A dataset containing an example poststratification frame
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{unit}{subnational unit ID}
#'   \item{education}{a categorical variable denoting education level}
#'   \item{freq}{the number of persons}
#'   ...
#' }
#' @source Example dataset by Joe Ornstein.
"education"


#' Marginal Frequency Distribution
#'
#' A dataset containing an example poststratification frame
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{unit}{subnational unit ID}
#'   \item{race}{a categorical variable denoting race}
#'   \item{freq}{the number of persons}
#'   ...
#' }
#' @source Example dataset by Joe Ornstein.
"race"

#' Marginal Frequency Distribution
#'
#' A dataset containing an example poststratification frame
#'
#' @format A data frame with 4 rows and 3 variables:
#' \describe{
#'   \item{unit}{subnational unit ID}
#'   \item{sex}{a categorical variable denoting gender}
#'   \item{freq}{the number of persons}
#'   ...
#' }
#' @source Example dataset by Joe Ornstein.
"sex"

#' Individual-level Public Opinion and Demographics
#'
#' An example individual-level public opinion dataset
#'
#' @format A data frame with 3000 rows and 8 variables:
#' \describe{
#'   \item{ID}{respondent ID}
#'   \item{y}{outcome of interest, continuous}
#'   \item{x1}{predictor variable, continuous}
#'   \item{x2}{predictor variable, continuous}
#'   \item{unit}{subnational unit ID}
#'   \item{latitude}{subnational unit 'latitude' (stylized)}
#'   \item{longitude}{subnational unit 'longitude' (stylized)}
#'   \item{unit_covariate}{unit-level predictor}
#'   ...
#' }
#' @source Ornstein, Joseph T. (2019) "Stacked Regression and Poststratification"
"vignetteData"

#' Poststratification Frame
#'
#' A dataset containing an example poststratification frame
#'
#' @format A data frame with 3200 rows and 7 variables:
#' \describe{
#'   \item{unit}{subnational unit ID}
#'   \item{x1}{predictor variable, continuous}
#'   \item{x2}{predictor variable, continuous}
#'   \item{freq}{number of persons}
#'   \item{unit_covariate}{unit-level predictor}
#'   \item{latitude}{subnational unit 'latitude' (stylized)}
#'   \item{longitude}{subnational unit 'longitude' (stylized)}
#'   ...
#' }
#' @source Ornstein, Joseph T. (2019) "Stacked Regression and Poststratification"
"vignettePSFrame"

#' True Unit-Level Means
#'
#' A dataset containing the true unit-level means for the vignette example
#'
#' @format A data frame with 3200 rows and 7 variables:
#' \describe{
#'   \item{unit}{subnational unit ID}
#'   \item{true_mean}{the true unit-level mean}
#'   ...
#' }
#' @source Ornstein, Joseph T. (2019) "Stacked Regression and Poststratification"
"vignetteTruth"
