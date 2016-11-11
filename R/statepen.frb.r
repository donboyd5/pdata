#' Federal Reserve Board state summaries.
#'
#' State and Local Government Defined Benefit Pension Plans: State-level Detail, annual calendar years 2000+
#' Liabilities and discounted at the following rates:
#'   2000-2003:  6.0%
#'   2004-2009:  5.5%
#'   2010-2014:  5.0%.
#'
#' @source Federal Reserve Board https://www.federalreserve.gov/apps/fof/efa/efa-project-state-local-government-defined-benefit-pension-plans.htm
#' @format Data frame with 1 row per state per variable per year.
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{vname}{Variable name}
#' \item{year}{Calendar year, integer}
#' \item{value}{Value; dollar amounts are in $ millions, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(statepen.frb)
"statepen.frb"

