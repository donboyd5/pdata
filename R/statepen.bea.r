#' Bureau of Economic Analysis state summaries.
#'
#' State and Local Government Defined Benefit Pension Plans, Liability and employer normal cost, annual calendar years 2000+
#' Liabilities and normal costs discounted at the following rates:
#'   2000-2003:  6.0%
#'   2004-2009:  5.5%
#'   2010-2014:  5.0%.
#'
#' @source Bureau of Economic Analysis http://www.bea.gov/regional/xls/PensionEstimatesByState.xlsx.
#' @format Data frame with 1 row per state per variable per year.
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{vname}{Variable name}
#' \item{year}{Calendar year, integer}
#' \item{value}{Value; dollar amounts are in $ millions, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(statepen.bea)
"statepen.bea"

