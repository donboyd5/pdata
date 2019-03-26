#' Bureau of Economic Analysis state summaries.
#'
#' State and Local Government Defined Benefit Pension Plans, Liability and employer normal cost, annual calendar years 2000+
#' Liabilities and normal costs discounted at the following rates:
#'   2000-2003:  6.0%
#'   2004-2009:  5.5%
#'   2010-2012:  5.0%.
#'   2013-2016:  4.0%
#'
#'   Per email from David Lenze, October 22, 2018: With the comprehensive update of the NIPAs released in July 2018,
#'   BEA lowered the NIPA discount rate to 4.0% for 2013 to the present. Previously, it has been 5.0% from 2013-2016.
#'
#' Landing page:    https://www.bea.gov/data/income-saving/personal-income-by-state
#' Current source:  http://apps.bea.gov/regional/xls/PensionEstimatesByState.xlsx
#' Older location:  http://www.bea.gov/regional/xls/PensionEstimatesByState.xlsx.
#'
#' @source Bureau of Economic Analysis http://apps.bea.gov/regional/xls/PensionEstimatesByState.xlsx
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

